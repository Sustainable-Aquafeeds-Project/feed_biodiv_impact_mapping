#### Takes ~20ish? hours to run all scenarios and allocations... sorry.
## This script accomplishes the same thing as 06b, but only for the finfish taxon. The finfish taxon was too large to process in one go, so we needed to split it so that our server could handle it. 

# In this script we overlap [Aquamaps probability of suitable habitat maps](https://www.aquamaps.org/) with our disturbance pressure maps created in the `02_feed` folder, and multiply by their vulnerability value. The goal of this script is to create impact maps, that is, the area of likely suitable habitat (>0.6 probability) for each species that is exposed AND impacted to harvest of forage or trimmings fish that is ultimately processed into FMFO. To do this, we: 
#   
# - We have created maps of disturbance (km2) of harvest of forage and trimmings fish species that end up as FMFO. They have these categories: 
# - Ingredient type; Fish oil or fish meal
# - Allocation type; mass, energetic, or economic
# - Diet type; feed formulation; plant-dominant or fish-dominant
# - Fish type; forage or trimmings species 
# - Depth zone; reef, pelagic, bentho-pelagic, or benthic 
# 
# 
# - Overlap re-projected and clipped Aquamaps species suitable native habitat maps for FINFISH TAXON with the appropriate disturbance rasters based on depth range. This will provide a km2 estimate of the amount of suitable habitat that is exposed to harvest of forage or trimmings fish used for FMFO. 
# - multiply by each species vulnerability values to get impact and save

# * Froese, R. and D. Pauly, Editors. 2000. FishBase 2000: concepts, design and data sources. ICLARM, Los Baños, Laguna, Philippines. 344 p.
# * Houde, E.D. and C.E. Zastrow. 1993. Ecosystem- and taxon-specific dynamic energetics properties of fish larvae assemblages. Bull. Mar. Sci. 53(2):290-335.
# * Sa-a, P., M.L. Palomares and D. Pauly. 2000. The FOOD ITEMS table, p. 182-188. In R. Froese and D. Pauly (eds.) FishBase 2000: concepts, design and data sources. ICLARM, Los Baños, Laguna, Philippines. 344 p.
# * Welcomme, R.L. 1988. International introductions of inland aquatic species. FAO Fish. Tech. Pap. 294, 318 p.
# * Butt, N. et al. 2022.
# * Kaschner, K., Kesner-Reyes, K., Garilao, C., Segschneider, J., Rius-Barile, J. Rees, T., & Froese, R. (2019, October). AquaMaps: Predicted range maps for aquatic species. Retrieved from https://www.aquamaps.org.
# * O'Hara et al. 2023 in review 
#   * Casey C. O’Hara et al., At-risk marine biodiversity faces extensive, expanding, and intensifying human impacts.Science372,84-87(2021).DOI:10.1126/science.abe6731
# 
# FishBase is a scientific database, and this has the implication - among others - that its use and the use of its contents are free as long as due credit is given.
# 
# This may be done at different levels, for which we suggest different forms of citations:
# 
# * when referring to FishBase concepts and design, cite its architects (Froese and Pauly 2000);
# * when referring to a set of values extracted from a FishBase table, cite the author(s) of the original data, e.g., "Houde and Zastrow (1993)", or "Welcomme (1988)". To help us track the use of FishBase in the literature, we would appreciate your also citing Froese and Pauly (2000) in an appropriate part of your text, as the source of the information;
# * when discussing the features of a FishBase table, cite the section documenting that table, e.g., "Sa-a et al. (2000)."
# 

library(tidyverse)
library(tidyr)
library(here)
library(sf)
library(data.table)
library(dtplyr)
library(terra)
library(parallel)
library(strex)
library(janitor)
library(glue)
library(qs)

select <- dplyr::select
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # set working directory to where this script is located
this_dir <- getwd()

options(dplyr.summarise.inform = FALSE)
source(here("src/directories.R"))
source(here("src/spatial.R"))
source(here("src/fxns.R"))

aquamaps_dir <- file.path(rdsi_raw_data_dir, "aquamaps")
biodiv_dir <- file.path(rdsi_dir, "biodiversity_impacts")
fish_dir <- file.path(biodiv_dir, "int/fish_taxon")
feed_rast_dir <- file.path(biodiv_dir, "int/resampled_ingredient_rasts")


gall_peters <- "+proj=cea +lon_0=0 +x_0=0 +y_0=0 +lat_ts=45 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

moll_ocean_template <- readRDS(file.path(this_dir, "data/spatial/moll_template_ocean_xy.rds"))

## Setup spp map source and vulnerability data 
spp_fp <- data.frame(filepath = list.files(file.path(aquamaps_dir, "reprojected_mol_csv"), full.names = TRUE)) %>%
  mutate(species = str_remove_all(str_replace_all(str_after_last(filepath, "\\/"), "_", " "), ".csv"))

am_info <- read.csv(file.path(aquamaps_dir, "GTE10_SPECIESOCCURSUM/speciesoccursum_gte10.csv"))

spp_info_df <- readRDS(file.path(biodiv_dir, "int/spp_vuln_depth_info.rds")) %>%
  filter(spp_type != "Microorganisms") %>%
  left_join(spp_fp) %>%
  mutate(wcol = case_when(
    depth_position == "pelagic" ~ 'pel', 
    depth_position == "benthic" ~ "ben",
    TRUE ~ "both"
  )) %>%
  mutate(ingredient = ifelse(ingredient == "fish meal" & fish_type == "trimmings fish", "fish meal, cut offs", ingredient)) %>%
  mutate(ingredient = ifelse(ingredient == "fish oil" & fish_type == "trimmings fish", "fish oil, cut offs", ingredient)) %>%
  filter(taxon == "fish")


## for fish species, need to split in half, and then take species weighted average. My server is memory limited and can't handle all 9k at once...  

length(unique(spp_info_df$species))/2

fis_spp_distinct <- spp_info_df %>%
  distinct(species) %>%
  mutate(row_id = row_number()) %>%
  mutate(taxon = ifelse(row_id %in% c(1:4617), "fish1", "fish2")) %>%
  dplyr::select(-row_id)

spp_info_df_fish <- fis_spp_distinct %>%
  left_join(spp_info_df %>% dplyr::select(-taxon))


spp_info_df_fish %>% 
  group_by(taxon) %>%
  summarize(nspp = n_distinct(species))

# # A tibble: 2 × 2
# taxon  nspp
# <chr> <int>
#   1 fish1  4617
# 2 fish2  4618

## perfect

allocations <- unique(spp_info_df_fish$allocation)
allocations <- c("economic")
diets <- unique(spp_info_df_fish$diet)
ingredients <- unique(spp_info_df_fish$ingredient)
fish_types <- unique(spp_info_df_fish$fish_type)
spp_types <- unique(spp_info_df_fish$taxon)
fcrs <- c("regular")
sensitivity_types <- c("plus-10", "minus-10")

for(tx_type in spp_types){

  ## read in taxon maps
  # tx_type = "fish2"
  tx_maps_df <- spp_info_df_fish %>%
    filter(taxon == tx_type) %>%
    dplyr::select(species, filepath) %>%
    distinct()

  ### read in all spp maps for this taxon -
  message('Loading spp maps for taxon ', tx_type, '...')
  tx_maps <- collect_spp_rangemaps_marine(tx_maps_df$species, tx_maps_df$filepath)

  message('Taxon ', tx_type, ' spp dataframe: ', nrow(tx_maps[["parent"]]),
          ' cell observations for ', nrow(tx_maps_df), ' species...')

  for(allocation_type in allocations){
   for(diet_type in diets){
     for(fcr in fcrs){
    for(ingredient_type in ingredients){
      for(fs_type in fish_types){
        for(sensitivity_type in sensitivity_types){
          

          # allocation_type = "economic"
          # diet_type = "plant-dominant"
          # ingredient_type = "fish oil, cut offs"
          # fs_type = "trimmings fish"
          # sensitivity_type = "original"
          # fcr = "regular"

          if(fs_type == "trimmings fish" & ingredient_type == "fish meal" |fs_type == "trimmings fish" & ingredient_type == "fish oil" |
             fs_type == "forage fish" & ingredient_type == "fish meal, cut offs"| fs_type == "forage fish" & ingredient_type == "fish oil, cut offs") {
            message('Not a category! ... skipping!')
            next()
          }

          if(file.exists(glue(file.path(biodiv_dir, "output/impact_maps_by_spp_ingredient_lists/{tx_type}_{diet_type}_{fcr}_{sensitivity_type}_{fs_type}_{ingredient_type}_{allocation_type}.qs")))) {
            message('Rasters exist for taxon ', tx_type, sensitivity_type, ' for pressure... skipping!')
            next()
          }

          tx_vuln_df <- spp_info_df_fish %>%
            filter(taxon == tx_type,
                   allocation == allocation_type,
                   diet == diet_type,
                   fish_type == fs_type,
                   ingredient == ingredient_type)

          # grab appropriate vuln_df column 
          if(sensitivity_type == "original"){
            tx_vuln_df <- tx_vuln_df %>%
              dplyr::select(-vuln_quartile_plus, -vuln_quartile_minus)
            
          }else if(sensitivity_type == "plus-10"){
            tx_vuln_df <- tx_vuln_df %>%
              dplyr::select(allocation, diet, ingredient, fish_type, SpeciesID, species, spp_type, taxon, depth_position, DepthPrefMax, trim_spp, for_spp, 
                            catch_type, filepath, wcol, vuln_quartile = vuln_quartile_plus)
          }else {
            tx_vuln_df <- tx_vuln_df %>%
              dplyr::select(allocation, diet, ingredient, fish_type, SpeciesID, species, spp_type, taxon, depth_position, DepthPrefMax, trim_spp, for_spp, 
                            catch_type, filepath, wcol, vuln_quartile = vuln_quartile_minus)
          }    
          

          ## Read in bycatch and catch stressor maps, and create a dataframe of the results.  Assign species to one of three bins based on water column position trait.  Benthopelagic and reef associated spp will take an average of the two bycatch stressor maps.
          benth_catch_rast <- rast(sprintf(file.path(feed_rast_dir, "%s/%s/%s_%s_%s_benthic_catch_A.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type))
          pelag_catch_rast <- rast(sprintf(file.path(feed_rast_dir, "%s/%s/%s_%s_%s_pelagic_catch_A.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type))
          bp_catch_rast <- (benth_catch_rast + pelag_catch_rast)/2

          benth_bycatch_rast <- rast(sprintf(file.path(feed_rast_dir, "%s/%s/%s_%s_%s_benthic_bycatch_A.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type))
          pelag_bycatch_rast <- rast(sprintf(file.path(feed_rast_dir, "%s/%s/%s_%s_%s_pelagic_bycatch_A.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type))
          bp_bycatch_rast <- (benth_bycatch_rast + pelag_bycatch_rast)/2

          catch_cells_df <- data.frame(ben = as.vector(values(benth_catch_rast)),
                                         pel = as.vector(values(pelag_catch_rast)),
                                         both    = as.vector(values(bp_catch_rast)),
                                         cell_id = 1:ncell(benth_catch_rast)) %>%
            filter(!is.na(ben) | !is.na(pel)) %>%
            pivot_longer(names_to = 'wcol', values_to = 'catch', cols = -cell_id)


          bycatch_cells_df <- data.frame(ben = as.vector(values(benth_bycatch_rast)),
                                         pel = as.vector(values(pelag_bycatch_rast)),
                                         both    = as.vector(values(bp_bycatch_rast)),
                                         cell_id = 1:ncell(benth_bycatch_rast)) %>%
            filter(!is.na(ben) | !is.na(pel)) %>%
            pivot_longer(names_to = 'wcol', values_to = 'bycatch', cols = -cell_id)


          ## Calculate mean impacts per species grouping
          # For each species in the taxon, multiply bycatch or catch stressor map by the spp vulnerability to identify impact map for that species. Summarize across the entire taxon to mean, sd, and nspp.


          message('Processing mean/sd vulnerability by species in taxon ', tx_type, allocation_type, fs_type, ingredient_type, diet_type, sensitivity_type,
                  ' to bycatch/catch stressor...')

          ### because failures might occur with summarizing a huge dataset,
          ### let's break this into chunks by cell_id - there are 6.6e+06 cells total
          ### but no ocean cells past 6.5e6

          chunk_size <- 500000
          n_chunks <- ceiling(6.5e6 / chunk_size)
          n_cores <- max(1, floor(n_chunks / ceiling(nrow(tx_maps[["parent"]])/3e7)))

          result_list <- parallel::mclapply(1:n_chunks, mc.cores = n_cores,
                                            FUN = function(n) {


                                              ### n <- 6
                                              cell_id_min <- as.integer((n - 1) * chunk_size + 1)
                                              cell_id_max <- as.integer(n * chunk_size)
                                              message('Summarizing harvest stressor on taxon ', tx_type,
                                                      ': cells ', cell_id_min, ' - ', cell_id_max, '...')

                                              chunk_sum_spp <- tx_maps %>%
                                                filter(between(cell_id, cell_id_min, cell_id_max)) %>%
                                                left_join(tx_vuln_df, by = c('species')) %>%
                                                left_join(bycatch_cells_df,
                                                          by = c('cell_id', 'wcol')) %>%
                                                left_join(catch_cells_df,
                                                          by = c('cell_id', 'wcol')) %>%
                                                as.data.table() %>%
                                                .[ , bycatch := ifelse(is.na(bycatch), 0, bycatch)] %>%
                                                .[ , catch := ifelse(is.na(catch), 0, catch)] %>%
                                                .[ , impact_km2  := ifelse(catch_type == "bycatch", vuln_quartile * bycatch, vuln_quartile*catch)] %>%
                                                .[ , hab_area := 100] 
                                              
                              
                                              
                                              chunk_sum_spp_hab <- chunk_sum_spp %>%
                                                .[, .(species, cell_id, hab_area)] %>%
                                                .[, .SD[!duplicated(.SD, by = c('species', 'cell_id', 'hab_area'))]] %>%
                                                .[ , .(hab_area = sum(hab_area)),
                                                   by = c('species')]
                                              
                                              chunk_sum_spp_global <- chunk_sum_spp %>%
                                                .[ , .(impact_total = sum(impact_km2)),
                                                   by = c('species')] %>%
                                                merge(., chunk_sum_spp_hab, by = "species", all = TRUE)
                                              
                                              chunk_sum_spp <- chunk_sum_spp %>%
                                                .[impact_km2>0]

                                              return(list(chunk_sum_spp = chunk_sum_spp, chunk_sum_spp_global = chunk_sum_spp_global))
                                            })



          if(check_tryerror(result_list)) {
            stop('Something went wrong with calculations for taxon ', tx_type, '!')
          }


          message(glue('Creating and saving global df for taxon {tx_type} {allocation_type} {ingredient_type} {fs_type} {diet_type} {fcr} {sensitivity_type}'))

          global_df <- rbindlist(lapply(result_list, function(x) x$chunk_sum_spp_global)) %>%
            as.data.frame() %>%
            group_by(species) %>%
            summarise(impact_total = sum(impact_total, na.rm = TRUE),
                      hab_area = sum(hab_area, na.rm = TRUE)) %>%
            mutate(allocation = allocation_type, diet = diet_type, fcr_type = fcr, ingredient = glue("{fs_type}_{ingredient_type}"),
                   sensitivity_scenario = sens_type)

          fish_ingredient_type = unique(global_df$ingredient)

          write_rds(global_df, glue(file.path(biodiv_dir, "int/aoh_impacts_marine/{tx_type}_{diet_type}_{fcr}_{sensitivity_type}_{fish_ingredient_type}_{allocation_type}.rds")))

          result_list %>%
            lapply(function(x) {
              if ("chunk_sum_spp" %in% names(x)) {
                return(x$chunk_sum_spp)
              } else {
                return(NA)  # Return NA or an empty dataframe if 'chunk_sum_spp' is not found
              }
            }) %>%
            qsave(file = glue(file.path(biodiv_dir, "output/impact_maps_by_spp_ingredient_lists/{tx_type}_{diet_type}_{fcr}_{sensitivity_type}_{fish_ingredient_type}_{allocation_type}.qs")))

          rm(result_list)
          rm(global_df)
          
          
        }
      }
      }
    }
   }
  }
}
