### Takes ~6 hours to run
# In this script we overlap [Aquamaps probability of suitable habitat maps](https://www.aquamaps.org/) with our disturbance pressure maps created in the `02_feed` folder, and multiply by their vulnerability value. The goal of this script is to create impact maps, that is, the area of likely suitable habitat (>0.6 probability) for each species that is exposured AND impacted to harvest of forage or trimmings fish that is ultimately processed into FMFO. To do this, we: 
#   
#   - We have created maps of disturbance (km2) of harvest of forage and trimmings fish species that end up as FMFO. They have these categories: 
#   - Ingredient type; Fish oil or fish meal
# - Allocation type; mass, energetic, or economic
# - Diet type; feed formulation; plant-dominant or fish-dominant
# - Fish type; forage or trimmings species 
# - Depth zone; reef, pelagic, bentho-pelagic, or benthic 
# 
# 
# - Overlap re-projected and clipped Aquamaps species suitable habitat maps with the appropriate disturbance rasters based on depth range. This will provide a km2 estimate of the amount of suitable habitat that is exposed to harvest of forage or trimmings fish used for FMFO. 
# - multiply by each species vulnerability values to get impact and save


# * Froese, R. and D. Pauly, Editors. 2000. FishBase 2000: concepts, design and data sources. ICLARM, Los Baños, Laguna, Philippines. 344 p.
# * Houde, E.D. and C.E. Zastrow. 1993. Ecosystem- and taxon-specific dynamic energetics properties of fish larvae assemblages. Bull. Mar. Sci. 53(2):290-335.
# * Sa-a, P., M.L. Palomares and D. Pauly. 2000. The FOOD ITEMS table, p. 182-188. In R. Froese and D. Pauly (eds.) FishBase 2000: concepts, design and data sources. ICLARM, Los Baños, Laguna, Philippines. 344 p.
# * Welcomme, R.L. 1988. International introductions of inland aquatic species. FAO Fish. Tech. Pap. 294, 318 p.
# * Butt, N. et al. 2022.
# * Kaschner, K., Kesner-Reyes, K., Garilao, C., Segschneider, J., Rius-Barile, J. Rees, T., & Froese, R. (2019, October). AquaMaps: Predicted range maps for aquatic species. Retrieved from https://www.aquamaps.org.
# * O'Hara et al. 2023 in prep 
# * Casey C. O’Hara et al., At-risk marine biodiversity faces extensive, expanding, and intensifying human impacts.Science372,84-87(2021).DOI:10.1126/science.abe6731
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
library(readxl)
library(rfishbase)
library(glue)

select <- dplyr::select
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # set working directory to where this script is located
this_dir <- getwd()
# feed_rast_dir <- here("prep/02_feed/output/resampled")

options(dplyr.summarise.inform = FALSE)
source(here("src/directories.R"))
source(here("src/spatial.R"))
source(here("src/fxns.R"))

aquamaps_dir <- file.path(rdsi_raw_data_dir, "aquamaps")
biodiv_dir <- file.path(rdsi_dir, "biodiversity_impacts")
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
  mutate(ingredient = ifelse(ingredient == "fish oil" & fish_type == "trimmings fish", "fish oil, cut offs", ingredient))


spp_info_df %>% 
  group_by(taxon) %>%
  summarize(nspp = n_distinct(species))

# # A tibble: 13 × 2
# taxon                    nspp
# <chr>                   <int>
# 1 Bird                        9
# 2 Marine mammal             118
# 3 Marine plant              280
# 4 Reptiles and amphibians    37
# 5 arthropods               3886
# 6 cephalopods               300
# 7 corals                   1480
# 8 echinoderms              1021
# 9 elasmobranchs             695
# 10 fish                     9235
# 11 molluscs                 5515
# 12 polychaetes               608
# 13 sponges                   455

## only 9 marine birds?? 


## will save a total of X rasters: 
# 3 allocations * 2 diets * 2 ingredients * 2 fish ingredient spp (forage vs trimmings) = 24 scenarios
# 24 scenarios * 13 spp type * 3 raster types (mean, sd, nspp) = 936 total or 468 per diet

## for fish species, need to split in half, and then take species weighted average. My server is memory limited and can't handle all 9k at once...  

allocations <- unique(spp_info_df$allocation)
diets <- unique(spp_info_df$diet)
ingredients <- unique(spp_info_df$ingredient)
fish_types <- unique(spp_info_df$fish_type)
fcrs <- c("regular", "efficient")
spp_types <- unique(spp_info_df$taxon)
# indices_to_remove <- grep("Bird|Marine mammal|Marine plant|Reptiles and amphibians|arthropods|echinoderms|polychaetes|sponges", spp_types)
indices_to_remove <- grep("fish|polychaetes", spp_types)
spp_types <- spp_types[-indices_to_remove] # remove fish category... we handle this separately in the next script `06c_overlap_fish_fix.R`

for(fs_type in fish_types){
  for(ingredient_type in ingredients){
    
    if(fs_type == "trimmings fish" & ingredient_type == "fish meal" |fs_type == "trimmings fish" & ingredient_type == "fish oil" |
       fs_type == "forage fish" & ingredient_type == "fish meal, cut offs"| fs_type == "forage fish" & ingredient_type == "fish oil, cut offs") {
      message('Not a category! ... skipping!')
      next()
    }
    
    for(tx_type in spp_types){
      for(fcr in fcrs){
        for(allocation_type in allocations){
          for(diet_type in diets){
            
# 
#             allocation_type = "economic"
#             diet_type = "plant-dominant"
#             ingredient_type = "fish meal"
#             fs_type = "forage fish"
#             tx_type = "cephalopods"
#              fcr = "regular"
            
            outf_mean <- sprintf(file.path(biodiv_dir, "output/impact_maps_by_taxon_ingredient/%s/%s/imp_unwt_%s_%s_%s_%s_mean.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type, tx_type)
            outf_sd <- sprintf(file.path(biodiv_dir, "output/impact_maps_by_taxon_ingredient/%s/%s/imp_unwt_%s_%s_%s_%s_sd.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type, tx_type)
            outf_nspp <- sprintf(file.path(biodiv_dir, "output/impact_maps_by_taxon_ingredient/%s/%s/imp_unwt_%s_%s_%s_%s_nspp.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type, tx_type)
            
            
            outf_mean_df <- glue(file.path(biodiv_dir, "int/aoh_impacts_marine/{tx_type}_{diet_type}_{fcr}_{fs_type}_{ingredient_type}_{allocation_type}.rds"))
            
            
            if(all(file.exists(outf_mean, outf_sd))) {
              message('Rasters exist for taxon ', tx_type, allocation_type, diet_type, fcr, ingredient_type, fs_type, ' for catch/bycatch stressor... skipping!')
              next()
            }
            
            tx_maps_df <- spp_info_df %>%
              filter(taxon == tx_type) %>%
              dplyr::select(species, filepath) %>%
              distinct()
            
            ### read in all spp maps for this taxon - 
            message(glue('Loading spp maps for taxon {tx_type} {allocation_type} {ingredient_type} {fs_type} {diet_type}'))
            tx_maps <- collect_spp_rangemaps_marine(tx_maps_df$species, tx_maps_df$filepath)
            
            
            message('Taxon ', tx_type, ' spp dataframe: ', nrow(tx_maps[["parent"]]), 
                    ' cell observations for ', nrow(tx_maps_df), ' species...')
            
            tx_vuln_df <- spp_info_df %>%
              filter(taxon == tx_type,
                     allocation == allocation_type, 
                     diet == diet_type,
                     fish_type == fs_type, 
                     ingredient == ingredient_type)
            
            
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
            
            
            message('Processing mean/sd vulnerability by species in taxon ', tx_type, allocation_type, fs_type, ingredient_type, diet_type, fcr,
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
                                                  .[ , impact  := 1 - ((100 - impact_km2)/100)^0.25] %>% # only count species if they are impacted? 
                                                  .[impact>0]
                                                
                                                
                                                chunk_sum <- chunk_sum_spp %>%
                                                  .[ , .(impact_mean = mean(impact),
                                                         impact_sd   = sd(impact),
                                                         n_spp       = length(unique(species))),
                                                     by = 'cell_id']
                                                
                                                
                                                chunk_sum_spp_global <- chunk_sum_spp %>%
                                                  .[ , .(impact_total = sum(impact_km2)),
                                                     by = 'species']
                                                
                                                return(list(chunk_sum_spp_global = chunk_sum_spp_global, chunk_sum = chunk_sum))
                                              })
            
            
            
            
            
            if(check_tryerror(result_list)) {
              stop('Something went wrong with calculations for taxon ', tx_type, '!')
            }
            
            message(glue('Binding results for {tx_type} {allocation_type} {ingredient_type} {fs_type} {diet_type} {fcr}'))
            
            result_df <- rbindlist(lapply(result_list, function(x) x$chunk_sum)) %>%
              filter(!is.na(cell_id)) %>%
              as.data.frame()
            
            message('Creating and saving rasters for taxon ', tx_type, diet_type, fcr, allocation_type, fs_type, ingredient_type)
            rast_mean <- result_df %>%
              dplyr::select(cell_id, impact_mean) %>%
              left_join(moll_ocean_template) %>%
              dplyr::select(x, y, impact_mean) %>%
              filter(impact_mean > 0) %>%
              rast(., type = "xyz")
            
            rast_sd   <- result_df %>%
              dplyr::select(cell_id, impact_sd) %>%
              left_join(moll_ocean_template) %>%
              dplyr::select(x, y, impact_sd) %>%
              rast(., type = "xyz")
            rast_nspp <- result_df %>%
              dplyr::select(cell_id, n_spp) %>%
              left_join(moll_ocean_template) %>%
              dplyr::select(x, y, n_spp) %>%
              rast(., type = "xyz")
            
            writeRaster(rast_mean, outf_mean, overwrite = TRUE)
            writeRaster(rast_sd,   outf_sd, overwrite = TRUE)
            writeRaster(rast_nspp, outf_nspp, overwrite = TRUE)
            
            message(glue('Creating and saving global df for taxon {tx_type} {allocation_type} {ingredient_type} {fs_type} {diet_type} {fcr}'))
            
            global_df <- rbindlist(lapply(result_list, function(x) x$chunk_sum_spp_global)) %>% 
              as.data.frame() %>%
              group_by(species) %>%
              summarise(impact_total = sum(impact_total, na.rm = TRUE)) %>%
              mutate(allocation = allocation_type, diet = diet_type, fcr_type = fcr, ingredient = glue("{fs_type}_{ingredient_type}")) 
            
            fish_ingredient_type = unique(global_df$ingredient)
            
            write_rds(global_df, glue(file.path(biodiv_dir, "int/aoh_impacts_marine/{tx_type}_{diet_type}_{fcr}_{fish_ingredient_type}_{allocation_type}.rds")))
            
            
          }
        }
      }
    }
  }
}
