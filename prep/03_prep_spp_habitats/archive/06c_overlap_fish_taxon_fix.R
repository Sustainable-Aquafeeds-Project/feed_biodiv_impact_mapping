#### Takes ~20ish? hours to run all scenarios... sorry.
## This script accomplishes the same thing as 06b, but only for the finfish taxon. The finfish taxon was too large to process in one go, so we needed to split it so that our server could handle it. We take a species weighted mean, and a pooled variance to get the final mean and sd maps for the finfish taxon. 

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
# - Overlap re-projected and clipped Aquamaps species suitable habitat maps for FINFISH TAXON with the appropriate disturbance rasters based on depth range. This will provide a km2 estimate of the amount of suitable habitat that is exposed to harvest of forage or trimmings fish used for FMFO. 
# - multiply by each species vulnerability values to get impact and save
# - then we take a species weighted mean of each finfish raster to get the final fish taxon raster. We are doing this workaround because our server is memory limited and cannot handle all 9k species of finfish at once and fails. 


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
diets <- unique(spp_info_df_fish$diet)
ingredients <- unique(spp_info_df_fish$ingredient)
fish_types <- unique(spp_info_df_fish$fish_type)
spp_types <- unique(spp_info_df_fish$taxon)
fcrs <- c("regular", "efficient")

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


          # allocation_type = "economic"
          # diet_type = "plant-dominant"
          # ingredient_type = "fish oil, cut offs"
          # fs_type = "trimmings fish"

          if(fs_type == "trimmings fish" & ingredient_type == "fish meal" |fs_type == "trimmings fish" & ingredient_type == "fish oil" |
             fs_type == "forage fish" & ingredient_type == "fish meal, cut offs"| fs_type == "forage fish" & ingredient_type == "fish oil, cut offs") {
            message('Not a category! ... skipping!')
            next()
          }

          outf_mean <- sprintf(file.path(fish_dir, "%s/%s/imp_unwt_%s_%s_%s_%s_mean.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type, tx_type)
          outf_sd <- sprintf(file.path(fish_dir, "%s/%s/imp_unwt_%s_%s_%s_%s_sd.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type, tx_type)
          outf_nspp <- sprintf(file.path(fish_dir, "%s/%s/imp_unwt_%s_%s_%s_%s_nspp.tif"), diet_type,fcr, fs_type, ingredient_type, allocation_type, tx_type)

          outf_mean_prop <- sprintf(file.path(fish_dir, "%s/%s/prop_unwt_%s_%s_%s_%s_mean.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type, tx_type)
          outf_sd_prop <- sprintf(file.path(fish_dir, "%s/%s/prop_unwt_%s_%s_%s_%s_sd.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type, tx_type)
          outf_nspp_prop <- sprintf(file.path(fish_dir, "%s/%s/prop_unwt_%s_%s_%s_%s_nspp.tif"), diet_type,fcr, fs_type, ingredient_type, allocation_type, tx_type)


          ## comment out if you want to rerun things without skipping!
          # if(all(file.exists(outf_mean, outf_sd))) {
          #    message('Rasters exist for taxon ', tx_type, allocation_type, diet_type, fcr, ingredient_type, fs_type, ' for catch/bycatch stressor... skipping!')
          #   next()
          # }

          outf_mean_df <- glue(file.path(biodiv_dir, "int/aoh_impacts_marine/{tx_type}_{diet_type}_{fcr}_{fs_type}_{ingredient_type}_{allocation_type}.rds"))

          # if(all(file.exists(outf_mean_df))) {
          #   message('Rasters exist for taxon ', tx_type, ' for bycatch stressor... skipping!')
          #   next()
          # }

          tx_vuln_df <- spp_info_df_fish %>%
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


          message('Processing mean/sd vulnerability by species in taxon ', tx_type, allocation_type, fs_type, ingredient_type, diet_type,
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
                                                .[ , hab_area := 100] %>%
                                                .[ , impact  := 1 - ((100 - impact_km2)/100)^0.25] %>% # only count species if they are impacted?
                                                .[ , prop_impact  := 1 - ((hab_area - impact_km2)/hab_area)] 
                                              
                                              
                                              
                                              chunk_sum <- chunk_sum_spp %>%
                                                .[impact > 0] %>% 
                                                .[ , .(impact_mean = mean(impact),
                                                       impact_sd   = sd(impact),
                                                       n_spp       = length(unique(species))),
                                                   by = 'cell_id']
                                              
                                              ## add in another chunk sum with mean proportion habitat left
                                              chunk_sum_prop <- chunk_sum_spp %>%
                                                .[impact > 0] %>% 
                                                .[ , .(prop_mean = mean(prop_impact),
                                                       prop_sd   = sd(prop_impact),
                                                       n_spp       = length(unique(species))),
                                                   by = 'cell_id']
                                              
                                              
                                              chunk_sum_spp_hab <- chunk_sum_spp %>%
                                                .[, .(species, cell_id, hab_area)] %>%
                                                .[, .SD[!duplicated(.SD, by = c('species', 'cell_id', 'hab_area'))]] %>%
                                                .[ , .(hab_area = sum(hab_area)),
                                                   by = c('species')]
                                              
                                              chunk_sum_spp_global <- chunk_sum_spp %>%
                                                .[ , .(impact_total = sum(impact_km2),
                                                       impact_mean = mean(impact)),
                                                   by = c('species')] %>%
                                                merge(., chunk_sum_spp_hab, by = "species", all = TRUE)

                                              return(list(chunk_sum_spp_global = chunk_sum_spp_global, chunk_sum = chunk_sum, chunk_sum_prop = chunk_sum_prop))
                                            })



          if(check_tryerror(result_list)) {
            stop('Something went wrong with calculations for taxon ', tx_type, '!')
          }

          message(glue('Binding results for {tx_type} {allocation_type} {ingredient_type} {fs_type} {diet_type} {fcr}'))


          result_df <- rbindlist(lapply(result_list, function(x) x$chunk_sum)) %>%
            filter(!is.na(cell_id)) %>%
            as.data.frame()

          result_df_prop <- rbindlist(lapply(result_list, function(x) x$chunk_sum_prop)) %>%
            filter(!is.na(cell_id)) %>%
            as.data.frame()


          message('Creating and saving rasters for taxon ', tx_type, diet_type, fcr, allocation_type, fs_type, ingredient_type)
          rast_mean <- result_df %>%
            dplyr::select(cell_id, impact_mean) %>%
            left_join(moll_ocean_template) %>%
            dplyr::select(x, y, impact_mean) %>%
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


            rast_mean_prop <- result_df_prop %>%
              dplyr::select(cell_id, prop_mean) %>%
              left_join(moll_ocean_template) %>%
              dplyr::select(x, y, prop_mean) %>%
              filter(prop_mean > 0) %>%
              rast(., type = "xyz")

            rast_sd_prop   <- result_df_prop %>%
              dplyr::select(cell_id, prop_sd) %>%
              left_join(moll_ocean_template) %>%
              dplyr::select(x, y, prop_sd) %>%
              rast(., type = "xyz")

            rast_nspp_prop <- result_df_prop %>%
              dplyr::select(cell_id, n_spp) %>%
              left_join(moll_ocean_template) %>%
              dplyr::select(x, y, n_spp) %>%
              rast(., type = "xyz")



          writeRaster(rast_mean_prop, outf_mean_prop, overwrite = TRUE)
          writeRaster(rast_sd_prop,   outf_sd_prop, overwrite = TRUE)
          writeRaster(rast_nspp_prop, outf_nspp_prop, overwrite = TRUE)


          message(glue('Creating and saving global df for taxon {tx_type} {allocation_type} {ingredient_type} {fs_type} {diet_type} {fcr}'))

          global_df <- rbindlist(lapply(result_list, function(x) x$chunk_sum_spp_global)) %>%
            as.data.frame() %>%
            group_by(species) %>%
            summarise(impact_total = sum(impact_total, na.rm = TRUE),
                      extinction_mean = mean(impact_mean, na.rm = TRUE),
                      hab_area = sum(hab_area, na.rm = TRUE)) %>%
            mutate(allocation = allocation_type, diet = diet_type, fcr_type = fcr, ingredient = glue("{fs_type}_{ingredient_type}"))

          fish_ingredient_type = unique(global_df$ingredient)

          write_rds(global_df, glue(file.path(biodiv_dir, "int/aoh_impacts_marine/{tx_type}_{diet_type}_{fcr}_{fish_ingredient_type}_{allocation_type}.rds")))

          }
      }
    }
   }
  }
}


## now combine the split fish1 and fish2 into one "fish" category. We take a regular spp weighted mean, add the nspp rasters, and calculate the pooled variance and sdev to get these. 

for(allocation_type in allocations){
  for(diet_type in diets){
    for(fcr in fcrs){
    for(ingredient_type in ingredients){
      for(fs_type in fish_types){

# allocation_type = "economic"
# diet_type = "fish-dominant"
# ingredient_type = "fish meal, cut offs"
# fs_type = "trimmings fish"
  #      fcr = "regular"
        
        tx_type = "fish"

        if(fs_type == "trimmings fish" & ingredient_type == "fish meal" |fs_type == "trimmings fish" & ingredient_type == "fish oil" |
           fs_type == "forage fish" & ingredient_type == "fish meal, cut offs"| fs_type == "forage fish" & ingredient_type == "fish oil, cut offs") {
          message('Not a category! ... skipping!')
          next()
        }

       rast_mean_1 <- rast(sprintf(file.path(fish_dir, "%s/%s/imp_unwt_%s_%s_%s_fish1_mean.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type))
       crs(rast_mean_1) <- crs(moll_template)
       rast_mean_1 <- rast_mean_1 %>%
         project(., moll_template, method = "near")

        rast_sd_1 <- rast(sprintf(file.path(fish_dir, "%s/%s/imp_unwt_%s_%s_%s_fish1_sd.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type))
        crs(rast_sd_1) <- crs(moll_template)
        rast_sd_1 <- rast_sd_1 %>%
          project(., moll_template, method = "near")

        rast_nspp_1 <- rast(sprintf(file.path(fish_dir, "%s/%s/imp_unwt_%s_%s_%s_fish1_nspp.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type))
        crs(rast_nspp_1) <- crs(moll_template)
        rast_nspp_1 <- rast_nspp_1 %>%
          project(., moll_template, method = "near")


        rast_mean_2 <- rast(sprintf(file.path(fish_dir, "%s/%s/imp_unwt_%s_%s_%s_fish2_mean.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type))
        crs(rast_mean_2) <- crs(moll_template)
        rast_mean_2 <- rast_mean_2 %>%
          project(., moll_template, method = "near")

        rast_sd_2 <- rast(sprintf(file.path(fish_dir, "%s/%s/imp_unwt_%s_%s_%s_fish2_sd.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type))
        crs(rast_sd_2) <- crs(moll_template)
        rast_sd_2 <- rast_sd_2 %>%
          project(., moll_template, method = "near")

        rast_nspp_2 <- rast(sprintf(file.path(fish_dir, "%s/%s/imp_unwt_%s_%s_%s_fish2_nspp.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type))
        crs(rast_nspp_2) <- crs(moll_template)
        rast_nspp_2 <- rast_nspp_2 %>%
          project(., moll_template, method = "near")


        outf_mean <- sprintf(file.path(biodiv_dir, "output/impact_maps_by_taxon_ingredient/%s/%s/imp_unwt_%s_%s_%s_fish_mean.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type)
        outf_sd <- sprintf(file.path(biodiv_dir, "output/impact_maps_by_taxon_ingredient/%s/%s/imp_unwt_%s_%s_%s_fish_sd.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type)
        outf_nspp <- sprintf(file.path(biodiv_dir, "output/impact_maps_by_taxon_ingredient/%s/%s/imp_unwt_%s_%s_%s_fish_nspp.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type)


        # if(all(file.exists(outf_mean, outf_sd))) {
        #   message('Rasters exist for taxon ', tx_type, ' for bycatch stressor... skipping!')
        #   next()
        # }


        rast_nspp <- rast_nspp_1 + rast_nspp_2

        rast_mean <- (rast_mean_1*rast_nspp_1 + rast_mean_2*rast_nspp_2)/(rast_nspp)

        mean_df_1 <- rast_mean_1 %>%
          as.data.frame(xy = TRUE) %>%
          mutate(taxon = "fish1") %>%
          rename("imp_mean" = "impact_mean")

        sd_df_1 <- rast_sd_1 %>%
          as.data.frame(xy=TRUE) %>%
          mutate(taxon = "fish1") %>%
          rename("imp_sdev" = 3)

        nspp_df_1 <- rast_nspp_1 %>%
          as.data.frame(xy = TRUE) %>%
          mutate(taxon = "fish1") %>%
          rename("imp_nspp" = 3)

        mean_df_2 <- rast_mean_2 %>%
          as.data.frame(xy = TRUE) %>%
          mutate(taxon = "fish2") %>%
          rename("imp_mean" = "impact_mean")

        sd_df_2 <- rast_sd_2 %>%
          as.data.frame(xy=TRUE) %>%
          mutate(taxon = "fish2") %>%
          rename("imp_sdev" = 3)

        nspp_df_2 <- rast_nspp_2 %>%
          as.data.frame(xy = TRUE) %>%
          mutate(taxon = "fish2") %>%
          rename("imp_nspp" = 3)

        mean_df <- rbind(mean_df_1, mean_df_2) %>%
          left_join(moll_ocean_template, by = c("x", "y")) %>%
          dplyr::select(-x, -y)
        sdev_df <- rbind(sd_df_1, sd_df_2) %>%
          left_join(moll_ocean_template, by = c("x", "y")) %>%
          dplyr::select(-x, -y)
        nspp_df <- rbind(nspp_df_1, nspp_df_2) %>%
          left_join(moll_ocean_template, by = c("x", "y")) %>%
          dplyr::select(-x, -y)

        rm(mean_df_1, mean_df_2, sd_df_1, sd_df_2, nspp_df_1, nspp_df_2)

        big_df <- mean_df %>%
          full_join(sdev_df, by = c('taxon', 'cell_id')) %>%
          full_join(nspp_df, by = c('taxon', 'cell_id'))

        rm(mean_df, sdev_df, nspp_df)

        rast_sd <- big_df %>%
          group_by(cell_id) %>%
          summarize(imp_var = iterated_pooled_var(imp_mean, imp_sdev, imp_nspp),
                    imp_sdev = sqrt(imp_var)) %>%
          left_join(moll_ocean_template, by = "cell_id") %>%
          dplyr::select(x, y, imp_sdev) %>%
          rast(., type = "xyz", crs = moll_template)



        writeRaster(rast_mean, outf_mean, overwrite = TRUE)
        writeRaster(rast_sd,   outf_sd, overwrite = TRUE)
        writeRaster(rast_nspp, outf_nspp, overwrite = TRUE)



          }
        }
      }
    }
  }

## do proportion impacts 

fcrs <- c("regular", "efficient")

for(allocation_type in allocations){
  for(diet_type in diets){
    for(fcr in fcrs){
      for(ingredient_type in ingredients){
        for(fs_type in fish_types){
          
          # allocation_type = "economic"
          # diet_type = "fish-dominant"
          # ingredient_type = "fish meal, cut offs"
          # fs_type = "trimmings fish"
          #      fcr = "regular"
          
          tx_type = "fish"
          
          if(fs_type == "trimmings fish" & ingredient_type == "fish meal" |fs_type == "trimmings fish" & ingredient_type == "fish oil" |
             fs_type == "forage fish" & ingredient_type == "fish meal, cut offs"| fs_type == "forage fish" & ingredient_type == "fish oil, cut offs") {
            message('Not a category! ... skipping!')
            next()
          }
          
          rast_mean_1 <- rast(sprintf(file.path(fish_dir, "%s/%s/prop_unwt_%s_%s_%s_fish1_mean.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type))
          crs(rast_mean_1) <- crs(moll_template)
          rast_mean_1 <- rast_mean_1 %>%
            project(., moll_template, method = "near")
          
          rast_sd_1 <- rast(sprintf(file.path(fish_dir, "%s/%s/prop_unwt_%s_%s_%s_fish1_sd.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type))
          crs(rast_sd_1) <- crs(moll_template)
          rast_sd_1 <- rast_sd_1 %>%
            project(., moll_template, method = "near")
          
          rast_nspp_1 <- rast(sprintf(file.path(fish_dir, "%s/%s/prop_unwt_%s_%s_%s_fish1_nspp.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type))
          crs(rast_nspp_1) <- crs(moll_template)
          rast_nspp_1 <- rast_nspp_1 %>%
            project(., moll_template, method = "near")
          
          
          rast_mean_2 <- rast(sprintf(file.path(fish_dir, "%s/%s/prop_unwt_%s_%s_%s_fish2_mean.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type))
          crs(rast_mean_2) <- crs(moll_template)
          rast_mean_2 <- rast_mean_2 %>%
            project(., moll_template, method = "near")
          
          rast_sd_2 <- rast(sprintf(file.path(fish_dir, "%s/%s/prop_unwt_%s_%s_%s_fish2_sd.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type))
          crs(rast_sd_2) <- crs(moll_template)
          rast_sd_2 <- rast_sd_2 %>%
            project(., moll_template, method = "near")
          
          rast_nspp_2 <- rast(sprintf(file.path(fish_dir, "%s/%s/prop_unwt_%s_%s_%s_fish2_nspp.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type))
          crs(rast_nspp_2) <- crs(moll_template)
          rast_nspp_2 <- rast_nspp_2 %>%
            project(., moll_template, method = "near")
          
          
          outf_mean <- sprintf(file.path(biodiv_dir, "output/impact_maps_by_taxon_ingredient/%s/%s/prop_unwt_%s_%s_%s_fish_mean.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type)
          outf_sd <- sprintf(file.path(biodiv_dir, "output/impact_maps_by_taxon_ingredient/%s/%s/prop_unwt_%s_%s_%s_fish_sd.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type)
          outf_nspp <- sprintf(file.path(biodiv_dir, "output/impact_maps_by_taxon_ingredient/%s/%s/prop_unwt_%s_%s_%s_fish_nspp.tif"), diet_type, fcr, fs_type, ingredient_type, allocation_type)
          
          
          # if(all(file.exists(outf_mean, outf_sd))) {
          #   message('Rasters exist for taxon ', tx_type, ' for bycatch stressor... skipping!')
          #   next()
          # }
          
          
          rast_nspp <- rast_nspp_1 + rast_nspp_2
          
          rast_mean <- (rast_mean_1*rast_nspp_1 + rast_mean_2*rast_nspp_2)/(rast_nspp)
          
          mean_df_1 <- rast_mean_1 %>%
            as.data.frame(xy = TRUE) %>%
            mutate(taxon = "fish1") %>%
            rename("prop_mean" = 3)
          
          sd_df_1 <- rast_sd_1 %>%
            as.data.frame(xy=TRUE) %>%
            mutate(taxon = "fish1") %>%
            rename("prop_sd" = 3)
          
          nspp_df_1 <- rast_nspp_1 %>%
            as.data.frame(xy = TRUE) %>%
            mutate(taxon = "fish1") %>%
            rename("prop_nspp" = 3)
          
          mean_df_2 <- rast_mean_2 %>%
            as.data.frame(xy = TRUE) %>%
            mutate(taxon = "fish2") %>%
            rename("prop_mean" = "prop_mean")
          
          sd_df_2 <- rast_sd_2 %>%
            as.data.frame(xy=TRUE) %>%
            mutate(taxon = "fish2") %>%
            rename("prop_sd" = 3)
          
          nspp_df_2 <- rast_nspp_2 %>%
            as.data.frame(xy = TRUE) %>%
            mutate(taxon = "fish2") %>%
            rename("prop_nspp" = 3)
          
          mean_df <- rbind(mean_df_1, mean_df_2) %>%
            left_join(moll_ocean_template, by = c("x", "y")) %>%
            dplyr::select(-x, -y)
          sdev_df <- rbind(sd_df_1, sd_df_2) %>%
            left_join(moll_ocean_template, by = c("x", "y")) %>%
            dplyr::select(-x, -y)
          nspp_df <- rbind(nspp_df_1, nspp_df_2) %>%
            left_join(moll_ocean_template, by = c("x", "y")) %>%
            dplyr::select(-x, -y)
          
          rm(mean_df_1, mean_df_2, sd_df_1, sd_df_2, nspp_df_1, nspp_df_2)
          
          big_df <- mean_df %>%
            full_join(sdev_df, by = c('taxon', 'cell_id')) %>%
            full_join(nspp_df, by = c('taxon', 'cell_id'))
          
          rm(mean_df, sdev_df, nspp_df)
          
          rast_sd <- big_df %>%
            group_by(cell_id) %>%
            summarize(prop_var = iterated_pooled_var(prop_mean, prop_sd, prop_nspp),
                      prop_sd = sqrt(prop_var)) %>%
            left_join(moll_ocean_template, by = "cell_id") %>%
            dplyr::select(x, y, prop_sd) %>%
            rast(., type = "xyz", crs = moll_template)
          
          
          
          writeRaster(rast_mean, outf_mean, overwrite = TRUE)
          writeRaster(rast_sd,   outf_sd, overwrite = TRUE)
          writeRaster(rast_nspp, outf_nspp, overwrite = TRUE)
          
          
          
        }
      }
    }
  }
}
