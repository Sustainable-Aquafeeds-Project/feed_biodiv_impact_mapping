## takes ~ 3 hours to complete! 

# In this script we overlap [terrestrial area of habitat maps](https://www.researchgate.net/publication/376637364_LIFE_A_metric_for_quantitively_mapping_the_impact_of_land-cover_change_on_global_extinctions) with our disturbance pressure maps created in the `02_feed` folder, and multiply by their vulnerability value. The goal of this script is to create impact maps, that is, the area of likely suitable habitat for each species that is exposured AND impacted to harvest of feed ingredients in salmon aquaculture. To do this, we: 
#   
# - We have created maps of disturbance (km2) of harvest of crop aquafeed ingredients. They have these categories: 
#   - Ingredient type; faba beans, guar meal, soybean meal, wheat meal, etc.
#   - Allocation type; mass, energetic, or economic
#   - Diet type; feed formulation; plant-dominant or fish-dominant
#   - crop type; pulses, soybeans, wheat, etc. 
#   - fcr type; regular (we no longer calculate an "efficient" scenario)
 

# - Overlap re-projected and AOH species suitable habitat maps with the appropriate disturbance rasters. This will provide a km2 estimate of the amount of suitable habitat that is EXPOSED to harvest of aquafeed ingredients.  
# - multiply by each species sensitivity values (downloaded in script 3a) to get IMPACT (km2) and save

# Code adapted from O'Hara et al. 2024 in review

## Data sources
# * Eyres et al. 2023 (preprint): https://www.researchgate.net/publication/376637364_LIFE_A_metric_for_quantitively_mapping_the_impact_of_land-cover_change_on_global_extinctions
# * O'Hara et al. 2023 (preprint): https://www.researchgate.net/publication/370573912_Cumulative_human_impacts_on_global_marine_fauna_highlight_risk_to_fragile_functional_diversity_of_marine_ecosystems
# * Williams et al. 2021: https://www.nature.com/articles/s41893-020-00656-5#

## Setup

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
terrestrial_dir <- file.path(rdsi_raw_data_dir, "AOH_eyres")
biodiv_dir <- file.path(rdsi_dir, "biodiversity_impacts")
feed_rast_dir <- file.path(biodiv_dir, "int/resampled_ingredient_rasts")


## Read in raster templates
gall_peters <- "+proj=cea +lon_0=0 +x_0=0 +y_0=0 +lat_ts=45 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

moll_land_template <- readRDS(file.path(this_dir, "data/spatial/moll_template_land_xy.rds"))

## get IUCN ids for each species
iucn_ids_names <- readRDS(here(this_dir, "data/iucn/eyres_iucn_spp.rds"))

## For loop

## Setup species map source and sensitivity data 

## get list of species 
spp_fp <- data.frame(filepath = list.files(file.path(terrestrial_dir, "reprojected_mol_csv_fin"), full.names = TRUE)) %>%
  mutate(species_full = str_after_last(filepath, "\\/")) %>%
  mutate(species_id = as.numeric(str_remove_all(species_full, ".csv"))) %>%
  left_join(iucn_ids_names, by = c("species_id" = "iucn_id")) %>%
  dplyr::select(filepath, species_full = scientific_name, spp_type, species_id) %>%
  mutate(spp_type = case_when(
    spp_type == "mammals" ~ "Terrestrial mammal", 
    spp_type == "birds" ~ "Bird",
    spp_type == "reptiles" ~ "Reptiles",
    spp_type == "amphibians" ~ "Amphibians"
  ))

## get sensitivity info dataframe
spp_info_df <- readRDS(file.path(this_dir, "int/terrestrial_spp_habitat_suitability.rds")) %>%
  inner_join(spp_fp, by = c("species" = "species_full", "spp_type")) %>% # change this to left join ? 
  rename(species_full = species) %>%
  mutate(cropland_sensitivity = 1 - cropland_suitability) %>%
  mutate(cropland_sensitivity_plus = case_when(cropland_sensitivity == 0 ~ 0.1,
                                               cropland_sensitivity == 0.5 ~ 0.6, 
                                               cropland_sensitivity == 1 ~ 1,
                                               TRUE ~ cropland_sensitivity)) %>%
  mutate(cropland_sensitivity_minus = case_when(cropland_sensitivity == 0 ~ 0,
                                                cropland_sensitivity == 0.5 ~ 0.4, 
                                                cropland_sensitivity == 1 ~ 0.9,
                                               TRUE ~ cropland_sensitivity))

## how many species are we running through?
spp_info_df %>% 
  group_by(spp_type) %>%
  summarize(nspp = n_distinct(species_full))

# # A tibble: 4 × 2
# spp_type            nspp
# <chr>              <int>
# 1 Amphibians          6792
# 2 Bird               10024
# 3 Reptiles            9279
# 4 Terrestrial mammal  5501

## get disturbance raster categories which we will run through
crop_ingredient_cats_1 <- read.csv(here("prep/02_feed/output/proportion_feed_per_country_system_diet_10-02-2024.csv")) %>%
  filter(fcr_type != "efficient") %>% # remove fcr scenarios because we no longer include those in the analysis!
  dplyr::select(diet, fcr_type, source_ingredient, GAEZ_category) %>%
  distinct() %>%
  mutate(diet_fcr = paste(diet, fcr_type, sep = "/")) %>%
  mutate(sensitivity_scen = "original") %>%
  mutate(diet_fcr_crop_ingredient = paste(diet_fcr, sensitivity_scen, sep = "/")) %>%
  mutate(diet_fcr_crop_ingredient = paste(diet_fcr_crop_ingredient, GAEZ_category, sep = "/")) %>%
  mutate(diet_fcr_crop_ingredient = paste(diet_fcr_crop_ingredient, source_ingredient, sep = "_")) ## filter here if you only want to do regular or efficient scenario
  
crop_ingredient_cats_2 <- read.csv(here("prep/02_feed/output/proportion_feed_per_country_system_diet_10-02-2024.csv")) %>%
  filter(fcr_type != "efficient") %>% # remove fcr scenarios because we no longer include those in the analysis!
  dplyr::select(diet, fcr_type, source_ingredient, GAEZ_category) %>%
  distinct() %>%
  mutate(diet_fcr = paste(diet, fcr_type, sep = "/")) %>%
  mutate(sensitivity_scen = "plus-10") %>%
  mutate(diet_fcr_crop_ingredient = paste(diet_fcr, sensitivity_scen, sep = "/")) %>%
  mutate(diet_fcr_crop_ingredient = paste(diet_fcr_crop_ingredient, GAEZ_category, sep = "/")) %>%
  mutate(diet_fcr_crop_ingredient = paste(diet_fcr_crop_ingredient, source_ingredient, sep = "_")) ## filter here if you only want to do regular or efficient scenario

crop_ingredient_cats_3 <- read.csv(here("prep/02_feed/output/proportion_feed_per_country_system_diet_10-02-2024.csv")) %>%
  filter(fcr_type != "efficient") %>% # remove fcr scenarios because we no longer include those in the analysis!
  dplyr::select(diet, fcr_type, source_ingredient, GAEZ_category) %>%
  distinct() %>%
  mutate(diet_fcr = paste(diet, fcr_type, sep = "/")) %>%
  mutate(sensitivity_scen = "minus-10") %>%
  mutate(diet_fcr_crop_ingredient = paste(diet_fcr, sensitivity_scen, sep = "/")) %>%
  mutate(diet_fcr_crop_ingredient = paste(diet_fcr_crop_ingredient, GAEZ_category, sep = "/")) %>%
  mutate(diet_fcr_crop_ingredient = paste(diet_fcr_crop_ingredient, source_ingredient, sep = "_")) ## filter here if you only want to do regular or efficient scenario

  
crop_ingredient_cats <- rbind(crop_ingredient_cats_1, crop_ingredient_cats_2, crop_ingredient_cats_3) %>%
  filter(sensitivity_scen != "original") # this is already done! 


allocations <- c("economic")
spp_types <- unique(spp_info_df$spp_type) # we'll loop through based on larger taxanomic groupings
diet_fcr_crop_ingredients <- unique(crop_ingredient_cats$diet_fcr_crop_ingredient)

for(tx_type in spp_types){
  
 # tx_type = "Reptiles"
  
  tx_vuln_df <- spp_info_df %>%
    filter(spp_type == tx_type)
  
  tx_maps_df <- tx_vuln_df %>%
    dplyr::select(species_full, filepath) %>%
    distinct()
  
  ### read in all harvest AOH maps for this taxon 
  message(glue('Loading aoh maps for taxon {tx_type}'))
  tx_maps <- collect_spp_rangemaps_terrestrial(tx_maps_df$species_full, tx_maps_df$filepath) # this function is defined in src/fxns.R
  
  
  
  message('Taxon ', tx_type, ' area of habitat maps: ', nrow(tx_maps[["parent"]]),
          ' cell observations for ', nrow(tx_maps_df), ' species...')
  
for(allocation_type in allocations){
    for(diet_fcr_crop_ingredient_type in diet_fcr_crop_ingredients){

# allocation_type = "economic"
# diet_fcr_crop_ingredient_type = "fish-dominant/regular/plus-10/Wheat_wheat gluten"

          diet_type = str_before_first(diet_fcr_crop_ingredient_type, "/")
          fcr <- str_before_first(str_after_first(diet_fcr_crop_ingredient_type, "/"), "/")
          sensitivity_type <- str_before_first(str_after_nth(diet_fcr_crop_ingredient_type, "/", 2), "/")
          ingredient_type = str_after_first(str_after_nth(diet_fcr_crop_ingredient_type, "/", 3), "_")
          crop_type =  str_before_first(str_after_nth(diet_fcr_crop_ingredient_type, "/", 3), "_")
          

          ## Read in harvest pressure maps, and create a dataframe of the results.  
          harvest_rast <- rast(sprintf(file.path(feed_rast_dir, "%s/%s/%s_%s_%s_A.tif"), diet_type, fcr, crop_type, ingredient_type, allocation_type))
          
          
          harvest_cells_df <- data.frame(harvest = as.vector(values(harvest_rast)),
                                         
                                         cell_id = 1:ncell(harvest_rast)) %>%
            filter(!is.na(harvest)) 
          
        
          
          
          ## Calculate mean impacts per species grouping 
          # Loop over each taxon; pull all AOH maps for that taxon. For each species in the taxon, multiply harvest pressure map by the spp vulnerability to identify impact map for that species. Summarize across the entire taxon to mean, sd, and nspp.

          ## comment this out if you want to rerun things
          if(file.exists(glue(file.path(biodiv_dir, "output/impact_maps_by_spp_ingredient_lists/{tx_type}_{diet_type}_{fcr}_{sensitivity_type}_{crop_type}_{ingredient_type}_{allocation_type}.qs")))) {
            message('Data exist for taxon ', tx_type, crop_type, ingredient_type, diet_type, allocation_type, fcr, ' for harvest stressor... skipping!')
            next()
          }

          
          # grab appropriate vuln_df column 
          if(sensitivity_type == "original"){
            tx_vuln_df_new <- tx_vuln_df %>%
              dplyr::select(species_full, cropland_sensitivity, spp_type, filepath, species_id)
          }else if(sensitivity_type == "plus-10"){
            tx_vuln_df_new <- tx_vuln_df %>%
              dplyr::select(species_full, cropland_sensitivity = cropland_sensitivity_plus, spp_type, filepath, species_id)
          }else {
            tx_vuln_df_new <- tx_vuln_df %>%
              dplyr::select(species_full, cropland_sensitivity = cropland_sensitivity_minus, spp_type, filepath, species_id)
          }
          
          message('Processing impact by species in taxon ', tx_type, 
                  ' to harvest pressure for ', allocation_type, diet_type, fcr, sensitivity_type, crop_type, ingredient_type)
          
          
          ### because failures might occur with summarizing a huge dataset,
          ### let's break this into chunks by cell_id - there are 6.6e+06 cells total
          ### but there are no ocean cells past 6.5e6, so we can cut out those cells
          
          chunk_size <- 500000
          n_chunks <- ceiling(6.6e6 / chunk_size)
          n_cores <- max(1, floor(n_chunks / ceiling(nrow(tx_maps[["parent"]])/3e7)))
          
          
          result_list <- parallel::mclapply(1:n_chunks, mc.cores = n_cores,
                                            FUN = function(n) { 
                                              
                                              ### n <- 6
                                              cell_id_min <- as.integer((n - 1) * chunk_size + 1)
                                              cell_id_max <- as.integer(n * chunk_size)
                                              message('Summarizing harvest pressure to impact on taxon ', tx_type, 
                                                      ': cells ', cell_id_min, ' - ', cell_id_max, '...')
                                              
                                              chunk_sum_spp <- tx_maps %>%
                                                filter(between(cell_id, cell_id_min, cell_id_max)) %>% # filter for chunked cells
                                                left_join(tx_vuln_df_new, by = c('species_full')) %>% # join to sensitivity df
                                                left_join(harvest_cells_df, 
                                                          by = c('cell_id')) %>% # join to pressure cells
                                                as.data.table() %>%
                                                .[ , harvest := ifelse(is.na(harvest), 0, harvest)] %>% # if no pressure present then its 0 (obvi)
                                                .[ , hab_area := 100] %>% # the cell size is approx 100km2
                                                .[ , impact_km2  := cropland_sensitivity*harvest] # calculate impact (1-suitability) = sensitivity
                                            
                                                chunk_sum_spp_hab <- chunk_sum_spp %>%
                                                  .[, .(species_full, cell_id, hab_area)] %>%
                                                .[, .SD[!duplicated(.SD, by = c('species_full', 'cell_id', 'hab_area'))]] %>%
                                                  .[ , .(hab_area = sum(hab_area)),
                                                     by = c('species_full')] # get a dataframe that has habitat area summarised for each spp 
                                                
                                                chunk_sum_spp_global <- chunk_sum_spp %>%
                                                  .[ , .(impact_total = sum(impact_km2)),
                                                     by = c('species_full')] %>% 
                                                  merge(., chunk_sum_spp_hab, by = "species_full", all = TRUE) # save df that have global spp level impacts summarised 
                                                
                                                chunk_sum_spp <- chunk_sum_spp %>%
                                                    .[impact_km2>0] # this is impact for each cell for each species
                                                
                                             return(list(chunk_sum_spp = chunk_sum_spp, chunk_sum_spp_global = chunk_sum_spp_global))
                                            }) 
          
          
          
          if(check_tryerror(result_list)) {
            stop('Something went wrong with calculations for taxon {tx_type} {allocation_type} {diet_fcr_crop_ingredient_type}')
          }
          
          message(glue('Binding results for taxon {tx_type} {allocation_type} {diet_fcr_crop_ingredient_type}'))
         
          message(glue('Creating and saving global df for taxon {tx_type} {allocation_type} {diet_fcr_crop_ingredient_type}'))
                    
          global_df <- rbindlist(lapply(result_list, function(x) x$chunk_sum_spp_global)) %>% 
            as.data.frame() %>%
            group_by(species_full) %>%
            summarise(impact_total = sum(impact_total, na.rm = TRUE),
                      hab_area = sum(hab_area, na.rm = TRUE)) %>%
            mutate(allocation = allocation_type, diet_fcr_crop_ingredient = diet_fcr_crop_ingredient_type) %>%
            mutate(diet = diet_type) %>%
            mutate(fcr_type = fcr) %>%
            mutate(crop_ingredient = paste0(crop_type, "_", ingredient_type)) %>%
            mutate(sensitivity_scenario = sensitivity_type) %>%
            ungroup() # save a global df for each spp and crop impact
          
          # diet_type = unique(global_df$diet)
          # fcr <- unique(global_df$fcr_type)
          crop_ingredient_type <- unique(global_df$crop_ingredient)

          write_rds(global_df, glue(file.path(biodiv_dir, "int/aoh_impacts_terrestrial/{tx_type}_{diet_type}_{fcr}_{sensitivity_type}_{crop_ingredient_type}_{allocation_type}.rds")))
  

          spp_list <- lapply(result_list, function(x) {
            if ("chunk_sum_spp" %in% names(x)) {
              return(x$chunk_sum_spp)
            } else {
              return(NA)  # Return NA or an empty dataframe if 'chunk_sum_spp' is not found
            }
          })
          
          # we are saving these datasets as .qs files are they are great at compacting data... some of these are VERY LARGE datasets
          qsave(spp_list, glue(file.path(biodiv_dir, "output/impact_maps_by_spp_ingredient_lists/{tx_type}_{diet_type}_{fcr}_{sensitivity_type}_{crop_ingredient_type}_{allocation_type}.qs")))

          }
      }
  }

  


