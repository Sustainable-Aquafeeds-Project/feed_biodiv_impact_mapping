
## takes ~ 1.5 hours to complete! 

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

select <- dplyr::select
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # set working directory to where this script is located
this_dir <- getwd()
feed_rast_dir <- here("prep/02_feed/output/resampled")

options(dplyr.summarise.inform = FALSE)
source(here("src/directories.R"))
source(here("src/spatial.R"))
source(here("src/fxns.R"))

aquamaps_dir <- file.path(rdsi_raw_data_dir, "aquamaps")
terrestrial_dir <- file.path(rdsi_raw_data_dir, "AOH_lumbierres")
biodiv_dir <- file.path(rdsi_dir, "biodiversity_impacts")


gall_peters <- "+proj=cea +lon_0=0 +x_0=0 +y_0=0 +lat_ts=45 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

moll_land_template <- readRDS(file.path(this_dir, "data/spatial/moll_template_land_xy.rds"))

## Setup spp map source and vulnerability data 
spp_fp <- data.frame(filepath = list.files(file.path(terrestrial_dir, "reprojected_mol_csv"), full.names = TRUE)) %>%
  mutate(species = str_remove_all(str_replace_all(str_after_last(filepath, "\\/"), "_", " "), ".csv"))

spp_info_df <- readRDS(file.path(this_dir, "int/terrestrial_spp_habitat_suitability.rds")) %>%
  left_join(spp_fp)

spp_info_df %>% 
  group_by(spp_type) %>%
  summarize(nspp = n_distinct(species))

# # A tibble: 2 × 2
# spp_type            nspp
# <chr>              <int>
#   1 Bird               10651
# 2 Terrestrial mammal  5482

crop_ingredient_cats <- read.csv(here("prep/02_feed/output/proportion_feed_per_country_system_diet.csv")) %>%
  dplyr::select(diet, source_ingredient, GAEZ_category) %>%
  distinct() %>%
  mutate(diet_crop_ingredient = paste(diet, GAEZ_category, sep = "/")) %>%
  mutate(diet_crop_ingredient = paste(diet_crop_ingredient, source_ingredient, sep = "_"))

## should end up with: 
# plant-dominant: 13 ingredients * 3 allocations * 2 taxon * 3 raster types = 234 outputs
# fish-dominant: 5 ingredients * 3 allocations * 2 taxon * 3 raster types = 90 outputs

allocations <- c("economic", "ge", "mass")
spp_types <- unique(spp_info_df$spp_type)
diet_crop_ingredients <- unique(crop_ingredient_cats$diet_crop_ingredient)

for(allocation_type in allocations){
    for(diet_crop_ingredient_type in diet_crop_ingredients){
        for(tx_type in spp_types){

          # allocation_type = "ge"
          # diet_crop_ingredient_type = "fish-dominant/Maize_corn gluten meal"
          # tx_type = "Terrestrial mammal"
          
          diet_type = str_before_first(diet_crop_ingredient_type, "/")
          ingredient_type = str_after_first(diet_crop_ingredient_type, "_")
          crop_type = str_after_first(str_before_first(diet_crop_ingredient_type, "_"), "/")
        
          
          tx_vuln_df <- spp_info_df %>%
            filter(spp_type == tx_type) 
          
          tx_maps_df <- tx_vuln_df %>%
            dplyr::select(species, filepath) %>%
            distinct()
          

          ## Read in harvest stressor maps, and create a dataframe of the results.  
          harvest_rast <- rast(sprintf(file.path(feed_rast_dir, "%s/%s_%s_%s_A.tif"), diet_type, crop_type, ingredient_type, allocation_type))
         
          
          harvest_cells_df <- data.frame(harvest = as.vector(values(harvest_rast)),
                                         
                                         cell_id = 1:ncell(harvest_rast)) %>%
            filter(!is.na(harvest)) 
          
        
          
          
          ## Calculate mean impacts per species grouping 
          # Loop over each taxon; pull all rangemaps for that taxon. For each species in the taxon, multiply harvest stressor map by the spp vulnerability to identify impact map for that species. Summarize across the entire taxon to mean, sd, and nspp.
          
          outf_mean <- sprintf(file.path(biodiv_dir, "output/impact_maps_by_taxon/%s/imp_unwt_%s_%s_%s_%s_mean.tif"), diet_type, crop_type, ingredient_type, allocation_type, tx_type)
          outf_sd <- sprintf(file.path(biodiv_dir, "output/impact_maps_by_taxon/%s/imp_unwt_%s_%s_%s_%s_sd.tif"), diet_type, crop_type, ingredient_type, allocation_type, tx_type)
          outf_nspp <- sprintf(file.path(biodiv_dir, "output/impact_maps_by_taxon/%s/imp_unwt_%s_%s_%s_%s_nspp.tif"), diet_type, crop_type, ingredient_type, allocation_type, tx_type)

          if(all(file.exists(outf_mean, outf_sd))) {
            message('Rasters exist for taxon ', tx_type, crop_type, ingredient_type, diet_type, allocation_type, ' for harvest stressor... skipping!')
            next()
          }
          
          ### read in all harvest stressor maps for this taxon - 
          message('Loading aoh maps for taxon ', tx_type, '...')
          tx_maps <- collect_spp_rangemaps(tx_maps_df$species, tx_maps_df$filepath)
          
          
          message('Taxon ', tx_type, ' harvest stressor dataframe: ', nrow(tx_maps[["parent"]]), 
                  ' cell observations for ', nrow(tx_maps_df), ' species...')
          
          message('Processing mean/sd vulnerability by species in taxon ', tx_type, 
                  ' to harvest stressor for ', allocation_type, diet_type, crop_type, ingredient_type)
          
          ### because failures might occur with summarizing a huge dataset,
          ### let's break this into chunks by cell_id - there are 6.6e+06 cells total
          ### but no ocean cells past 6.5e6
          
          chunk_size <- 500000
          n_chunks <- ceiling(6.6e6 / chunk_size)
          n_cores <- max(1, floor(n_chunks / ceiling(nrow(tx_maps[["parent"]])/3e7)))
          
          result_list <- parallel::mclapply(1:n_chunks, mc.cores = n_cores,
                                            FUN = function(n) { ### n <- 6
                                              cell_id_min <- as.integer((n - 1) * chunk_size + 1)
                                              cell_id_max <- as.integer(n * chunk_size)
                                              message('Summarizing harvest stressor on taxon ', tx_type, 
                                                      ': cells ', cell_id_min, ' - ', cell_id_max, '...')
                                              
                                              chunk_sum <- tx_maps %>%
                                                # as.data.frame() %>% 
                                                filter(between(cell_id, cell_id_min, cell_id_max)) %>%
                                                left_join(tx_vuln_df, by = c('species')) %>%
                                                left_join(harvest_cells_df, 
                                                                by = c('cell_id')) %>%
                                               as.data.table() %>%
                                              .[ , harvest := ifelse(is.na(harvest), 0, harvest)] %>%
                                              .[ , impact  := (1-cropland_suitability)*harvest] %>%
                                              .[ , impact  := 1 - ((100 - impact)/100)^0.25] %>%
                                              .[ , .(impact_mean = mean(impact),
                                                     impact_sd   = sd(impact),
                                                     n_spp       = length(unique(species))),
                                                 by = 'cell_id']
                                            }) 
          
          
          
          if(check_tryerror(result_list)) {
            stop('Something went wrong with calculations for taxon ', tx_type, '!')
          }
          
          message('Binding results for taxon ', tx_type, '...')
          
          result_df <- result_list %>%
            data.table::rbindlist() %>%
            filter(!is.na(cell_id)) %>% 
            as.data.frame()
          
          message('Creating and saving rasters for taxon ', tx_type, '...')
          rast_mean <- result_df %>% 
            dplyr::select(cell_id, impact_mean) %>% 
            left_join(moll_land_template) %>%
            dplyr::select(x, y, impact_mean) %>%
            rast(., type = "xyz", crs = moll_template)
          rast_sd   <- result_df %>% 
            dplyr::select(cell_id, impact_sd) %>% 
            left_join(moll_land_template) %>%
            dplyr::select(x, y, impact_sd) %>%
            rast(., type = "xyz", crs = moll_template)
          rast_nspp <- result_df %>% 
            dplyr::select(cell_id, n_spp) %>% 
            left_join(moll_land_template) %>%
            dplyr::select(x, y, n_spp) %>%
            rast(., type = "xyz", crs = moll_template)
          
          writeRaster(rast_mean, outf_mean, overwrite = TRUE)
          writeRaster(rast_sd,   outf_sd, overwrite = TRUE)
          writeRaster(rast_nspp, outf_nspp, overwrite = TRUE)
          
          
          }
      }
    }
  

