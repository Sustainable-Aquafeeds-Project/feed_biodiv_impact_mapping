
library(tidyverse)
library(here)
library(terra)
library(strex)
library(janitor)
library(tools)
library(glue)
library(tidyterra)
library(qs)

source(here("src/directories.R"))

source(here("src/spatial.R"))
source(here("src/fxns.R"))

select <- dplyr::select
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # set working directory to where this script is located
this_dir <- getwd()

aoh_dir <- file.path(rdsi_raw_data_dir, "AOH_lumbierres/reprojected_csvs/")

base_rast <- rast(res=0.5)
ext(base_rast) <- c(-180, 180, -90, 90)

biodiv_dir <- file.path(rdsi_dir, "biodiversity_impacts")

impact_maps_dir <- file.path(biodiv_dir, "output")

diet_types <- c("plant-dominant", "fish-dominant")
fcr_types <- c("regular")
allocation_types <- c("economic")


for(diet in diet_types){
  for(fcr in fcr_types){
    for(allocation in allocation_types){
      
      all_impact_df <- data.frame(cell_id = NA, prop_mean = NA, prop_sd = NA, prop_nspp = NA)
      
      for(chunk_id in 1:13){
        
        # chunk_id = 1
        
        files <- list.files(file.path(biodiv_dir, "int/marine_aggregation/sub_chunk_dfs/"), pattern = glue(".*_{diet}_{fcr}_.*_{allocation}_chunk_{chunk_id}.qs"), full.names = TRUE)
        
        impacts_chunk <- lapply(files, qread) %>%
          bind_rows() %>%
          filter(!is.na(cell_id)) %>%
          as.data.table() %>%
          group_by(cell_id, species) %>%
          summarise(impact_km2 = sum(impact_km2, na.rm = TRUE)) %>% # divide by 100 after this for prop and cap prop at 1
          ungroup() %>%
          mutate(prop_impact = impact_km2/100) %>%
          mutate(prop_impact = ifelse(prop_impact > 1, 1, prop_impact)) %>%
          group_by(cell_id) %>%
          summarise(prop_mean = mean(prop_impact,na.rm=TRUE),
                    prop_sd = sd(prop_impact, na.rm = TRUE),
                    prop_nspp = n_distinct(species)) %>%
          ungroup() %>%
          as.data.frame()
        
        
        all_impact_df <- rbind(all_impact_df, impacts_chunk)
        
        
        
      }
      rast_impact <- all_impact_df %>%
        dplyr::select(cell_id, prop_mean) %>%
        filter(!is.na(cell_id)) %>%
        left_join(moll_template_xy) %>%
        dplyr::select(x, y, prop_mean) %>%
        rast(., type = "xyz", crs = moll_template)
      
      writeRaster(rast_impact, glue(file.path(biodiv_dir, "int/marine_aggregation/impact_maps_across_taxon_ingredient/{diet}/{fcr}/{allocation}_mean_prop.tif")), overwrite = TRUE)
      
      rast_impact_sd <- all_impact_df %>%
        dplyr::select(cell_id, prop_sd) %>%
        filter(!is.na(cell_id)) %>%
        left_join(moll_template_xy) %>%
        dplyr::select(x, y, prop_sd) %>%
        rast(., type = "xyz", crs = moll_template)
      
      writeRaster(rast_impact_sd, glue(file.path(biodiv_dir, "int/marine_aggregation/impact_maps_across_taxon_ingredient/{diet}/{fcr}/{allocation}_sd_prop.tif")), overwrite = TRUE)
      
      
      rast_impact_nspp <- all_impact_df %>%
        dplyr::select(cell_id, prop_nspp) %>%
        filter(!is.na(cell_id)) %>%
        left_join(moll_template_xy) %>%
        dplyr::select(x, y, prop_nspp) %>%
        rast(., type = "xyz", crs = moll_template)
      
      writeRaster(rast_impact_nspp, glue(file.path(biodiv_dir, "int/marine_aggregation/impact_maps_across_taxon_ingredient/{diet}/{fcr}/{allocation}_nspp.tif")), overwrite = TRUE)
      
      
    }
  }
}  