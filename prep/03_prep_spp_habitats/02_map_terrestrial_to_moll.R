##### Map terrestrial species to Mollweide from 1km by 1km

# # Summary
# 
# Convert 1kmx1kms maps from Lumbierres et al. into the Mollweide 10 km x 10 km projection. This takes ~ 7 hours to complete, so I ran as a background job. 
# 
# # Methods
# 
# For each species, reproject to mollweide and save as a csv with just the new Mollweide ID cell values, per species.
# 

### SETUP
library(tidyverse)
library(tidyr)
library(here)
library(terra)
library(parallel)
library(strex)
library(janitor)
library(tidyterra)

source(here("src/directories.R"))

source(here("src/spatial.R"))

lumbierres_dir <- file.path(rdsi_raw_data_dir, "AOH_lumbierres")
lumbierres_dir_mol <- file.path(lumbierres_dir, "reprojected_mol_csv")


### create cell id lookup

moll_template_xy_land <- readRDS(here("prep/03_prep_spp_habitats/data/spatial/moll_template_land_xy.rds"))

spp_info <- data.frame(aoh_files = basename(list.files(file.path(rdsi_raw_data_dir, "AOH_lumbierres"), full.names = TRUE))) %>%
  filter(str_detect(aoh_files, ".tif")) %>%
  pull(aoh_files) %>%
  str_remove_all('.tif$')
  

x <- list.files(lumbierres_dir_mol, full.names = TRUE)

spp_done <- basename(x) %>% str_remove_all('.csv$')

spp_vec <- setdiff(spp_info, spp_done) %>% unique() %>% sort()



map_terrestrial_to_moll <- function(s) {
  # s <- spp_vec[1]
  i <- which(spp_vec == s)
  
  out_f <- file.path(lumbierres_dir_mol, paste0(s, ".csv"))
  
  if(!file.exists(out_f)) {
    message('Processing map for ', s, '... (', i, ' of ', length(spp_vec), ')')
    
    spp_file <- sprintf(file.path(lumbierres_dir, "%s.tif"), s)
    
  spp_csv <- rast(spp_file) %>%
      project(moll_template, method = "near") %>%
      as.data.frame(., xy = TRUE) %>%
     rename(presence = 3) %>%
     inner_join(moll_template_xy_land) %>%
     dplyr::select(-x, -y)
  
  # test <- spp_csv %>%
  #   left_join(moll_template_xy_land) %>%
  #   dplyr::select(x, y, presence) %>%
  #   rast(., type = "xyz", crs = moll_template)
  
    
    if(nrow(spp_csv) == 0){
      
      rast(spp_file) %>%
        aggregate(fact = 10, fun = "mean", na.rm = TRUE) %>%  
        as.data.frame(., xy = TRUE) %>%
        rename(presence = 3) %>%
        inner_join(moll_template_xy_land) %>%
        dplyr::select(-x, -y) %>%
        write_csv(., out_f)
    }else{
      write_csv(spp_csv, out_f)
    }

  
    
  }
}

tmp <- parallel::mclapply(spp_vec, map_terrestrial_to_moll, mc.cores = 12)


