##### Reproject terrestrial species AOH maps to Mollweide from 1km by 1km

# # Summary
# 
# Convert 1 km x 1 km maps from Eyres et al. into the Mollweide 10 km x 10 km projection. This takes a long time to run... run overnight if possible. 
# 
# # Methods
# 
# For each species, reproject to mollweide and save as a csv with just the new Mollweide ID cell values, per species.

## Data sources 
# * Eyres et al. 2024: Obtained through email communication with Alison Eyres (ae491@cam.ac.uk) and Michael Winston Dales (mwd24@cam.ac.uk) at Cambridge University. https://www.cl.cam.ac.uk/research/eeg/4c/data/aoh/ 


### SETUP
library(tidyverse)
library(tidyr)
library(here)
library(raster)
library(terra)
library(parallel)
library(strex)
library(janitor)
library(tidyterra)
library(glue)

source(here("src/directories.R"))
source(here("src/spatial.R"))

eyres_dir <- file.path(rdsi_raw_data_dir, "AOH_eyres")
eyres_dir_mol <- file.path(eyres_dir, "reprojected_mol_csv")


### create cell id lookup
moll_template_xy_land <- readRDS(here("prep/03_prep_spp_habitats/data/spatial/moll_template_land_xy.rds"))
moll_template_raster <- raster(moll_template)


## Get list of species to loop through
spp_info <- data.frame(aoh_files = basename(list.files(file.path(rdsi_raw_data_dir, "AOH_eyres"), full.names = TRUE, recursive = TRUE))) %>%
  filter(str_detect(aoh_files, ".tif")) %>%
  pull(aoh_files) %>%
  str_remove_all('.tif$')
  

x <- list.files(eyres_dir_mol, full.names = TRUE)

spp_done <- basename(x) %>% str_remove_all('.csv$')

spp_vec <- setdiff(spp_info, spp_done) %>% unique() %>% sort()

# spp_vec <- spp_done

# Specify the directories
directories <- grep(list.files(eyres_dir, full.names = TRUE), pattern = "reprojected", value = TRUE, invert = TRUE) # invert = TRUE means we want only NON REPROJECTED FILES

# Create a list to store the file names
file_list <- list()

# Loop through each directory (since there are 4) and get the file names for birds, mammals, reptiles, and amphibians
for (dir in directories) {
  files <- list.files(path = dir, full.names = TRUE)
  file_list[[dir]] <- files
}

# Flatten the list
all_files <- unlist(file_list)

map_terrestrial_to_moll <- function(s) {
  # s <- spp_vec[20] # test one file if you like 
    i <- which(spp_vec == s)
  
  # test <- rast(file.path(eyres_dir, "birds/Seasonality.BREEDING-22697109.tif")) # just taking a look 
  
  out_f <- file.path(eyres_dir_mol, paste0(s, ".csv"))
  
  if(!file.exists(out_f)) {
    message('Processing map for ', s, '... (', i, ' of ', length(spp_vec), ')')
    
    spp_file <- grep(all_files, pattern = glue("{s}.tif"), value = TRUE)

  spp_csv <- # rast(spp_file) %>% 
    raster::raster(spp_file) %>% # using raster instead of terra here because it was working faster at the time of analysis
   raster::projectRaster(., moll_template_raster, method = "ngb") %>%
   rast() %>%
    # project(., moll_template, method = "near") %>%
      as.data.frame(., xy = TRUE) %>%
     rename(presence = 3) %>%
    filter(presence > 0) %>%
    mutate(presence = 1) %>%
    inner_join(moll_template_xy) %>%
     dplyr::select(-x, -y)

  # test <- spp_csv %>%
  # left_join(moll_template_xy) %>%
  # dplyr::select(x, y, presence) %>%
  # rast(., type = "xyz", crs = moll_template) # just some checking to see it worked
  
    if(nrow(spp_csv) == 0){
      
      
      ## switched to using raster package because it was much faster than terra for aggregating this for some reason
    rast(spp_file) %>%
        aggregate(fact = 10, fun = "mean", na.rm = TRUE) %>%  
        raster() %>%
        projectRaster(., moll_template, method = "ngb") %>% 
        rast() %>%
        as.data.frame(., xy = TRUE) %>%
        rename(presence = 3) %>%
        filter(presence >0) %>%
        mutate(presence = 1) %>%
        inner_join(moll_template_xy) %>%
        dplyr::select(-x, -y) %>%
        write_csv(., out_f)
      
  
      
    }else{
      write_csv(spp_csv, out_f) # save the reprojected file
    }

  
    
  }
}

tmp <- parallel::mclapply(spp_vec, map_terrestrial_to_moll, mc.cores = 12) # run this with 12 cores (or however many you can handle)


