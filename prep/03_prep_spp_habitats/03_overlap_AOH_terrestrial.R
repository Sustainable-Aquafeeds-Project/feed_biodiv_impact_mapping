library(tidyverse)
library(here)
library(data.table)
library(dtplyr)
library(terra)
library(raster)
library(parallel)
library(rnaturalearth)
library(strex)
library(janitor)

source(here("src/directories.R"))

source(here("src/spatial.R"))

select <- dplyr::select


# Overlap all species AOH with crop feed ingredient rasters! This takes ~6.5 hours to run. 

aoh_files <- list.files(file.path(rdsi_raw_data_dir, "AOH_lumbierres/reprojected"), full.names = TRUE)

ingredient_files <- list.files(here("prep/02_feed/output/resampled"), full.names = TRUE, recursive = TRUE, pattern = "_A")
ingredient_files <- ingredient_files[!grepl( "fish meal|fish oil", ingredient_files)]

chunk_size = 112 # choosing this as a chunk size since it is a multiple of 14 (the number of cores I'm using). This way it will do exactly 112 species at a time

## 1569 didn't work 

for(i in seq(1681, length(aoh_files), chunk_size)) {
  # i = 1569
  
  chunk_aoh_files <- aoh_files[i:min(i + chunk_size -1, length(aoh_files))]
  
  # Start timing
  start_time <- proc.time()
  
  chunk_terrestrial_aoh <- mclapply(X = chunk_aoh_files, FUN = \(this_spp){
    
    #isolate species
    #this spp id
    # this_spp <- chunk_aoh_files[[1]]
    
    this_spp_name <- basename(tools::file_path_sans_ext(this_spp))
    
    this_spp_rast <- rast(this_spp)
    
    aoh_orig <- global(this_spp_rast*102.6423, "sum", na.rm = TRUE)$sum
    
    
    overlap_list <- map(ingredient_files, \(this_crop_ingredient){
      
      # this_crop_ingredient = ingredient_files[5]
      
      this_crop_ingredient_name <- str_before_nth(str_before_first(str_after_last(this_crop_ingredient, "/"), ".tif"), "_", 2)
      
      this_allocation_type <- str_before_last(str_after_nth(str_after_last(this_crop_ingredient, "/"), "_",  2), "_")
      
      this_diet_type <- str_before_last(str_after_nth(this_crop_ingredient, "/", 9), "/")
      
      # message("processing terrestrial spp impacts in ", this_crop_ingredient_name, " for ", this_allocation_type, " for ", this_diet_type)
      
      this_crop_ingredient_raster <- terra::rast(this_crop_ingredient)
      
      
      #inner join species and crop files so we know if they overlap and summarise the crop portion of spp habitat
      spp_crop_overlap_rast <- this_crop_ingredient_raster*this_spp_rast
      
      spp_overlap_df <- data.frame(
        sciname = this_spp_name,
        allocation = this_allocation_type,
        ingredient = this_crop_ingredient_name, 
        diet = this_diet_type, 
        aoh_overlap = global(spp_crop_overlap_rast, "sum", na.rm = TRUE)$sum,
        aoh_area_orig = aoh_orig
      )
      
    })
    
    return(bind_rows(overlap_list))
    
  },  mc.cores = detectCores()-2)
  
  
  full_df <- bind_rows(chunk_terrestrial_aoh) %>% as_tibble()
  
  saveRDS(full_df, here(sprintf("prep/03_prep_spp_habitats/int/aoh_overlay_chunks_terrestrial/full_df_chunk_%s.rds", i)))
  
  # End timing
  end_time <- proc.time()
  
  # Calculate the elapsed time
  elapsed_time <- end_time - start_time
  
  # Print the elapsed time
  print(elapsed_time)
  
  print(paste("chunk ", i, " out of 18558 finished"))
  
}