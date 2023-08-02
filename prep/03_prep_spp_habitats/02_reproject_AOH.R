###### This script is to reproject all AOH rasters to 10km by 10km mollweide rasters. This takes ~14 hours to complete. Only run when absolutely necessary. I ran as a background job so that I could work on other things. 

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

aoh_files <- list.files(file.path(rdsi_raw_data_dir, "AOH_lumbierres"), full.names = TRUE)

chunk_size = 112

for(i in seq(1, length(aoh_files), chunk_size)) {
  # i = 1
  
  chunk_aoh_files <- aoh_files[i:min(i + chunk_size -1, length(aoh_files))]
  
  # Start timing
  start_time <- proc.time()
  
  chunk_terrestrial_aoh <- mclapply(X = chunk_aoh_files, FUN = \(this_spp){
    
    #isolate species
    #this spp id
    # this_spp <- chunk_aoh_files[[1]]
    
    this_spp_name <- basename(tools::file_path_sans_ext(this_spp))
    
    rast(this_spp) %>%
      project(moll_template, method = "near") %>%
      writeRaster(., sprintf(file.path(rdsi_raw_data_dir, "AOH_lumbierres/reprojected/%s.tif"), this_spp_name), overwrite = TRUE)
    
  
   min_value = global(rast(sprintf(file.path(rdsi_raw_data_dir, "AOH_lumbierres/reprojected/%s.tif"), this_spp_name)), "min", na.rm = TRUE)
    
    if(is.na(min_value)){
      
     rast(this_spp) %>%
        aggregate(fact = 10, fun = "mean", na.rm = TRUE) %>%  
        project(moll_template, method = "near") %>%
        writeRaster(., sprintf(file.path(rdsi_raw_data_dir, "AOH_lumbierres/reprojected/%s.tif"), this_spp_name), overwrite = TRUE)
      
    }
  },  mc.cores = detectCores()-2)
  
  
  # End timing
  end_time <- proc.time()
  
  # Calculate the elapsed time
  elapsed_time <- end_time - start_time
  
  # Print the elapsed time
  print(elapsed_time)
  
  print(paste("chunk ", i, " out of 18565 finished"))
  
}
