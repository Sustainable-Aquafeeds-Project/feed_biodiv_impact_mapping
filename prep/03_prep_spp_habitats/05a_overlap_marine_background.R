
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

options(dplyr.summarise.inform = FALSE)
source(here("src/directories.R"))

source(here("src/spatial.R"))

aquamaps_dir <- file.path(rdsi_raw_data_dir, "aquamaps")


gall_peters <- "+proj=cea +lon_0=0 +x_0=0 +y_0=0 +lat_ts=45 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#raster template 
base_rast <- rast(res=0.5)
ext(base_rast) <- c(-180, 180, -90, 90)



rast_shallow <- rast(here("prep/03_prep_spp_habitats/data/spatial/bathy_mol_shallow.tif")) # taken from casey's project 
rast_neritic <- rast(here("prep/03_prep_spp_habitats/data/spatial/bathy_mol_neritic.tif"))
rast_bathy <- rast(here("prep/03_prep_spp_habitats/data/spatial/bathy_mol.tif"))


clip_to_depth <- function(spp_rast, spp_df) {
  ### depth clip if necessary; otherwise clip to bathy raster (which previously
  ### was clipped to area raster - so cells with any marine area will be kept,
  ### and non-marine cells will be dropped).
  ### Manual adds of shallow spp:
  
  max_depth <- unique(spp_df$DepthPrefMax)
  
  if(is.na(max_depth)){
    max_depth = 201
  }
  
  if(max_depth < 20) {
    ### intertidal, very shallow spp
    spp_rast <- mask(spp_rast, rast_shallow)
  } else if(max_depth < 200) {
    ### spp on the continental shelf
    spp_rast <- mask(spp_rast, rast_neritic)
  } else{
    ### rast_bathy covers the entire ocean - effectively masks out land
    spp_rast <- mask(spp_rast, rast_bathy)
  }
  
  return(spp_rast)
}

am_spp_depth_df <- read.csv(file.path(aquamaps_dir, "aquamaps_0.6_depth_prepped.csv"))

# Overlap all species AOH with fish feed ingredient rasters! This takes ~6.5 hours to run. 

forage_spp <- read.csv(here("data/raw_data/fisheries/forage_fish_list_final.csv")) %>%
  mutate(species = tolower(sci_name)) # ok so some of these are family level... will need to fix that eventually...

trimmings_spp <- read_csv(here("data/raw_data/biomar/trimmings_spp_list.csv")) %>% 
  mutate(species = tolower(scientific_name)) 

trim_spp_targeted <- am_spp_depth_df %>%
  filter(species %in% c(trimmings_spp$species)) %>%
  pull(SpeciesID) %>%
  unique()

forage_spp_targeted <- am_spp_depth_df %>%
  filter(species %in% c(forage_spp$species)) %>%
  pull(SpeciesID) %>%
  unique()

spp_names_not_targeted <- setdiff(unique(am_spp_depth_df$SpeciesID), c(forage_spp_targeted, trim_spp_targeted))
  
rm(trimmings_spp, forage_spp)
# spp_names <- spp_to_run # use this if rerunning based on setdiff in chunk below

## we need to split by species type
# trim_spp only affected by trimmings catch and forage bycatch
# forage spp only affected by trimmings bycatch and forage catch
# all other species only affected by bycatch for both

spp_type <- c("trim_species", "forage_species", "else_species")
# spp_type <- "else_species"

for(spp_class in spp_type){
  
  # spp_class = "else_species"

  if(spp_class == "trim_species"){
    
    spp_names = trim_spp_targeted
    
    # get files that affect trimmings spp (trimmings catch and forage bycatch)
ingredient_files <- c(
  list.files(here("prep/02_feed/output/resampled"), full.names = TRUE, recursive = TRUE, pattern = "trimmings fish_.*_catch_A\\.tif"),
  list.files(here("prep/02_feed/output/resampled"), full.names = TRUE, recursive = TRUE, pattern = "forage fish_.*_bycatch_A")
)


  }else if(spp_class == "forage_species"){
  
    spp_names = forage_spp_targeted
    
    # get files that affect forage spp (forage catch and trimming bycatch)
    ingredient_files <- c(
      list.files(here("prep/02_feed/output/resampled"), full.names = TRUE, recursive = TRUE, pattern = "trimmings fish_.*_bycatch_A\\.tif"),
      list.files(here("prep/02_feed/output/resampled"), full.names = TRUE, recursive = TRUE, pattern = "forage fish_.*_catch_A")
    )
    
  }else{
    
    # everything else affected by bycatch
    ingredient_files <- list.files(here("prep/02_feed/output/resampled"), full.names = TRUE, recursive = TRUE, pattern = "fish_.*bycatch_A")
    
    spp_names = spp_names_not_targeted
}

chunk_size = 112 # choosing this as a chunk size since it is a multiple of 14 (the number of cores I'm using). This way it will do exactly 112 species at a time

# 3921, 14897 aren't working

for(i in seq(1, length(spp_names), chunk_size)) {
  # i = 1
  
  chunk_spp_names <- spp_names[i:min(i + chunk_size -1, length(spp_names))]
  
  chunk_spp_names <- chunk_spp_names[!(chunk_spp_names %in% c("Fis-32191", "SLB-184666", "Fis-148871"))] # these two aren't working for some reason...     # Fis-32191 throwing this: Error: [rast] cannot create a raster geometry from a single y coordinate
  
  
  # Start timing
  start_time <- proc.time()

  
  chunk_marine_aoh <- mclapply(X = chunk_spp_names, 
                               FUN = \(this_spp){
    
    #isolate species
    #this spp id
    #   this_spp <- chunk_spp_names[[112]]
    # this_spp = "Fis-32191"
    # this_spp = "Fis-148871"
    # this_spp = "Fis-22721"
    
    ## ok so we are getting errors on some of these species because they don't have any AOH due to our >0.6 probability filter. We need to skip these somehow. 
    
    
    this_spp_name <- basename(this_spp)
    
    this_spp_df <- am_spp_depth_df %>%
      filter(SpeciesID == this_spp) %>% 
      dplyr::select(CenterLong, CenterLat, DepthPrefMax, depth_position) %>%
      mutate(presence = 1)
    
    if(nrow(this_spp_df) == 0){
      
      moll_template_0 <- setValues(moll_template, 0)
      
      this_spp_rast <- moll_template_0
      
      
    }else{
      
      this_spp_rast <- this_spp_df %>%
        dplyr::select(-DepthPrefMax, -depth_position) %>%
        rast(., type = "xyz", crs = crs(base_rast))  %>% 
        project(moll_template, method = "near")
      
      
      
      this_spp_rast <- clip_to_depth(this_spp_rast, this_spp_df)
      
      
    }
    
    aoh_orig <- global(this_spp_rast*102.6423, "sum", na.rm = TRUE)$sum
    
    
    
    overlap_list <- map(ingredient_files, \(this_ingredient){
      
      # this_ingredient = ingredient_files[2]
      
      this_ingredient_name <- str_before_nth(str_before_first(str_after_last(this_ingredient, "/"), ".tif"), "_", 2)
      
      this_allocation_type <- str_before_last(str_before_last(str_before_last(str_after_nth(str_after_last(this_ingredient, "/"), "_",  2), "_"), "_"), "_")
      
      this_diet_type <- str_before_last(str_after_nth(this_ingredient, "/", 10), "/")
      
      this_ingredient_raster <- terra::rast(this_ingredient)
      
      this_depth_position_spp <- unique(this_spp_df$depth_position)
      
      this_depth_position_disturbance <- str_after_last(str_before_last(str_before_last(str_after_nth(str_after_last(this_ingredient, "/"), "_",  2), "_"), "_"), "_")
      
      if(this_depth_position_spp %in% c("benthopelagic", "reef")){
        this_depth_position_spp_2 = this_depth_position_disturbance
      }else{
        this_depth_position_spp_2 = this_depth_position_spp
      }
      
      
      if(this_depth_position_spp == this_depth_position_disturbance | this_depth_position_spp_2 == this_depth_position_disturbance){
        
        # spp_overlap_rast_df <- as.data.frame(this_ingredient_raster*this_spp_rast, xy = TRUE) %>%
        #   rename(aoh_overlap = 3) %>%
        #   filter(aoh_overlap > 0) %>%
        #   mutate(          sciname = this_spp_name,
        #                    allocation = this_allocation_type,
        #                    ingredient = this_ingredient_name, 
        #                    diet = this_diet_type, 
        #                    spp_depth_pos = this_depth_position_spp,
        #                    dist_depth_pos = this_depth_position_disturbance,
        #                    aoh_area_orig = aoh_orig
        #  ) 
        
        ## could i just make this xyz csv, filter out any AOH == 0, and save that in chunks? Instead of saving >1 million rasters. Would those csvs be too large for R? 
       # writeRaster(spp_overlap_rast, sprintf(file.path(aquamaps_dir, "spp_overlap/%s/%s_%s_%s_%s.tif"), this_diet_type, this_ingredient_name, this_allocation_type, this_depth_position_disturbance, this_spp_name), overwrite = TRUE) # save a raster version
        
    spp_overlap_rast <- this_ingredient_raster*this_spp_rast
    
        spp_overlap_df <- data.frame(
          sciname = this_spp_name,
          allocation = this_allocation_type,
          ingredient = this_ingredient_name,
          diet = this_diet_type,
          spp_depth_pos = this_depth_position_spp,
          dist_depth_pos = this_depth_position_disturbance,
          aoh_overlap = global(spp_overlap_rast, "sum", na.rm = TRUE)$sum,
          aoh_area_orig = aoh_orig
        )
        
      }else{
        
        # spp_overlap_rast_df <- data.frame(
        #   sciname = this_spp_name,
        #   allocation = this_allocation_type,
        #   ingredient = this_ingredient_name, 
        #   diet = this_diet_type, 
        #   spp_depth_pos = this_depth_position_spp,
        #   dist_depth_pos = this_depth_position_disturbance, 
        #   aoh_overlap = 0,
        #   aoh_area_orig = aoh_orig
        # ) %>%
        #   mutate(x = NA, y = NA)
        
        spp_overlap_df <- data.frame(
          sciname = this_spp_name,
          allocation = this_allocation_type,
          ingredient = this_ingredient_name,
          diet = this_diet_type,
          spp_depth_pos = this_depth_position_spp,
          dist_depth_pos = this_depth_position_disturbance,
          aoh_overlap = 0,
          aoh_area_orig = aoh_orig
        )
      } 
      
    })
    
    return(bind_rows(overlap_list))
    
  },  mc.cores = detectCores()-4)
  
  
  full_df <- bind_rows(chunk_marine_aoh) %>% as_tibble() 

  
  saveRDS(full_df, here(sprintf("prep/03_prep_spp_habitats/int/aoh_overlay_chunks_marine/%s/full_df_chunk_targeted_%s.rds", spp_class, i)))
  
  # saveRDS(full_df, here(sprintf("prep/03_prep_spp_habitats/int/aoh_overlay_chunks_marine/retry_spp/full_df_chunk_%s.rds", i))) ### Use this if rerunning species based on setdiff in chunk below
  
  # End timing
  end_time <- proc.time()
  
  # Calculate the elapsed time
  elapsed_time <- end_time - start_time
  
  # Print the elapsed time
  print(elapsed_time)
  
  print(paste("chunk ", i, " out of 23444 finished"))
  
}
}