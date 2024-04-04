##### Map terrestrial species to Mollweide from 1km by 1km

# # Summary
# 
# Some terrestrial species have breeding and non breeding AOH maps, and some of those maps have cells with overlap. To avoid double counting habitat area, we will combine any species maps that have non breeding and breeding areas into one map, and save a separate map with only the area for one cell each, regardless of it is breeding, resident, or nonbreeding. 
# Read in species with multiple maps, group by cell_id and sum the prescence value. If prescence value > 1, make it 1. Fin. 

## Data sources 
# * Eyres et al. 2024: Obtained through email communication with Alison Eyres (ae491@cam.ac.uk) and Michael Winston Dales (mwd24@cam.ac.uk) at Cambridge University. https://www.cl.cam.ac.uk/research/eeg/4c/data/aoh/ 


### SETUP
library(tidyverse)
library(tidyr)
library(here)
library(parallel)
library(strex)
library(janitor)
library(glue)

source(here("src/directories.R"))
source(here("src/spatial.R"))

eyres_dir <- file.path(rdsi_raw_data_dir, "AOH_eyres")
eyres_dir_mol <- file.path(eyres_dir, "reprojected_mol_csv")
eyres_dir_dup <- file.path(eyres_dir, "reprojected_mol_csv_fin")


spp_info <- data.frame(aoh_files = basename(list.files(file.path(rdsi_raw_data_dir, "AOH_eyres"), full.names = TRUE, recursive = TRUE))) %>%
  filter(str_detect(aoh_files, ".tif")) %>%
  pull(aoh_files) %>%
  str_remove_all('.tif$')
  

x <- list.files(eyres_dir_mol, full.names = TRUE)

spp_done <- basename(x) %>% str_remove_all('.csv$') 

spp_ids <- str_after_last(spp_done, "-") %>%
  unique() 


all_files <- list.files(eyres_dir_mol, full.names = TRUE)

map_terrestrial_to_moll <- function(s) {
 # s <- 20
    i <- which(spp_ids == s)
  
  # test <- rast(file.path(eyres_dir, "birds/Seasonality.BREEDING-22697109.tif"))
  
  out_f <- file.path(eyres_dir_dup, paste0(s, ".csv"))
  
#d  if(!file.exists(out_f)) {
    message('Processing map for ', s, '... (', i, ' of ', length(spp_ids), ')')
    
    spp_files <- grep(all_files, pattern = glue("-{s}.csv"), value = TRUE)
    
    if(length(spp_files) > 1){
    
    data_frame <- data.frame(presence = NA, cell_id = NA)
        
    for(file in spp_files){
      
      # file <- spp_files[1]
      df <- read.csv(file)
      
      data_frame <- rbind(data_frame, df)
    }

    
    spp_csv <- data_frame %>%
      filter(!is.na(cell_id)) %>%
      distinct(cell_id, presence)
    } else{
  
      spp_csv <- read.csv(spp_files[1])
}
  
      write_csv(spp_csv, out_f)
  
#  }
}

tmp <- parallel::mclapply(spp_ids, map_terrestrial_to_moll, mc.cores = 12)


