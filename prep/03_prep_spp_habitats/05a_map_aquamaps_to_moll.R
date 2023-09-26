### SETUP
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
library(tidyterra)

source(here("src/directories.R"))

source(here("src/spatial.R"))

aquamaps_dir <- file.path(rdsi_raw_data_dir, "aquamaps")
am_dir_mol <- file.path(aquamaps_dir, "reprojected_mol")


gall_peters <- "+proj=cea +lon_0=0 +x_0=0 +y_0=0 +lat_ts=45 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
#raster template 
base_rast <- rast(res=0.5)
ext(base_rast) <- c(-180, 180, -90, 90)


### create cell id lookup
loiczid_mol_r <- rast(here('prep/03_prep_spp_habitats/data/spatial/loiczid_mol.tif')) %>%
  project(moll_template, method = "near")
ocean_mol_r <- rast(here('prep/03_prep_spp_habitats/data/spatial/ocean_area_mol.tif')) %>%
  project(moll_template, method = "near")
bathy_mol_neritic <- rast(here('prep/03_prep_spp_habitats/data/spatial/bathy_mol_neritic.tif'))
bathy_mol_shallow <- rast(here('prep/03_prep_spp_habitats/data/spatial/bathy_mol_shallow.tif'))
test <- rast(here('prep/03_prep_spp_habitats/data/spatial/bathy_mol_neritic_test.tif'))

cell_id_df <- data.frame(loiczid = values(loiczid_mol_r),
                         ocean_a = values(ocean_mol_r),
                         neritic = values(bathy_mol_neritic),
                         shallow = values(bathy_mol_shallow),
                         cell_id = 1:ncell(moll_template)) %>%
  rename(loiczid = loiczid_mol, ocean_a = ocean_area_mol, neritic = bathy_mol_neritic, shallow = bathy_mol_shallow) %>%
  filter(!is.na(ocean_a))

moll_template_xy <- as.data.frame(moll_template %>% mutate(value = 1), xy = TRUE) %>%
  mutate(cell_id = 1:ncell(moll_template)) %>%
 # dplyr::select(x,y,cell_id) %>%
  left_join(cell_id_df) %>%
  filter(!is.na(ocean_a)) %>%
  dplyr::select(x, y, cell_id)

# write_rds(moll_template_xy, here("prep/03_prep_spp_habitats/data/spatial/moll_template_xy.rds"))


### read in aquamaps data

hcaf_info <- read.csv(file.path(aquamaps_dir, 'hcaf_v7.csv')) %>%
  mutate(across(where(is.numeric), ~ ifelse(.x == -9999, NA, .x))) %>%
  clean_names() %>%
  dplyr::select(loiczid, csquare_code) %>%
  distinct()

am_spp_depth <- read.csv(file.path(aquamaps_dir, "aquamaps_0.6_depth_prepped.csv")) %>%
  left_join(hcaf_info, by = c("CsquareCode" = "csquare_code"))


### Create flags for how to mask the aquamaps data
am_spp_info <- am_spp_depth %>%
  mutate(
         mask   = case_when(str_detect(depth_position, 'pelagic') ~ 'none', ### don't clip pelagics
                            is.na(depth_position)                 ~ 'none', ### don't clip unknowns
                            DepthPrefMax <=  60 ~ 'shallow',
                            DepthPrefMax <= 200 ~ 'neritic',
                            TRUE       ~ 'none')) %>%
  mutate(mask = factor(mask, levels = c('shallow', 'neritic', 'none'), ordered = TRUE)) %>%
  dplyr::select(SpeciesID, species, mask) %>%
  distinct()

x <- list.files(am_dir_mol, full.names = TRUE)

spp_done <- basename(x) %>% str_remove_all('csv$') %>%
  str_replace_all('[^a-z]+', ' ')

spp_vec <- am_spp_info %>%
  filter(!is.na(species)) %>%
  filter(!species %in% spp_done) %>%
  .$species %>% unique() %>% sort()



map_am_hcaf_to_moll <- function(s) {
  # s <- spp_vec[1]
  i <- which(spp_vec == s)
  
  out_f <- file.path(am_dir_mol, paste0(str_replace_all(s, ' ', '_'), ".csv"))
  
  if(!file.exists(out_f)) {
    message('Processing map for ', s, '... (', i, ' of ', length(spp_vec), ')')
    
    ### identify the species ID(s) for this species
    s_sids <- am_spp_info %>% 
      filter(species == s) %>%
      .$SpeciesID %>% unique()
    
    ### identify the depth(s); if multiple, choose the least restrictive
    s_depth <- am_spp_info %>% 
      filter(species == s) %>%
      filter(mask == max(mask)) %>%
      .$mask %>% unique()
    
    ### identify the cells for am_sid(s) and join to Mollweide cells
    s_cells <- am_spp_depth %>%
      filter(SpeciesID %in% s_sids)
    
    s_mol <- s_cells %>%
      left_join(cell_id_df, by = c('loiczid'))
    
    ### filter to proper depth category
    if(s_depth == 'shallow') {
      message('... clipping ', s, ' to shallow waters only')
      s_mol <- s_mol %>%
        filter(!is.na(shallow))
    } else if(s_depth == 'neritic') {
      message('... clipping ', s, ' to neritic waters only')
      s_mol <- s_mol %>%
        filter(!is.na(neritic))
    } ### else, 'none', so no bathymetric filter
    
    s_mol <- s_mol %>%
      dplyr::select(prob = Probability, cell_id) %>%
      mutate(presence = 1)
    
    # s_mol_rast <- s_mol %>%
    #   left_join(moll_template_xy) %>%
    #   dplyr::select(x, y, prob) %>%
    #   rast(., type = "xyz", crs = crs(moll_template))
    # 
    # plot(s_mol_rast)

    
    write_csv(s_mol, out_f)
  }
}

tmp <- parallel::mclapply(spp_vec, map_am_hcaf_to_moll, mc.cores = 12)


