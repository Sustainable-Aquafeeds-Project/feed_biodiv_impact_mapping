# IUCN API query functions

library(rredlist)
library(here)
library(terra)
library(tidyverse)
library(parallel)
library(data.table)


source(here("src/directories.R"))


api_file <- file.path(iucn_dir, "api_key", "api_token.txt")
api_key <- scan(api_file, what = 'character')

#api_version <- rl_version(key=api_key)
api_version <-  "2021-3" #set the version


get_spp_api <- function(this_page){
  spplist_url <- sprintf("https://apiv3.iucnredlist.org/api/v3/species/page/%s?token=%s", this_page,  api_key)
  jsonlite::fromJSON(spplist_url) %>% .$result
}


get_habitat_api <- function(this_spp_id){
  message(paste("processing taxonid #", this_spp_id))
  hablist_url <- sprintf("https://apiv3.iucnredlist.org/api/v3/habitats/species/id/%s?token=%s", this_spp_id,  api_key)
  this_result <-jsonlite::fromJSON(hablist_url)
  return(tibble(this_result %>% .$result) %>% mutate(spp_id = this_result %>% .$id))
}


get_threat_api <- function(this_spp_id){
  message(paste("processing taxonid #", this_spp_id))
  threat_url <- sprintf("https://apiv3.iucnredlist.org/api/v3/threats/species/id/%s?token=%s", this_spp_id,  api_key)
  this_result <- jsonlite::fromJSON(threat_url) %>% data.frame(stringsAsFactors = FALSE)
  return(this_result)
  
}



## Common functions+packages
select <- dplyr::select
summarise <- dplyr::summarise


### Helper functions for gathering species rangemaps
check_tryerror <- function(l) {
  x <- sapply(l, class) %>% 
    unlist() %>% as.vector()
  return(any(stringr::str_detect(tolower(x), 'error')))
} 

get_one_map <- function(f) {
  #f = "/mnt/rdsi/raw_data/aquamaps/reprojected_mol_csv/diomedea_sanfordi.csv"  
  if(file.exists(f)) {
    df <- read.csv(f)
    
      df <- df %>% filter(presence == 1) %>% dplyr::select(cell_id)

    return(df)
  } else {
    warning('No map found for ', basename(f))
    return(NULL)
  }
}


collect_spp_rangemaps_terrestrial <- function(spp_vec, file_vec, idcol = 'species_full', parallel = TRUE) {
 # spp_vec = unique(tx_maps_df$species)
  # file_vec = unique(tx_maps_df$filepath)
  ### give a vector of species names (or IDs) and filenames; 
  ### default: read in using parallel::mclapply
  message('Collecting ', n_distinct(file_vec), ' maps for ', 
          n_distinct(spp_vec), ' species...')
  if(parallel == TRUE) {
    out_maps_list <- parallel::mclapply(file_vec, mc.cores = 12, FUN = get_one_map)
  } else {
    out_maps_list <- lapply(file_vec, FUN = get_one_map)
  }
  if(check_tryerror(out_maps_list)) {
    stop('Try-error found when assembling species rangemaps!')
  }
  
  
  message('... Binding maps...')

  out_maps_df <- out_maps_list %>%
    setNames(spp_vec) %>%
    purrr::compact() %>%
    data.table::rbindlist(idcol = "species_full") %>%
    distinct() 
  
  return(out_maps_df)
}

collect_spp_rangemaps_marine <- function(spp_vec, file_vec, idcol = 'species', parallel = TRUE) {
  # spp_vec = unique(tx_maps_df$species)
  # file_vec = unique(tx_maps_df$filepath)
  ### give a vector of species names (or IDs) and filenames; 
  ### default: read in using parallel::mclapply
  message('Collecting ', n_distinct(file_vec), ' maps for ', 
          n_distinct(spp_vec), ' species...')
  if(parallel == TRUE) {
    out_maps_list <- parallel::mclapply(file_vec, mc.cores = 12, FUN = get_one_map)
  } else {
    out_maps_list <- lapply(file_vec, FUN = get_one_map)
  }
  if(check_tryerror(out_maps_list)) {
    stop('Try-error found when assembling species rangemaps!')
  }
  
  
  message('... Binding maps...')
  
  out_maps_df <- out_maps_list %>%
    setNames(spp_vec) %>%
    purrr::compact() %>%
    data.table::rbindlist(idcol = "species") %>%
    distinct() 
  
  return(out_maps_df)
}




pooled_var <- function(x_bar, y_bar, s_x, s_y, n_x, n_y) {
  ### convert std dev to var
  var_x <- ifelse(is.na(s_x), 0, s_x^2)
  var_y <- ifelse(is.na(s_y), 0, s_y^2)
  
  var_xy_clean <- ((n_x - 1)*var_x + (n_y - 1)*var_y) / (n_x + n_y - 1)
  var_xy_error <- (n_x * n_y) * (x_bar - y_bar)^2 / ((n_x + n_y)*(n_x + n_y - 1))
  
  return(var_xy_clean + var_xy_error)
}

iterated_pooled_var <- function(mean_vec, sdev_vec, n_vec, flag = FALSE) {
  if(!all.equal(length(mean_vec), length(sdev_vec), length(n_vec))) {
    stop('Mean, std dev, and n vectors must all be equal length!')
  }
  if(length(mean_vec) == 1) {
    warning('Only one element - no need for pooled variance!')
    return(sdev_vec[1]^2)
  }
  ### initialize values for first in list
  mean_x <- mean_vec[1]; s_x <- sdev_vec[1]; n_x <- n_vec[1]
  for(i in 2:length(mean_vec)) { ## i <- 2
    if(flag) message('  ... processing iteration ', i - 1, '...')
    
    mean_y <- mean_vec[i]
    s_y    <- sdev_vec[i]
    n_y    <- n_vec[i]
    var_out <- pooled_var(x_bar = mean_x, y_bar = mean_y, 
                          n_x = n_x, n_y = n_y, 
                          s_x = s_x, s_y = s_y)
    
    ### set up values for next iteration
    mean_x <- (mean_x * n_x + mean_y * n_y) / (n_x + n_y)
    s_x <- sqrt(var_out)
    n_x <- n_x + n_y
  }
  return(var_out)
}
