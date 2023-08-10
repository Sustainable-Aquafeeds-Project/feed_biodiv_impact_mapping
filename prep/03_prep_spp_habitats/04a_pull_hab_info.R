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
library(rredlist)

source(here("src/directories.R"))


species_list <- readLines(here("prep/03_prep_spp_habitats/int/species_list.txt"))

api_version <- '2022-2'

api_key = read.csv(file.path(rdsi_raw_data_dir, "iucn/api_key/api_token.txt"))
api_key = colnames(api_key)


num_chunks <- 16133 / 112  # Number of chunks
species_chunks <- split(species_list, rep(1:num_chunks, each = 112, length.out = length(species_list)))


pull_habitats <- function(species_chunk, chunk_number) {
  
  chunk_filename <- sprintf(here("prep/03_prep_spp_habitats/int/habitat_suitability_chunks/full_df_chunk_%s.rds"), chunk_number)
  
  # Check if the chunk file already exists
  if (file.exists(chunk_filename)) {
    message(paste("Skipping chunk", chunk_number, "as the file already exists."))
    habitat_df <- readRDS(chunk_filename)
    return(habitat_df)
  }
  
  
  # Replace 'rl_habitats' with the actual function to pull species habitat information
  habitat_data <- lapply(species_chunk, rl_habitats, key = api_key)
  
  species_data <- list()
  
  # Loop through the species list and extract the required information
  for (species in habitat_data) {
    if (length(species$result) > 0) {
      # If the result is available, use it
      species_data[[length(species_data) + 1]] <- data.frame(
        name = species$name,
        code = species$result$code,
        habitat = species$result$habitat,
        suitability = species$result$suitability,
        season = species$result$season,
        majorimportance = species$result$majorimportance
      )
    } else {
      # If the result is not available, create empty data with NAs
      species_data[[length(species_data) + 1]] <- data.frame(
        name = species$name,
        code = NA,
        habitat = NA,
        suitability = NA,
        season = NA,
        majorimportance = NA
      )
    }
  }
  
  
  # Do any data manipulation if needed (e.g., binding rows)
  habitat_df <- do.call(rbind, species_data)
  
  # Save the dataframe as an .rds file
  saveRDS(habitat_df, file = chunk_filename)
  
  return(habitat_df)
}


# Number of cores to use
num_cores <- detectCores() - 2

# Initialize a parallel backend with the specified number of cores
cl <- makeCluster(num_cores)

# Use 'mclapply' to run 'pull_habitats' function in parallel for each chunk
results <- mclapply(seq_along(species_chunks), function(i) {
  pull_habitats(species_chunks[[i]], i)
}, mc.cores = num_cores)

# Stop the parallel backend
stopCluster(cl)
