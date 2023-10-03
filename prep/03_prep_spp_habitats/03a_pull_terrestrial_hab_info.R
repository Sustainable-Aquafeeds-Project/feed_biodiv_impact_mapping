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


species_list <- readRDS(here("prep/03_prep_spp_habitats/int/terrestrial_species_list.rds")) %>%
  pull(binomial) %>%
  unique()

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



## Estimate species' tolerance for croplands

data_list <- lapply(list.files(here("prep/03_prep_spp_habitats/int/habitat_suitability_chunks/"), full.names = TRUE), readRDS)

hab_suitability_df <- bind_rows(data_list) %>%
  filter(!is.na(name)) %>%
  mutate(suitable = ifelse(suitability == "Suitable",
                           yes = 1, no = 0),
         marginal = ifelse(suitability == "Marginal",
                           yes = 0.5, no = 0))

mammals_list <- read.csv(here("prep/data/Mammals_list_AOH.csv"))

birds_list <- read.csv(here("prep/data/Birds_list_AOH.csv"))


# Estimating species' tolerance for agricultural lands----
cropland_habitats <- c("Artificial/Terrestrial - Arable Land",
                       "Artificial/Terrestrial - Plantations")

ag_suitability <- hab_suitability_df %>%
  mutate(is_ag = ifelse(habitat %in% c(cropland_habitats),
                        yes = "cropland_suitability",
                        no = "not_ag"),
         is_ag = factor(is_ag),
         species = factor(name)) %>%
  dplyr::select(-suitability, -majorimportance, -name) %>%
  gather(tmp, suitability_value, 
         -c(code:season, is_ag, species)) %>%
  select(-tmp) %>%
  complete(species, is_ag, fill = list(suitability_value = 0)) %>%
  filter(is_ag != "not_ag") %>%
  group_by(species, is_ag) %>%
  summarise(highest_suitability = max(suitability_value, na.rm = TRUE)) %>%
  spread(is_ag, highest_suitability) %>%
  ungroup() %>%
  mutate(spp_type = case_when(
    species %in% c(mammals_list$BINOMIAL) ~ "Terrestrial mammal",
    species %in% c(birds_list$BINOMIAL) ~ "Bird")) %>%
  mutate(spp_type = ifelse(species == "Murina balaensis", "Terrestrial mammal", spp_type)) %>%
  mutate(spp_type = ifelse(species == "Ardea Cinerea", "Bird", spp_type))
  
write_rds(ag_suitability, here("prep/03_prep_spp_habitats/int/terrestrial_spp_habitat_suitability.rds"))
