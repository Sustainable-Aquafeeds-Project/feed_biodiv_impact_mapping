AOH impacts folder organization

Summary:
In this folder we calculate the exposure and impact of aquafeeds on species AOH. 

## 01_download_AOH.Rmd

GOAL: Download all Area of Habitat data (AOH) for terrestrial species (mammals, birds, amphibians, and reptiles)

## 02_map_terrestrial_to_mol.R

GOAL: Reproject terrestrial AOH rasters to mollweide 10km by 10km rasters and save as csvs. 

## 02a_terrestrial_distinct_maps.R

GOAL: Save 1 AOH map for each species for AOH maps from Eyres et al. 2023. Some species have breeding and non-breeding grounds, which sometimes overlap in the habitat maps. To avoid double counting, we save a single raster for each species, regardless of breeding or non-breeding (just keep unique cells). 

## 03a_pull_terrestrial_hab_info

GOAL: Pull all habitat suitability information for terrestrial species. Adapted from Williams et al. 2021. This is not crop specific, but only include values for if species are sensitive to cropland as a whole or not.


## 03b_overlap_terrestrial_background

GOAL: This overlaps the terrestrial AOH maps with terrestrial crops grown for salmon aquafeeds. The result is files that have the amount of habitat impacted per cell per species per scenario. You can run this as a background job if you'd like, it takes ~3.5 hours to complete. 

## 04_terrestrial_overlap_summary

GOAL: Prep data frames of total global amount of impacted habitat per terrestrial species.

## 05a_prep_aquamaps_depths.Rmd

GOAL: Find depth information for all AquaMaps species. 

## 05b_map_aquamaps_to_moll.R

GOAL: Reproject marine AOH rasters to mollweide 10km by 10km rasters and save as csvs. Takes a really long time to run...

## 06a_marine_vuln.Rmd

GOAL: Assign and gapfill vulnerability values for each species and pressure combination. Each species is vulnerable to either bycatch, biomass removal, or both, depending upon the ingredient which is being fished. 

## 06b_overlap_marine_background.R

GOAL: Overlap [Aquamaps probability of suitable habitat maps](https://www.aquamaps.org/) with our disturbance pressure maps created in the `02_feed` folder, and multiply by their vulnerability value. The goal of this script is to create impact maps, that is, the area of likely suitable habitat (>0.6 probability) for each species that is exposed AND impacted to harvest of forage or trimmings fish that is ultimately processed into FMFO. This will take ~12 hours for all scenarios.

## 06c_overlap_fish_taxon.R

GOAL: In this script we had to split the larger "finfish" taxonomic grouping into two to be able to run that grouping on our server. This will probably take ~20 hours for all scenarios...

## 07_marine_overlap_summary.Rmd

GOAL: In this script we prep a data set of total global impacted habitat per marine species, ingredient/raw material, and allocation approach.


## 08_process_mean_sd_summary.Rmd

GOAL: In this script we summarize the species level impact rasters created in 03b, 06a and 06b by finding the mean, standard deviation, and number of species impacted. We do this across taxa and materials, by taxa and across material, and across taxa by materials. This will take a really long time to run all scenarios.


