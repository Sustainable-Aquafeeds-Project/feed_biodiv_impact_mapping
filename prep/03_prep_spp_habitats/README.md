AOH impacts folder organization

## 01_download_AOH.Rmd

GOAL: Download all Area of Habitat data (AOH) for terrestrial mammals and birds

## 02_map_terrestrial_to_mol.R

GOAL: Reproject terrestrial AOH rasters to mollweide 10km by 10km rasters and save as csvs. 

## 02a_terrestrial_distinct_maps.R

GOAL: Save 1 AOH map for each species for AOH maps from Eyres et al. 2023. Some species have breeding and non-breeding grounds, which sometimes overlap in the habitat maps. To avoid double counting, we save a single raster for each species, regardless of breeding or non-breeding (just keep unique cells). 

## 03a_pull_terrestrial_hab_info

GOAL: Pull all terrestrial habitat suitability information for species. Adapted from Williams et al. 2021.


## 03b_overlap_terrestrial_background

GOAL: This overlaps the terrestrial AOH maps with terrestrial crops grown for salmon aquafeeds. The result are rasters which describe for larger taxonomic group, the mean and sd amount of impacted habitat (both prop impact and extinction risk), and the number of species per cell. You can run this as a background job if you'd like, it takes ~3.5 hours to complete. 

## 04_terrestrial_overlap_summary

GOAL: Prep data frames of total global amount of impacted habitat per terrestrial species.

## 05a_prep_aquamaps_depths.Rmd

GOAL: Find depth information for all AquaMaps species. 

## 05b_map_aquamaps_to_moll.R

GOAL: Reproject marine AOH rasters to mollweide 10km by 10km rasters and save as csvs. Takes a really long time to run...

## 06a_marine_vuln.Rmd

GOAL: Assign and gapfill vulnerability values for each species and stressor combination. Each species is vulnerable to either bycatch, biomass removal, or both, depending upon the ingredient which is being fished. 

## 06b_overlap_marine_background.R

GOAL: Overlap [Aquamaps probability of suitable habitat maps](https://www.aquamaps.org/) with our disturbance pressure maps created in the `02_feed` folder, and multiply by their vulnerability value. The goal of this script is to create impact maps, that is, the area of likely suitable habitat (>0.6 probability) for each species that is exposed AND impacted to harvest of forage or trimmings fish that is ultimately processed into FMFO. We create mean, sd, and nspp rasters for each taxonomic and ingredient grouping.

## 06c_overlap_fish_taxon.R

GOAL: In this script we had to split the larger "finfish" taxonomic grouping into two to be able to run that grouping on our server. At the end we combine the split group into one "fish" grouping by taking a species weighted mean and pooled variance for each cell.

## 07_marine_overlap_summary.Rmd

GOAL: In this script we prep a data set of total global impacted habitat per marine species, ingredient, and allocation approach.


## 08_process_mean_sd_summary.Rmd

GOAL: In this script we summarize the impact rasters created in 03b, 06a and 06b by finding the mean, standard deviation (pooled variance), and number of species.


## 09a - 09c

GOAL: Explore results and make plots for publication! 

