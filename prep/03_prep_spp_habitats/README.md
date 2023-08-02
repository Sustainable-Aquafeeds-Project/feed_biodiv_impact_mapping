AOH overlap folder organization

## 01_download_AOH.Rmd

GOAL: Download all Area of Habitat data (AOH)


## 02_reproject_AOH.R

GOAL: This reprojects all AOH rasters (which are ~1km x 1km) to 10km by 10km mollweide resolution we use for our analysis. I ran this as a background job (hence the .R script), and it takes ~14 hours to complete. 


## 03_overlap_AOH_aquafeeds.R

GOAL: This overlaps the terrestrial AOH maps with terrestrial crops grown for salmon aquafeeds. The result is dataframes which describe for each species, the amount of habitat they have, the amount of habitat that is disturbed, and other relevant indicators. You can run this as a background job if you'd like, it takes ~XX hours to complete. 

## 04_download_prep_aquamaps.Rmd

GOAL: Preps aquamaps raster data to be overlapped with our FMFO production rasters. 