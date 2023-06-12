# Crop Farm

This folder organizes crop maps using MapSPAM data as a foundation for calculating the raw material origins associated with feed crops. The scripts follow a step-by-step approach to harmonizing crop codes in MapSPAM to FAOSTAT data we'll be using in the analyses, scaling the crop production data to the year 2021 and understanding how crop production varies within each country.

## Scripts
|File Name|Description|Output|
|---	|--- |---	|
|step1_MapSPAM_verification|Verification for GitHub issue #98 regarding raster extent problem|None|
|step2_MapSPAM_wrangling|Harmonizing MapSPAM names and FAO crop codes|MapSPAM_names.csv|
|step3_MapSPAM_scaling_2010_to_2021|Rescale MapSPAM data to years 2019, 2020, and 2021|scaling_coef.csv|

## Data 
|File Name|Process Extent|Description|Source|
|---	|--- |---	|---	|
|FAOSTAT_crop_production_2010_2021.csv|Raw|Crop production data extracted for year 2010-2021.|FAOSTAT database: crop production.|
|MapSPAM_crop_info.csv|Raw|Dataset extracted from MapSPAM with FAO codes for each SPAM crop category.|[MapSPAM methodology webpage](https://www.mapspam.info/methodology/).|
|scaling_coef.csv|Modified|Crop production (2010-2021) coefficients by SPAM crop category.|Output from step 2.|
|prod_crop_rgns.csv|Crop production in tonnes for each production system, iso3c, and crop.|NA|

## Contributors
[Gage Clawson](samuel.clawson@utas.edu.au)