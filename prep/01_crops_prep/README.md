# Crop data wrangling

This folder organizes crop maps using GAEZ crop data as a foundation for calculating the raw material origins associated with feed crops. The scripts follow a step-by-step approach to harmonizing crop codes in GAEZ to FAOSTAT data we'll be using in the analyses, scaling the crop production data to the year 2021 and understanding how crop production varies within each country.

## Scripts
|File Name|Description|Output|
|---	|--- |---	|
|step2_GAEZ_wrangling|Harmonizing GAEZ names and FAO crop codes|GAEZ_names.csv|
|step3_GAEZ_scaling_2015_to_2021|Rescale GAEZ data to years 2019, 2020, and 2021|scaling_coef.csv|

## Data 
|File Name|Process Extent|Description|Source|
|---	|--- |---	|---	|
|FAOSTAT_crop_production_2015_2021.csv|Raw|Crop production data extracted for year 2015-2021.|FAOSTAT database: crop production.|
|GAEZ_crop_info.csv|Raw|Dataset downloaded from GAEZ with FAO codes for each SPAM crop category.|[GAEZ data webpage](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/KJFUO1).|
|scaling_coef.csv|Modified|Crop production (2015-2021) coefficients by GAEZ crop category.|Output from step 2.|
|prod_crop_rgns.csv|Crop production in tonnes for each production system, iso3c, and crop.|NA|

## Contributors
[Gage Clawson](gage.clawson@utas.edu.au)