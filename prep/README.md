Folder and file organization

## 00_tidying_data
GOAL: Tidy data that will be used in subsequent scripts

Inputs: 
* GAEZ crop production tiff files
* FAOSTAT crop and aquaculture production
* Watson fisheries catch data
* IUCN API

## 00a_crop_raw_materials_conversions.Rmd
GOAL: This script preps co-product processing conversion factors. 

This is used later in the `02_feed` folder to allocate aquafeed ingredients to the raw material weight or live weight material.

Inputs: 
* plant_ingredient_codes: codes of plant ingredients that match FAO codes to MAPSPAM/GAEZ codes. 
* FAOSTAT tidied crop production data.
* mass and gross energy allocation data

Outputs: 
* crop_ingredient_allocation_factors.csv

## 01_crops_prep/
GOAL: This folder contains scripts that wrangles GAEZ production data. It rescales the 2015 GAEZ data to match the mean of 2019, 2020, and 2021 FAOSTAT crop production data. 


## 02_feed/
GOAL: This folder contains scripts that estimate the raw material origins of salmon aquaculture feeds using economic, energetic, and mass allocation. 

## 03_prep_spp_habitats/

Goal: Contains scripts that prep Area of Habitat (AOH) maps and Aquamaps suitable habitat maps to the correct resolution and projection (10km by 10km, WGS84 Mollweide) and overlays these with the raw material origins feed maps and species vulnerabilities to determine amount of habitat impcated by salmon aquafeed production.


