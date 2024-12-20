Folder and file organization

## 00_tidying_data
GOAL: Tidy data that will be used in subsequent scripts

Inputs: 
* GAEZ crop production tiff files
* FAOSTAT crop and aquaculture production
* Watson fisheries catch data
* IUCN API

## 01_crops_prep/
GOAL: This folder contains scripts that wrangles GAEZ production data. It re-scales the 2015 GAEZ data to match the mean of 2019, 2020, and 2021 FAOSTAT crop production data. 


## 02_feed/
GOAL: This folder contains scripts that estimate the raw material origins (and their disturbance pressures) of salmon aquaculture feeds using economic, energetic, and mass allocation, under different feed formulation scenarios.

## 03_prep_spp_habitats/

Goal: Contains scripts that prep Area of Habitat (AOH) maps and AquaMaps suitable habitat maps to the correct resolution and projection (10km by 10km, WGS84 Mollweide) and overlays these with the raw material origins feed maps and species sensitivities to determine amount of habitat impacted by salmon aquafeed production.


