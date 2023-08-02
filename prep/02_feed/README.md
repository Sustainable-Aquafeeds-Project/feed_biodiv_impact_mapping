Feed folder organization

## 1a_GAEZ_production.Rmd

GOAL: calculate tonnes of production for each GAEZ crop in each country.
These data are used in several places in the analysis.

Inputs: 
* GAEZ production tiff files
* spatial country boundaries
* GAEZ_crop_info.csv (combine some GAEZ categories, such as pulses)
Outputs: 
* GAEZcrop_production.csv

## 1b_GAEZ_trade_proportions.Rmd

GOAL: This uses the FAO Food balance sheets to determine imports vs. local production (minus exports).  Then, we use FAO Detailed Trade Matrix to determine the proportion of total imports coming from each country.  We use crop production data to gapfill countries with no trade data, if a country has no trade data we assume imports are proportional to global production.

This is used later in the 4a script where we determine country of origin for feed items consumed in each country.

Inputs: 
* FAO Detailed trade matrix
* FAO Food balance sheets
* crop production data (from 1.1_GAEZ_production)
* GAEZ_to_FAO_v2.csv
Outputs: 
* FAO_MAPSPAMcrop_trade_data.csv


## 2_aquaculture_diet_composition

GOAL: Extract and wrangle data describing average % diet composition for salmon aquaculture.

Inputs:
* feed/diet_composition_wrangling/salmon_aquaculture_diet_composition.xlsx (values taken from Aas)
* Region list: _spatial/_output/food_rgns.csv
Outputs:
aquaculture_diet_composition.csv


## 3_aquaculture_diet_total_consumption.Rmd

GOAL: Determine tonnes of each feed item consumed per year for 2021 production of salmon mariculture  
* feed conversion ratios  
* total tonnes production in each country

Inputs:
* aquaculture_diet_composition.csv (from: 2 aquaculture_diet_composition.Rmd)
* aquaculture_production_tidy.rds 
* FCR.csv

Outputs:
* total_aquaculture_ingredient_demand.csv
* total_global_ingredient_demand.csv
* global_feed_demand.rds


## 4a_crop_ingredient_allocation.Rmd

GOAL: Get proportion of each GAEZ crop that is consumed by salmon aquaculture in each country. Allocate to raw weight materials using mass and energetic allocation rates.

Outputs: a raster for each crop describing the amount of each crop produced (tonnes) in each country used by salmon aquaculture systems.


## 4b_forage_fmfo_allocation.Rmd

GOAL: Get proportion of fish oil and fish meal live weight equivalents that is consumed by salmon aquaculture in each country per forage fish species. Allocate to live weight fish using mass and energetic allocation rates.

Outputs: a raster and dataset for each fish oil and fish meal, under each diet scenario, describing the harvest of each FMFO live weight equivalents (tonnes) that are consumed in each country by salmon aquaculture systems.

## 4c_trimmings_fmfo_allocation.Rmd

GOAL: Get proportion of fish oil and fish meal live weight equivalents that is consumed by salmon aquaculture in each country per species that come from fish trimmings. Allocate to live weight fish using mass and energetic allocation rates.

Outputs: a raster and dataset for each fish oil and fish meal, under each diet scenario, describing the harvest of each FMFO live weight equivalents (tonnes) from trimmings that are consumed in each country by salmon aquaculture systems.

## 5_fish_disturbance_prod_rasters.Rmd

GOAL: Convert the tonnes of liveweight fish oil and fish meal to km2 equivalent using a PPR/NPP method taken from [Cashion et al.2017](https://doi.org/10.1111/faf.12222).

Outputs: A rastser for each fish oil and fish meal, under each diet scenario, describing the km2 equivalent of the harvest of FMFO liveweight equivalents (tonnes) from trimmings and regular FMFO catch that are consumed in each country by salmon aquaculture systems. 

## 6_resample_feed_rasters.Rmd

Goal: Resample all crop and fish based feed rasters to 100km2 cells. 


