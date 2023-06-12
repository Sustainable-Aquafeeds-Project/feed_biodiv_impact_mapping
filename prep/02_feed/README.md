Feed folder organization

## 1a_MAPSPAM_production.Rmd

GOAL: calculate tonnes of production for each mapspam crop in each country.
These data are used in several places in the analysis.

Inputs: 
* mapspam production tiff files
* spatial country boundaries
* MapSPAM_crop_info.csv (combine some SPAM categories, such as millet)
Outputs: 
* MAPSPAMcrop_production.csv

## 1b_MAPSPAM_trade_proportions.Rmd

GOAL: This uses the FAO Food balance sheets to determine imports vs. local production (minus exports).  Then, we use FAO Detailed Trade Matrix to determine the proportion of total imports coming from each country.  We use crop production data to gapfill countries with no trade data, if a country has no trade data we assume imports are proportional to global production.

This is used later in the 4a script where we determine country of origin for feed items consumed in each country.

Inputs: 
* FAO Detailed trade matrix
* FAO Food balance sheets
* crop production data (from 1.1_MAPSPAM_production)
* MapSPAM_to_FAO_v2.csv
Outputs: 
* FAO_MAPSPAMcrop_trade_data.csv


## 2_aquaculture_diet_composition

GOAL: Extract and wrangle data describing average % diet composition for salmon aquaculture.

Inputs:
* feed/diet_composition_wrangling/salmon_aquaculture_diet_composition.csv (from Aas)
* Region list: _spatial/_output/food_rgns.csv
Outputs:
aquaculture_diet_composition.csv


## 3_aquaculture_diet_total_consumption.Rmd

GOAL: Determine tonnes of each feed item consumed per year for 2017 production of salmon mariculture  
* feed conversion ratios  
* total tonnes production in each country

Inputs:
* aquaculture_diet_composition.csv (from: 2.3 aquaculture_diet_composition.Rmd)
* tonnes_per_country_group.csv 
* feed_conversion_aquaculture.csv

Outputs:
* total_aquaculture_feedstuff_consumption.csv
* fofm_aquaculture_corrected_consumption.csv


## 4a_crop_ingredient_allocation.Rmd

GOAL: Get proportion of each MAPSPAM crop that is consumed by salmon aquaculture in each country. Allocate to raw weight materials using mass and energetic allocation rates.

Outputs: a raster for each crop describing the amount of each crop produced (tonnes) in each country used by salmon aquaculture systems.


## 4b_forage_fmfo_allocation.Rmd

GOAL: Get proportion of fish oil and fish meal live weight equivalents that is consumed by salmon aquaculture in each country per forage fish species. Allocate to live weight fish using mass and energetic allocation rates.

Outputs: a raster for each fish oil and fish meal, under each diet scenario, describing the harvest of each FMFO live weight equivalents (tonnes) that are consumed in each country by salmon aquaculture systems.

## 4c_trimmings_fmfo_allocation.Rmd

GOAL: Get proportion of fish oil and fish meal live weight equivalents that is consumed by salmon aquaculture in each country per species that come from fish trimmings. Allocate to live weight fish using mass and energetic allocation rates.

Outputs: a raster for each fish oil and fish meal, under each diet scenario, describing the harvest of each FMFO live weight equivalents (tonnes) from trimmings that are consumed in each country by salmon aquaculture systems.


