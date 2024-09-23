# feed_biodiv_impact_mapping
This is the repository for Clawson et al., "Continued transitions from fish meal and oil in aquafeeds require close attention to biodiversity trade-offs". This paper examines biodiversity impacts from the transition from fish- to plant-dominant feeds for Atlantic salmon (Salmo salar) and explores trade offs among ingredients and sourcing locations. 

Please read this file before trying to reproduce the output from this research project. Below you will find information on the publication associated with this repository, contact information for the lead author, and a description of the repository structure with each section explained.

## Link to association publication

Not yet available

## Contact

Please direct any correspondence to Gage Clawson at `gage.clawson@utas.edu.au`

## Reproducibility

We strongly advocate for open and reproducible science. The code in this repository enables a use to recreate the results outlined in the above publication. There are a few important points to know/adhere to for the code to run smoothly:

 - The code must be run as an R project (unless you would like to recreate it in another language) - the code within relies on relative file paths from the project home folder. 
 - There is large data required throughout, that we do not include in this repository due to GitHub's large file size limits. Please follow any instructions to download this data that is contained in the scripts within the `prep` folder. All data used is freely accessible online. 


## Repository structure

This repository is organised into four main folders:

 - /data: Contains raw and tidied data that is used in the `prep` folder scripts.
 - /prep: Contains data preparation scripts for tidying data and calculating biodiversity impacts. 
 - /src: Contains functions that read in file paths and necessary directories.
 - /analysis: Contains the figures (main and supplementary) from the analysis. Check here for results after running the preparation scripts. 
 
### 'prep' folder

The prep folder contains all code necessary to generate the results attached to the publication listed above.

For individual running, all scripts are numbered by the order in which they should be run. Within the 'prep' folder, there are actually 3 sub folders, which also have numbering, and individual READMEs within for guidance. Folder 01-02 are mostly processing, and do not require too much memory to run. However, `03_prep_spp_habitats` contains code which preps over 50,000 species habitat maps, overlaps said maps with a disturbance pressure, and calculates km2 impacts from this. We ran this on an external server which has substantial memory (128GB) and CPU cores (32), and the overlapping of species habitats and pressures may take multiple days to run (you have been warned). Additionally, some outputs from the data preparation can be very large and there are thousands of files which are saved (raw material and species level impacts of mean, sd, nspp impacted, etc.).

Note: `00_tidying_data` cannot be run unless the user is a member of the Sustainable Aquafeeds Project and has access to the Research Data Storage facility. Raw data is tidied in this script and products are saved to the project folders.

### 'src' folder

Contains three reference scripts `directories.R`, `functions.R`, and `spatial.R`, which hold regularly used directory file paths and functions respectively. These are sourced within individual markdown scripts when needed.
 
 
### 'data' folder

The data folder is divided into a number of subfolders, each with their own sub directories. The main sub directories within the data folder are listed below.
 
| Folder | Description|
|:---------|:------------|
| raw_data | This folder contains a number of sources of raw data needed for the analysis. This often not the raw product per se, it may be tidied, but it is not a data product |
| tidy_data | This folder contains a number of data products that have been produced from analyses regarding allocation, dietary scenarios, raw material and ingredient demand, and figure icons among others |
| spatial | This folder holds files which make spatial data manipulation easier, including a base raster and region ID rasters. |


### 'analysis' folder

This folder holds all code and figures produced from that code. Each script is numbered, but the folder does not necessarily need to be run in order. I've just numbered them in the order in which I ran them. Within this folder, there are a number of sub folders which contain main manuscript figures (`MS_plots`), supplementary figures (`SI_plots`), intermediate (`int/`) data files useful for plotting, and uncertainty/sensitivity exploration (`uncertainty`). 



