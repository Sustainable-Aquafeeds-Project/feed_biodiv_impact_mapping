library(terra)
library(raster)
library(tidyterra)
library(dplyr)

select <- dplyr::select

#projections

equal_area_gp_proj <- "+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

equal_area_moll_projstring <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

equal_area_moll_esri <- "ESRI:54009"


food_raster <- rast(nrow=2160, ncol=4320, xmin=-180, xmax=180, ymin=-90, ymax=90)
food_crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

## make a mollweide 10km by 10km raster 

# Define the dimensions
nrow <- 1814
ncol <- 3617
nlyr <- 1

# Define the resolution
x_res <- 10000
y_res <- 10000

# Define the extent
xmin <- -18086282
xmax <- 18083718
ymin <- -9069952
ymax <- 9070048

# Define the coordinate reference system (CRS)
crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# Create the raster template
moll_template <- rast(nrow=nrow, ncol=ncol, nlyr=nlyr, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
          res=c(x_res, y_res), crs=crs)


moll_template_xy <- data.frame(cell_id = 1:ncell(moll_template), as.data.frame(moll_template %>% tidyterra::mutate(val = 1), xy = TRUE)) %>%
  dplyr::select(x, y, cell_id)

## reproject the global food systems EEZ and land raster to be 10km mollweide and extract to get a csv 

# land_eez_rgns <- rast(file.path("/mnt/rdsi/raw_data/food-systems-project/land_eez_rgns.tif"))
# 
# land_eez_rgns_mol <- project(land_eez_rgns, moll_template, method = "bilinear")
# 
# plot(land_eez_rgns)
# plot(land_eez_rgns_mol)
# 
# land_eez_rgns_mol_df <- land_eez_rgns_mol %>%
#   as.data.frame(., xy = TRUE)
#  # left_join(rgn_ids, by = c("land_eez_rgns" = "ID_0"))
# 
# write_rds(land_eez_rgns_mol_df, here("prep/03_prep_spp_habitats/data/spatial/land_eez_rgns_mol_xy.rds"))

