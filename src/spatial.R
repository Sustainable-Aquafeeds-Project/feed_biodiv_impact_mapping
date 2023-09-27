
library(terra)
library(raster)
library(tidyterra)

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



## make a lon/lat WGS 1km by 1km raster 

# # Define the dimensions
# nrow <- 180360
# ncol <- 360720
# nlyr <- 1
# 
# # Define the resolution
# x_res <- 0.0009920722
# y_res <- 0.0009920722
# 
# # Define the extent
# xmin <- -180
# xmax <- 180
# ymin <- -90
# ymax <- 90
# 
# # Define the coordinate reference system (CRS)
# crs <- "EPSG:4326"
# 
# # Create the raster template
# lon_lat_template <- rast(nrow=nrow, ncol=ncol, nlyr=nlyr, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
#                       res=c(x_res, y_res), crs=crs) %>%
#   mutate(new = 1)
# 
# lon_lat_cell_id <- setValues(lon_lat_template, 1:ncell(lon_lat_template))


