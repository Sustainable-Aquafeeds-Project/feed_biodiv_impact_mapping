
library(terra)
library(raster)

#projections

equal_area_gp_proj <- "+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

equal_area_moll_projstring <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

equal_area_moll_esri <- "ESRI:54009"


food_raster <- rast(nrow=2160, ncol=4320, xmin=-180, xmax=180, ymin=-90, ymax=90)
food_crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

## make a mollweide 10km by 10km raster 

# Define the dimensions
nrow <- 1755
ncol <- 3510
nlyr <- 1

# Define the resolution
x_res <- 10279.27
y_res <- 10279.27

# Define the extent
xmin <- -18040095
xmax <- 18040134
ymin <- -9020047
ymax <- 9020067

# Define the coordinate reference system (CRS)
crs <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# Create the raster template
moll_template <- rast(nrow=nrow, ncol=ncol, nlyr=nlyr, xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax,
          res=c(x_res, y_res), crs=crs)


