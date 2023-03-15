

#projections

equal_area_gp_proj <- "+proj=cea +lon_0=0 +lat_ts=45 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

equal_area_moll_projstring <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"

equal_area_moll_esri <- "ESRI:54009"



library(raster)
library(sp)


## to source:
## source('https://raw.githubusercontent.com/cdkuempel/food_chicken_salmon/master/_spatial/template_raster.R?token=ABLMCDNSCGBY3NWPCDJRISC5JA4JI')


food_raster <- raster(nrows=2160, ncols=4320, xmn=-180, xmx=180, ymn=-90, ymx=90)
food_crs <- "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"

raster_df <- function(input_raster){ #input_raster=food_raster
  template_rast <- raster(nrows=2160, ncols=4320, xmn=-180, xmx=180, ymn=-90, ymx=90)
  values(template_rast) <- 1:ncell(food_raster)
  template_df <- as.data.frame(template_rast, xy=TRUE)
  names(template_df)[3] <- "cellindex"
  if(ncell(input_raster) == ncell(template_rast)){
    final_rast <- cbind(template_df, as.data.frame(input_raster))
    return(final_rast)
  } else
    stop("Rasters are different sizes")
  
}


fake_raster <- raster(nrows=2160, ncols=4320, xmn=-180, xmx=180, ymn=-89.999, ymx=90)
values(fake_raster) <- 1000
check <- raster_df(fake_raster)