#' process snodas for smaller basin

library(raster)
library(gdalUtils)
library(sf)
library(tidyverse)
library(fasterize)

#' # Read GIS file for a basin that drains to a dam
#' a test basin to crop the continental snowpack dataset
#' this is a shapefile for the tuolumne basin in California (contains most of yosemite NP). the snowpack melts into the Hetch Hetchy Reservoir and is subject to significant scientific research.
#' I happen to have this shape file from the folks at aso.jpl.nasa.gov but it could also be computed using standard drop point GIS functions.
#' It could also be created by merging polygons of smaller watersheds from the national hydrography database (NHD). The basin boundary I am using is unfortunately not a standard HUC 6, 8, 10, etc
tuo_border <- sf::st_read('data/gis/tuo_border_utm11.shp')
tuo_cropextent <- tuo_border %>% st_buffer(10000) %>% st_bbox() # a little buffer for cropping so we don't lose any data

#' # Get list of SNODAS files
#' .hdr files are metadata files that reference the data in .dat
dir_snodas <- 'data/snodas/swe/conus'
fns <- dir(dir_snodas, pattern='.Hdr$', full.names=T)

#' ## Setup new directory to save cropped files
dir_tuo <- 'data/snodas/swe/tuo'
dir.create(dir_tuo,rec=T,showWarnings = FALSE)

#' # Read CONUS SNODAS file and crop to extents of small basin
f=fns[1]
purrr:::walk(rev(fns),function(f){
  print(f)
  dte_num <- str_sub(basename(f),start=28,end=35)
  myfile <- paste0('swe_tuo_',dte_num,'.tif')
  myfile_full <- file.path(dir_tuo,myfile)
  
  # there are lines of the metadata file that is causing an error for some files. I dont think its important for the general structure  so removing
  # bug reported, and it should be fixed in gdal 2.3.0 https://github.com/OSGeo/gdal/issues/506
  rl <- read_lines(f)
  rl2 <- str_trim(rl)
  rl3 <- map_chr(rl2,function(x){
    # print(x)
  if(grepl('BARD',x)){
    x <- 'BARD codes too long'
  } 
    return(x)
  })
  write_lines(x=rl3,path=f)
  
  if(!file.exists(myfile_full)){
    gdalUtils::gdalwarp(f,
                        myfile_full,
                        overwrite = T,
                        tr=c(1000,1000),
                        t_srs=st_crs(tuo_border)$proj4string,
                        te=as.vector(tuo_cropextent),
                        te_srs=st_crs(tuo_border)$proj4string,
                        r='near', #preserve classified pixels, eg snow free
                        dstnodata = -99,
                        multi = T
    )
  }
})

#' # Scale data and mask basin boundary
tif_fns <- dir(path='data/snodas/swe/tuo',glob2rx('^swe_tuo*.tif$'),full.names=T)
swe_stack <- raster::stack(tif_fns) / 1000 # convert mm to meters
# names(swe_stack) <- sub('swe_','',names(swe_stack))#drop the first part of the names so we can overwrite exact same filename 
poly_raster <- fasterize::fasterize(tuo_border, swe_stack[[1]]) #need to create a basin mask
swe_stack <- mask(swe_stack,poly_raster)
writeRaster(swe_stack,'data/snodas/swe/tuo/masked.tif',bylayer=T,suffix='names',NAflag=-99,overwrite=T)


