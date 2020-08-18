#########################################################################
#CDED functions
#input a list of URL addresses for the cded mapsheets you want to download
#unzip and merge.https://pub.data.gov.bc.ca/datasets/175624

#author: Deepa Filatow
#modified: April 26, 2019
#########################################################################

#########################################################################
# define cded_get function
# input an are of interest, aoi to create a list of cded dem url links 
# unzip and merge files from https://pub.data.gov.bc.ca/datasets/175624
# return a stitched together RasterLayer object of the cded digital elevation model
# cropped to the aoi polygon extent
#########################################################################

cded_get <- function (aoi){
  start.time <- Sys.time()
  #libraries
  require(raster)
  require(dplyr)
  require(rgdal)
  require (bcdata)
  
  #url for cded data by east and west map tiles 
  cdedurl<- "https://pub.data.gov.bc.ca/datasets/175624/"
  
  cat("Retreiving mapsheet polygons from bcdc.")
  grid50k <- bcdc_query_geodata("https://catalogue.data.gov.bc.ca/dataset/f9483429-fedd-4704-89b9-49fd098d4bdb")%>%
    filter( INTERSECTS(aoi)) %>%
    collect()
  
  #add a field and populate with the map block
  cat("\nCreating url list for mapsheet tiles.")
  grid50k$block<-substring(grid50k$MAP_TILE, first = 2, last = 4)
  
  grid50k$url1<-paste0(cdedurl, grid50k$block,"/", grid50k$MAP_TILE, "_e.dem.zip")
  grid50k$url2<-paste0(cdedurl, grid50k$block,"/", grid50k$MAP_TILE, "_w.dem.zip")
  
  maplist <- c(grid50k$url1, grid50k$url2)
  
  temp<- tempfile()
  temp2 <- tempfile()
  x<-1
  for (i in maplist) {
    
    download.file(i, temp)
    unzip(zipfile = temp, exdir = temp2)
    cat("\nUnzipping ", i, ".")
    filename <- sub('\\.zip$','',(sapply(strsplit(i, '/'), tail, 1)))
    filename <- tolower(filename)
    
    if (x==1){
      r <- raster(file.path(temp2, filename))
      cat("Loaded ", filename, ".")
    }else{
      r<-merge(r, (raster(file.path(temp2, filename))))
      cat("\nLoaded and merged ", filename, ".\n")  
    }
    
    x<-x+1
  }
  unlink(c(temp, temp2))
  
  # crop and mask the merged dem to the aoi polygon. 
  #NOTE I believe cropping and masking is faster when your dem is large and your mask small.
  #WARNING the reprojecti0n may fail for large study areas due to memoey limitations
  cat("\nReprojecting raster. ")
  r <- raster::projectRaster(r, crs = crs(aoi)) 
  cat("\nCroping raster to aoi extent.\n")
  r <- crop(r, aoi) 
  
  print(Sys.time()- start.time)
  return (r)
}
##########################################################################

##########################################################################
#test function
##########################################################################

library(tidyverse)

#small community watershed test that spans mapsheet tiles
gam <- bcdata::bcdc_query_geodata("bc57faf7-23e4-43fe-918a-e999936dbafa", crs = 3005) %>% 
  filter( CW_CODE == 'GAM.001') %>% collect()
gam.dem <- cded_get(gam)
plot(gam.dem)

#get a larger test watershed for the aoi. 
#WARNING this takes a few minutes ~5.
pars <- bcdc_query_geodata("51f20b1a-ab75-42de-809d-bf415a0f9c62", crs = 3005) %>%
  filter( WATERSHED_GROUP_CODE == "PARS") %>%
  collect()

#run function with test aoi
#WARNING this takes a few minutes ~5
pars.dem <-cded_get(pars)
plot(pars.dem)

dem <- mask(pars.dem, pars)

#####consider splitting this into 3 functions
#bccded_url_list
#bccded_get
#bccded_reproject
#bccded (runs all)
