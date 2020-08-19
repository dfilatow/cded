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




#' Download a CDED DEM
#'
#' This function takes an area of interest \code{aoi} and obtains all intersecting CDED digital elevation tiles from \link{https://pub.data.gov.bc.ca/datasets/175624}.
#' Output DEM is merged and cropped to the \code{aoi}.
#'
#' @param aoi An area of interest as a simple features object.
#' @return A CDED digital elevation model as a raster object, cropped to area of interest.
#' @export
cded_get <- function (aoi){
  start.time <- Sys.time()

  #url for cded data by east and west map tiles
  cdedurl <- "https://pub.data.gov.bc.ca/datasets/175624/"

  nad83<-'+proj=longlat +ellps=GRS80 +datum=NAD83 +no_defs'
  aoi_crs<-sf::st_crs(aoi)

  if(aoi_crs!=nad83)
  {
    aoi<-sf::sf_project(from=aoi,to=nad83)
  }


  cat("Retreiving mapsheet polygons from bcdc.")
  grid50k <- bcdata::bcdc_query_geodata("https://catalogue.data.gov.bc.ca/dataset/f9483429-fedd-4704-89b9-49fd098d4bdb")%>%
    dplyr::filter(bcdata::INTERSECTS(aoi)) %>%
    dplyr::collect()

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
      r <- raster::raster(file.path(temp2, filename))
      cat("Loaded ", filename, ".")
    }else{
      r<-raster::merge(r,(raster::raster(file.path(temp2, filename))))
      cat("\nLoaded and merged ", filename, ".\n")
    }

    x<-x+1
  }
  unlink(c(temp, temp2))

  # crop and mask the merged dem to the aoi polygon.
  #NOTE I believe cropping and masking is faster when your dem is large and your mask small.
  #WARNING the reprojecti0n may fail for large study areas due to memoey limitations
  cat("\nReprojecting raster. ")
  r <- raster::projectRaster(r, crs = aoi_crs)
  cat("\nCroping raster to aoi extent.\n")
  r <- raster::crop(r, aoi)

  print(Sys.time()-start.time)
  return (r)
}
##########################################################################

##########################################################################
#test function
##########################################################################

# library(tidyverse)
#
# #small community watershed test that spans mapsheet tiles
# gam <- bcdata::bcdc_query_geodata("bc57faf7-23e4-43fe-918a-e999936dbafa", crs = 3005) %>%
#   filter( CW_CODE == 'GAM.001') %>% collect()
# gam.dem <- cded_get(gam)
# plot(gam.dem)
#
# #get a larger test watershed for the aoi.
# #WARNING this takes a few minutes ~5.
# pars <- bcdc_query_geodata("51f20b1a-ab75-42de-809d-bf415a0f9c62", crs = 3005) %>%
#   filter( WATERSHED_GROUP_CODE == "PARS") %>%
#   collect()
#
# #run function with test aoi
# #WARNING this takes a few minutes ~5
# pars.dem <-cded_get(pars)
# plot(pars.dem)
#
# dem <- mask(pars.dem, pars)
#
# #####consider splitting this into 3 functions
# #bccded_url_list
# #bccded_get
# #bccded_reproject
# #bccded (runs all)
