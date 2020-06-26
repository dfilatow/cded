#########################################################################
#CDED functions
#input a list of URL addresses for the cded mapsheets you want to download
#unzip and merge.https://pub.data.gov.bc.ca/datasets/175624

#author: Deepa Filatow
#modified: April 26, 2019
#########################################################################

#########################################################################
#define cded_get function
# input an are of interest, aoi to create a list of cded dem url links 
#unzip and merge files from https://pub.data.gov.bc.ca/datasets/175624
# return a stitched together RasterLayer object of the dem in the crs provided, crs1
#########################################################################

cded_get <- function (crs1 = 3005, aoi){
  #libraries
  require(raster)
  require(dplyr)
  require(rgdal)
  require (bcdata)
  
  #url for cded data by east and west map tiles 
  cdedurl<- "https://pub.data.gov.bc.ca/datasets/175624/"
  grid50k <- bcdc_query_geodata("https://catalogue.data.gov.bc.ca/dataset/f9483429-fedd-4704-89b9-49fd098d4bdb", crs = crs1)%>%
    filter( INTERSECTS(aoi)) %>%
    collect()
  
  #add a field and populat with the map block
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
    print (cat("Unzipping ", i))
    filename <- sub('\\.zip$', '', (sapply (strsplit (i, '/'), tail, 1)))
    if (x==1){
      r <- raster(file.path(temp2, filename))
      print(cat("Loaded ", filename))
    }
    else {
      r<-merge(r, (raster(file.path(temp2, filename))))
    print(cat("Loaded and merged ", filename))  
    }
    
    x<-x+1
    }
  unlink(c(temp, temp2))
  return (r)
  
  }
##########################################################################

##########################################################################
#test function
##########################################################################

#get a test watershed for the aoi
aoi <- bcdc_query_geodata("51f20b1a-ab75-42de-809d-bf415a0f9c62", crs = 3005) %>%
  filter( WATERSHED_GROUP_CODE == "PARS") %>%
  collect()

#run function with test aoi
pars_dem <-cded_get(crs = 3005, aoi = aoi)
plot(pars_dem)
