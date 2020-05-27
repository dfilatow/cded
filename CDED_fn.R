#########################################################################
#CDED functions
#input a list of URL addresses for the cded mapsheets you want to download
#unzip and merge.https://pub.data.gov.bc.ca/datasets/175624

#author: Deepa Filatow
#modified: April 26, 2019
#########################################################################

#clear memory
rm(list = ls())

#load some test inputs
url1<-"https://pub.data.gov.bc.ca/datasets/175624/93j/093j09_e.dem.zip"
url2<-"https://pub.data.gov.bc.ca/datasets/175624/93j/093j09_w.dem.zip"
maplist <- c(url1, url2)


#########################################################################
#define functions
#########################################################################

#########################################################################
#cded_get function
#input a list of URL addresses for the cded mapsheets you want to download
#unzip and merge.https://pub.data.gov.bc.ca/datasets/175624
#########################################################################

cded_get <- function (maplist){
  require(raster)
  require(dplyr)
  require(rgdal)
  
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




#run function with test data
cdem93j09 <-cded_get(maplist)
plot(cdem93j09)
