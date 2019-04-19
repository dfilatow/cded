require(raster)
require(dplyr)
require(rgdal)

url1<-"https://pub.data.gov.bc.ca/datasets/175624/93j/093j09_e.dem.zip"
url2<-"https://pub.data.gov.bc.ca/datasets/175624/93j/093j09_w.dem.zip"
r3 <- r1+r2

temp<- tempfile()
temp2 <- tempfile()
download.file(url1, temp)
unzip(zipfile = temp, exdir = temp2)
r1 <- raster(file.path(temp2, "093j09_e.dem"))
unlink(c(temp, temp2))

temp<- tempfile()
temp2 <- tempfile()
download.file(url2, temp)
unzip(zipfile = temp, exdir = temp2)
r2 <- raster(file.path(temp2, "093j09_w.dem"))
unlink(c(temp, temp2))

r3 <- merge(r1, r2)
plot (r3)

#####################script works up to here################################
#########work on making this into a for loop that works off the naming convention in the CDED FTP
for (i in urllist) {
  temp<- tempfile()
  temp2 <- tempfile()
  download.file(i, temp)
  unzip(zipfile = temp, exdir = temp2)
  dem <- ########## minus the .zip and only grab the file name from the url i
  r <- raster(file.path(temp2, dem))
  ########add up the rasters into a raster list that will be merged
  unlink(c(temp, temp2))
}
  






r3 <- r1+r2