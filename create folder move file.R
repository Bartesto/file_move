################################################################################
## Movin' files about

rm(list=ls())

library(rgdal)
library(dplyr)
library(tidyr)

### Directories
topdir <- "Z:\\DEC\\Marine\\Working\\Kimberley\\Beaches\\Flight_Photos"
photodir <- paste0(topdir, "\\Turtles\\Summer Aerial Surveys\\2013_2014\\",
                   "ORIGINAL_PHOTOS\\Day_8_20140107")
photodir <- paste0(topdir, "\\Turtles\\Summer Aerial Surveys\\2013_2014\\",
                   "ORIGINAL_PHOTOS")
mosdir <- paste0(topdir, "\\Turtles\\Summer Aerial Surveys\\2013_2014\\",
                 "PHOTO_MOSAICS")

### Get Info From Shape File
setwd(topdir)
shpfiles <- list.files(pattern = "*.shp")
shpfiles <- shpfiles[!grepl("xml", shpfiles)] #xml handler
shpnames <- unlist(strsplit(shpfiles, split = "\\."))
shp <- shpnames[c(TRUE,FALSE)] #this returns the odd indexes
shp <- readOGR(dsn = topdir, shp)

### Make a Useful Data Frame
shpdf <- shp@data
shpdf$LOCATION <- as.character(shpdf$LOCATION)
shpdf$IMAGE <- as.character(shpdf$IMAGE)
shpdf$MOSFOLD <- paste(shpdf$LOCATION, shpdf$ID_MOSAIC, sep = "_")

### Get Original Photo Folders to Loop Over
list.dirs <- function(path=".", pattern=NULL, all.dirs=FALSE,
                      full.names=FALSE, ignore.case=FALSE) {
  # use full.names=TRUE to pass to file.info
  all <- list.files(path, pattern, all.dirs,
                    full.names=TRUE, recursive=FALSE, ignore.case)
  dirs <- all[file.info(all)$isdir]
  # determine whether to return full names or just dir names
  if(isTRUE(full.names))
    return(dirs)
  else
    return(basename(dirs))
}
oFolds <- list.dirs(photodir)

for(i in 1:length(oFolds)){
  photos <- list.files(paste(photodir, oFolds[i], sep = "\\"))
  ## Subset Data Frame to Match Existing Photos
  wkdf <- shpdf[photos %in% shpdf$IMAGE, ]
  wkdf <- wkdf%>%
    arrange(IMAGE)
  ## Create Mosaic Folders
  setwd(mosdir)
  foldnames <- unique(wkdf$MOSFOLD)
  for(j in 1:length(foldnames)){
    if(!file.exists(foldnames[j])){dir.create(foldnames[j])}
  }
  ## Copy Photos to Mosaic Folders
  for(k in 1:length(wkdf$IMAGE)){
    from.k <- paste(photodir, wkdf$IMAGE[k], sep = "\\")
    to.k <- paste(mosdir, wkdf$MOSFOLD[k], wkdf$IMAGE[k], sep = "\\")
    file.copy(from=from.k, to=to.k, recursive = FALSE, overwrite = TRUE, 
              copy.mode = TRUE)
  }
}








#
photos <- list.files(photodir)
wkdf <- shpdf[photos %in% shpdf$IMAGE, ]
wkdf <- wkdf%>%
  arrange(IMAGE)

#
setwd(mosdir)
foldnames <- unique(wkdf$MOSFOLD)
for(j in 1:length(foldnames)){
  if(!file.exists(foldnames[j])){dir.create(foldnames[j])}
}

##
for(k in 1:length(wkdf$IMAGE)){
  from.k <- paste(photodir, wkdf$IMAGE[k], sep = "\\")
  to.k <- paste(mosdir, wkdf$MOSFOLD[k], wkdf$IMAGE[k], sep = "\\")
  file.copy(from=from.k, to=to.k, recursive = FALSE, overwrite = TRUE, 
            copy.mode = TRUE)
}

### WATCH OUT!! Deleting Original Photos
# files_to_delete <- paste(photodir, list.files(photodir), sep = "\\")
# file.remove(files_to_delete)


