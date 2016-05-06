################################################################################
## This is script is designed to interrogate a shapefile for information. It 
## then uses that info to create folders and move files based on an ID. The 
## intention is that photos moved to a common folder will then be mosaiced.
##
## The final code block deals with deletions so BEWARE! Run this block after
## mosaic creation to clean the mosaic folders of individual images.
##
## Bart Huntley 06/05/2016

rm(list=ls())


### Install and Load Packages
is_installed <- function(mypkg) is.element(mypkg, installed.packages()[,1])
load_or_install<-function(package_names)  
{  
  for(package_name in package_names)  
  {  
    if(!is_installed(package_name))  
    {  
      install.packages(package_name,repos="http://cran.csiro.au/")  
    }  
    library(package_name,character.only=TRUE,quietly=TRUE,verbose=FALSE)  
  }  
}  
load_or_install(c("rgdal","dplyr","tidyr"))


### Directories
topdir <- paste0("Z:\\DEC\\Marine\\Working\\Kimberley\\Beaches\\Flight_Photos\\",
                 "Turtles\\Summer_Aerial_Surveys\\2013_2014")
photodir <- paste0(topdir, "\\ORIGINAL_PHOTOS")
mosdir <- paste0(topdir, "\\PHOTO_MOSAICS")

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
  opath <- paste(photodir, oFolds[i], sep = "\\")
  photos <- list.files(opath)
  ## Subset Data Frame to Match Existing Photos
  wkdf <- shpdf[shpdf$IMAGE %in% photos, ]
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
    from.k <- paste(opath, wkdf$IMAGE[k], sep = "\\")
    to.k <- paste(mosdir, wkdf$MOSFOLD[k], wkdf$IMAGE[k], sep = "\\")
    file.copy(from=from.k, to=to.k, recursive = FALSE, overwrite = TRUE, 
              copy.mode = TRUE)
  }
}


## WATCH OUT!! Deleting Photos used to make Mosaic
# mFolds <- list.dirs(mosdir)
# for(i in 1:length(mFolds)){
#   path.i <- paste(mosdir, mFolds[i], sep = "\\")
#   to_delete <- paste(path.i, list.files(path = path.i, pattern = "DSC"), 
#                      sep = "\\")
#   file.remove(to_delete)
# }

