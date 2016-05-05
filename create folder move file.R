################################################################################
## Movin' files about


wkdir <- ""
newloc <- ""



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


afiles <- list.dirs()
folds <- substr(afiles, 8, 15)

for(i in 1:length(folds)){
  if(!file.exists(folds[i])){dir.create(folds[i])}
}

for(j in 1:length(folds)){
  dir.i <- paste0(dir,"\\", afiles[j])
  from.i <- paste0(afiles[j], "\\", list.files(dir.i))
  to.i <- folds[j]
  file.copy(from=from.i, to=to.i, recursive = FALSE, overwrite = TRUE, copy.mode = TRUE)
}


