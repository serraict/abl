library(plyr)

plotShots <- function(shots) {
  
}

splitCoordinates <- function(coordinates) {
  splt <- strsplit(as.character(coordinates), '+', fixed=TRUE)
  return(c(as.numeric(splt[[1]][1]),as.numeric(splt[[1]][2])))
}

splitCoordinatesX <- function(coordinates) {
  splt <- splitCoordinates(coordinates)
  return(as.numeric(splt[1]))
}

splitCoordinatesY <- function(coordinates) {
  splt <- splitCoordinates(coordinates)
  return(as.numeric(splt[2]))
}


