library(plyr)

plotShots <- function(shots) {
  symbols(shots$ShotLocation.x, 
          shots$ShotLocation.y, 
          circles=rep(1,length(shots$ShotLocation.x)), asp=1, inches=FALSE)
  
  # 10px binning:
  shotsBin <- transform(shots 
                        , xbin = ShotLocation.x %/% 5
                        , ybin = ShotLocation.y %/% 5
  )
  
  shots.count <- count(shotsBin, c("xbin","ybin"))
  shots.eff <-aggregate(list(PointsScored=shotsBin$PointsScored), by=list(xbin=shotsBin$xbin,ybin=shotsBin$ybin), 
                      FUN=mean, na.rm=TRUE)
  
  shots.agg <- merge(shots.count, shots.eff, by=c("xbin", "ybin"))
  
#   shots.agg <- transform(shots.agg, 
#                          color = getColorByPoints(PointsScored))
  
  shots.agg$color <- sapply(shots.agg$PointsScored, getColorByPoints)

  View(shots.agg)
  # Size symbols by number of shots.
  symbols(shots.agg$xbin, 
          shots.agg$ybin, 
          squares=sqrt(shots.agg$freq)/6, asp=1, inches=FALSE,
          bg=shots.agg$color,
          fg=NA)
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

getColorByPoints <- function(val) {
  minVal <- .6   # stone cold
  maxVal <- 1.6  # red hot
  
  val <- min(maxVal,val)
  val <- max(minVal,val)
  
  numCols <- 30
  
  #pal <- colorRampPalette(c("#eddfab", "#d86853"))  # Yellow to red
  pal <- colorRampPalette(c("#2222ff", "#ff2222"))  
  cols <- pal(numCols)
  
  # Get index to pick color.
  colIndex <- round(numCols * (val - minVal) / (maxVal - minVal))
  colIndex <- max(1, colIndex)
  
  return(cols[colIndex])
}

