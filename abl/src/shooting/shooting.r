library(plyr)
library(ggplot2)
library(png)
library(grid)

getCourt <- function() {
  img <- readPNG("docs/court-coordinates.png")
  g <- rasterGrob(img, interpolate=TRUE)
  return(g)
}

courtImg <- getCourt()

shotPlot <- function(shots) { 
  p <- ggplot(shots, aes(ShotLocation.x, ShotLocation.y, 
                         colour=factor(PointsScored)
                         )) +
        xlim(0,279) + ylim(-200,0) +
        geom_point(alpha=0.5) +
        coord_fixed() +
        annotation_custom(courtImg, xmin=-0, xmax=279, ymin=-200, ymax=0)
  return(p)
}

shootingHeatMap <- function(shots) {
  binSize <- 5
  shotsBin <- transform(shots 
                        , xbin = ShotLocation.x %/% binSize
                        , ybin = ShotLocation.y %/% binSize
  )
  
  shots.count <- count(shotsBin, c("xbin","ybin"))
  shots.eff <-aggregate(list(PointsScored=shotsBin$PointsScored), by=list(xbin=shotsBin$xbin,ybin=shotsBin$ybin), 
                        FUN=mean, na.rm=TRUE)
  
  shots.agg <- merge(shots.count, shots.eff, by=c("xbin", "ybin"))
  
  #   shots.agg <- transform(shots.agg, 
  #                          color = getColorByPoints(PointsScored))
  
  shots.agg$color <- sapply(shots.agg$PointsScored, getColorByPoints)
  
  # Size symbols by number of shots.
  plot <- symbols(shots.agg$xbin, 
          shots.agg$ybin, 
          squares=sqrt(shots.agg$freq)/3, asp=1, inches=FALSE,
          bg=shots.agg$color,
          fg=NA) 
  return(plot)
}

plotShots <- function(shots) {
  print(shotPlot(shots))
  print(shootingHeatMap(shots))
}

plotShotsByPlayer <- function(shots) {
  p <- shotPlot(shots) + facet_wrap(~name)  #,scales="free"
  print(p)
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

getShotsFromPlayByPlay <- function(pbp) {
  shots <- pbp[(pbp$log_action == 'shot'),]
  
  shots <- rename(shots, 
                  c("log_param_1"="Team"
                    ,"log_param_2"="PlayerShirtNumber"
                    ,"log_param_3"="Coordinates"
                    ,"log_param_4"="Made"
                    ,"log_param_5"="ShotType"
                    ,"log_param_6"="Points"
                  ))
  
  shots <- transform(shots, 
                     ShotLocation = colsplit(Coordinates, pattern = "\\+", names = c('x', 'y')))
  
  # use the coordinate system of the court-coordinates reference (see docs)
  shots$ShotLocation.y <- shots$ShotLocation.y * -1
  
  # Made == indicates a made fieldgoal; Why Points -1? I don't know
  shots <- transform(shots, 
                     PointsScored = ifelse(Made == 0, as.numeric(Points) - 1, 0))
  
  return(shots)
}