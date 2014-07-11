library(plyr)
library(ggplot2)
library(png)
library(grid)
library(reshape2)

getCourt <- function() {
  img <- readPNG("docs/court-coordinates.png")
  g <- rasterGrob(img, interpolate=TRUE)
  return(g)
}

getShootingZones <- function() {
  img <- readPNG("docs/court-coordinates-shooting-zones.png")
  return(img)  
}

getShootingZonesGrob <- function() {
  img <- getShootingZones()
  return(rasterGrob(img, interpolate=TRUE))
}

courtImg <- getCourt()
shootingColorScale <- c("#49FF00FF", "#49FF00FF",  
                        "#FFDB00FF", 
                        "#FF0000FB", "#FF0000FC", "#FF0000FD", "#FF0000FE", "#FF0000FF")

shootingZones <- getShootingZones()
shootingZonesDF <- read.csv('./docs/shootingzones.csv')

#todo: build this map from the dataframe
shootingZonesColorMap <- list(
    cffcc33='left.corner.3',
    cff9933='left.above.the.break.3',
    cff6633='top.3',
    cff3333='right.above.the.break.3',
    cff3399='right.corner.3',
    c33ffcc='left.baseline.long.2',
    c33ffff='left.long.2',
    c33ccff='top.long.2',
    c3366ff='right.long.2',
    c6633ff='right.baseline.long.2',
    c33ff00='left.short.2',
    c99ff00='top.short.2',
    cffff00='right.short.2',
    cff9900='left.at.basket',
    cff6600='top.at.basket',
    cff3300='right.at.basket'    
  ) 

getShootingZone <- function(x, y) {
  rowIndex <- y
  colIndex <- x
  zc <- shootingZones[rowIndex,colIndex,]
  clr <- tolower(gsub('#', 'c', rgb(zc[1], zc[2], zc[3])))
  return(shootingZonesColorMap[[clr]])
}

shotPlot <- function(shots) { 
  p <- ggplot(shots, aes(ShotLocation.x, ShotLocation.y, 
                         colour=factor(PointsScored)
                         )) +
        xlim(0,279) + ylim(-200,0) +
        geom_point(alpha=0.5) +
        coord_fixed() +
        annotation_custom(courtImg, xmin=-0, xmax=279, ymin=-200, ymax=0) +
        theme_bw()
  return(p)
}

shootingByZoneDataFrame <- function(shots) {
  shots.agg <- ddply(shots, .(ShootingZone), summarize, 
                     FGA=length(ShootingZone),
                     FGM=sum(as.numeric(Made==0)),
                     Ast=sum(assisted),
                     Points=sum(PointsScored),
                     PointsPerShot=mean(PointsScored)
                     )
  return(merge(shootingZonesDF, shots.agg, by=c("ShootingZone"), all = TRUE))
}

shootingByZonePlot <- function(shootingByZoneDataFrame) {
  p <- ggplot(shootingByZoneDataFrame, 
              aes(x, -y,
                  label=paste(round(PointsPerShot,2),"pps\n",
                              #FGM, "/", FGA, "\n",
                              round(Ast/FGM, 2), "ast%"
                              ))) +
    xlim(0,279) + ylim(-200,0) +
    geom_point(aes(size=FGA,
                   colour=PointsPerShot)) +
    annotation_custom(courtImg, xmin=-0, xmax=279, ymin=-200, ymax=0) +    
    geom_text(vjust=+1.2) +
    scale_colour_gradientn(colours = shootingColorScale, limits=c(0.0,3.0)) +
    coord_fixed() +
    theme_bw()
  
  return(p)  
}

shootingZoneLegendPlot <- function() {
  p <- ggplot(shootingZonesDF, 
              aes(x, -y,
                  label=ShootingZone)) +
    xlim(0,279) + ylim(-200,0) +
    annotation_custom(getShootingZonesGrob(), xmin=-0, xmax=279, ymin=-200, ymax=0) +    
    annotation_custom(courtImg, xmin=-0, xmax=279, ymin=-200, ymax=0) +    
    geom_point() +
    geom_text(size=5, angle=-30, vjust=1.2) +
    coord_fixed() +
    theme_bw()
  
  return(p)  
}

shootingHeatMapDataFrame <- function(shots, binSize=10) {
  shotsBin <- transform(shots 
                        , xbin = (ShotLocation.x %/% binSize) * binSize + binSize/2
                        , ybin = (ShotLocation.y %/% binSize) * binSize + binSize/2
  )
  
  shots.count <- count(shotsBin, c("xbin","ybin"))
   
  shots.eff <- aggregate(list(PointsPerShot=shotsBin$PointsScored), 
                        by=list(xbin=shotsBin$xbin,ybin=shotsBin$ybin), 
                        FUN=mean, na.rm=TRUE)
  
  shots.agg <- merge(shots.count, shots.eff, by=c("xbin", "ybin"))
  
  return(shots.agg) 
}

shootingHeatMapPlot <- function(shootingHeatMapDataFrame) { 
  p <- ggplot(shootingHeatMapDataFrame, aes(xbin, ybin, size=freq, colour=PointsPerShot, shape='square')) +
                  xlim(0,279) + ylim(-200,0) +
                  geom_point(alpha=0.7, shape=15) +
                  scale_colour_gradientn(colours = shootingColorScale, limits=c(0.0,3.0)) +
                  coord_fixed() +
                  theme_bw() +
                  annotation_custom(courtImg, xmin=-0, xmax=279, ymin=-200, ymax=0) 
  return(p)
}

plotShots <- function(shots) {
  print(shotPlot(shots))
  print(shootingHeatMap(shots))
}

plotShotsByPlayer <- function(shots) {
  p <- shotPlot(shots) + facet_wrap(~name) 
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
  nextPlay <- pbp[-1,c('log_action')]         # skip first row
  assisted <- c((nextPlay=='assist'), FALSE)  # append item to match nr of rows
  assisted <- as.numeric(assisted)
  pbp$assisted <- assisted
  
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
  shots$ShootingZone <- mapply(getShootingZone,
                               shots$ShotLocation.x,
                               shots$ShotLocation.y)
  shots$ShotLocation.y <- shots$ShotLocation.y * -1

  
  # Made == indicates a made fieldgoal; Why Points -1? I don't know
  shots <- transform(shots, 
                     PointsScored = ifelse(Made == 0, as.numeric(Points) - 1, 0))
  
  return(shots)
}