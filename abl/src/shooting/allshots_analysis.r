library(plyr)
library(reshape2)

source("src/shooting/shooting.r")

# create a shot chart for a single game
# inspiration:
# Kirk Goldsberry (of course)
# http://flowingdata.com/2012/10/04/more-on-making-heat-maps-in-r/

# plan

# read play-by-play data for game 2891071 (first play-off game Redwell Gunners)
pbp <- read.csv("./input/2013-2014/07-play-by-play.csv")
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

# Made == indicates a made fieldgoal; Why Points -1? I don't know
shots <- transform(shots, 
                   PointsScored = ifelse(Made == 0, as.numeric(Points) - 1, 0))

shotsHome <- shots[(shots$Team == 1),]
shotsAway <- shots[(shots$Team == 2),]

plotShots(shotsHome)
plotShots(shotsAway)
