library(plyr)

# create a shot chart for a single game
# inspiration:
# Kirk Goldsberry (of course)
# http://flowingdata.com/2012/10/04/more-on-making-heat-maps-in-r/

# plan

# read play-by-play data for game 2891071 (first play-off game Redwell Gunners)
pbp <- read.csv("./input/2013-2014/07-play-by-play.csv")
pbpGame <- pbp[(pbp$game_id == 2891071 & pbp$log_action == 'shot'),]

pbpGame <- rename(pbpGame, 
                  c("log_param_1"="Team"
                  ,"log_param_2"="PlayerShirtNumber"
                  ,"log_param_3"="Coordinates"
                  ,"log_param_4"="Made"
                  ,"log_param_5"="ShotType"
                  ,"log_param_6"="Points"
                  ))

# filter out shooting events
# filter out 