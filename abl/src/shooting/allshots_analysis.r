library(plyr)
library(reshape2)

source("src/shooting/shooting.r")

# create a shot chart for a single game
# inspiration:
# Kirk Goldsberry (of course)
# http://flowingdata.com/2012/10/04/more-on-making-heat-maps-in-r/

pbp <- read.csv("./input/2013-2014/07-play-by-play.csv")
shots <- getShotsFromPlayByPlay(pbp)

regseasPlyr <- read.csv("output/2013-2014_advanced_player_stats.csv")
gunners <- regseasPlyr[(regseasPlyr$team_name == 'Redwell Gunners Oberwart'),]
gunnersHome <- gunners[(gunners$home == 1),]
gunnersAway <- gunners[(gunners$home == 0),]

shotsHome <- shots[(shots$Team == 1),]
shotsAway <- shots[(shots$Team == 2),]

#merge(prettyBoxscore, rawPlayerBoxscore, by=c("game_id", "team_id"))
shotsHomeWithPlayerData <- merge(gunnersHome, shotsHome, 
                                 by.x=c("game_id", "player_no"),
                                 by.y=c("game_id", "PlayerShirtNumber"),
)  
shotsAwayWithPlayerData <- merge(gunnersAway, shotsAway, 
                                 by.x=c("game_id", "player_no"),
                                 by.y=c("game_id", "PlayerShirtNumber"),
)  

shotsGunners <- rbind(shotsHomeWithPlayerData, shotsAwayWithPlayerData)

# plotShots(shots)
# plotShots(shotsHome)
# plotShots(shotsAway)

plotShots(shotsGunners)
plotShotsByPlayer(shotsGunners)