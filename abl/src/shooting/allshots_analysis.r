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
homeGames <- regseasPlyr[(regseasPlyr$home == 1),]
awayGames <- regseasPlyr[(regseasPlyr$home == 0),]

shotsHome <- shots[(shots$Team == 1),]
shotsAway <- shots[(shots$Team == 2),]

#merge(prettyBoxscore, rawPlayerBoxscore, by=c("game_id", "team_id"))
shotsHomeWithPlayerData <- merge(homeGames, shotsHome, 
                                 by.x=c("game_id", "player_no"),
                                 by.y=c("game_id", "PlayerShirtNumber"),
)  
shotsAwayWithPlayerData <- merge(awayGames, shotsAway, 
                                 by.x=c("game_id", "player_no"),
                                 by.y=c("game_id", "PlayerShirtNumber"),
)  

advancedShots <- rbind(shotsHomeWithPlayerData, shotsAwayWithPlayerData)

plot <- shotPlot(advancedShots)

print(plot)

byTeamPlot <- plot + 
  facet_wrap(~team_name) +
  labs(title="Shooting by team")
plot(byTeamPlot)

byOpponentPlot <- plot + 
  facet_wrap(~opp_team_name) +
  labs(title="Opponent shooting by team")
plot(byOpponentPlot)

