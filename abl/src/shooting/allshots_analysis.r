library(plyr)
library(reshape2)

source("src/shooting/shooting.r")

# create a shot chart for a single game
# inspiration:
# Kirk Goldsberry (of course)
# http://flowingdata.com/2012/10/04/more-on-making-heat-maps-in-r/

prepareShootingData <- function(pbpFile="./input/2013-2014/07-play-by-play.csv",
                                statsFile="./output/2013-2014_advanced_player_stats.csv") {
  pbp <- read.csv(pbpFile)
  shots <- getShotsFromPlayByPlay(pbp)
  
  regseasPlyr <- read.csv(statsFile)
  cols <- c("game_id", "player_no", "name", "person_id",
            "team_name", "opp_team_name", "home")
  homeGames <- regseasPlyr[(regseasPlyr$home == 1), cols]
  awayGames <- regseasPlyr[(regseasPlyr$home == 0), cols]
  
  shotsHome <- shots[(shots$Team == 1),]
  shotsAway <- shots[(shots$Team == 2),]
  
  shotsHomeWithPlayerData <- merge(homeGames, shotsHome, 
                                   by.x=c("game_id", "player_no"),
                                   by.y=c("game_id", "PlayerShirtNumber"),
  )  
  shotsAwayWithPlayerData <- merge(awayGames, shotsAway, 
                                   by.x=c("game_id", "player_no"),
                                   by.y=c("game_id", "PlayerShirtNumber"),
  )  
  
  advancedShots <- rbind(shotsHomeWithPlayerData, shotsAwayWithPlayerData)

  return(advancedShots)
}

reportShooting <- function(advancedShots, 
                           reportTeamData=TRUE, reportPlayerData=FALSE) {
 
  # competition & by team
  plot <- shotPlot(advancedShots)  
  plot(plot + labs(title="All games"))
  
  shootingByZone <- shootingByZoneDataFrame(advancedShots)
  plot(shootingByZonePlot(shootingByZone) + 
         labs(title="Shooting by zone - all games"))  
  
  shootingHeatMap <- shootingHeatMapDataFrame(advancedShots)
  plot(shootingHeatMapPlot(shootingHeatMap) 
       + labs(title="Shooting heat map - all games"))
   
  byTeamPlot <- plot + 
    facet_wrap(~team_name) +
    labs(title="Shooting by team")
  plot(byTeamPlot)
  
  byOpponentPlot <- plot + 
    facet_wrap(~opp_team_name) +
    labs(title="Opponent shooting by team")
  plot(byOpponentPlot)
  
  if(reportTeamData) {
    plotByTeam(advancedShots, reportPlayerData)
  }
  
}

plotByTeam <- function(advancedShots, reportPlayerData) {
  byTeam <- split(advancedShots, advancedShots$team_name)  # why not drop=TRUE?    
  
  for(team in names(byTeam)) {    
    allTeamShots <- byTeam[[team]]    
    plotTeamShots(allTeamShots, team, reportPlayerData=reportPlayerData)
  }
  
  byOpponent <- split(advancedShots, advancedShots$opp_team_name)   
  for(team in names(byOpponent)) {    
    allTeamShots <- byOpponent[[team]]    
    plotTeamShots(allTeamShots, paste(team, "'s opponents"), reportPlayerData=FALSE)
  }
}

plotTeamShots <- function (allTeamShots, titleHeader, reportPlayerData) {
  plot <- shotPlot(allTeamShots)
  plot(plot + labs(title=paste(titleHeader, "- All shots")))
  
  shootingByZone <- shootingByZoneDataFrame(allTeamShots)
  plot(shootingByZonePlot(shootingByZone) + 
         labs(title=paste(titleHeader, " - Shooting by zone"))) 
  
  shootingHeatMap <- shootingHeatMapDataFrame(allTeamShots)  
  plot(shootingHeatMapPlot(shootingHeatMap) + 
         labs(title=paste(titleHeader, " - Shooting heat map")))
  
  if(reportPlayerData) {
    byPlayerPlot <- plot + 
      facet_wrap(~name) +
      labs(title=paste(titleHeader, "Shooting by player"))
    plot(byPlayerPlot)
    
    plotByPlayer(allTeamShots)
  }
}

plotByPlayer <- function(allTeamShots, 
                         team = "",
                         reportPlayerData=FALSE) {
  byPlayer <- split(allTeamShots, allTeamShots$name, drop=TRUE)
  for(player in names(byPlayer)) {
    
    allPlayerShots <- byPlayer[[player]]
    
    plot <- shotPlot(allPlayerShots)
    plot(plot + labs(title=paste(player, team, "- All shots")))
    
    shootingByZone <- shootingByZoneDataFrame(allPlayerShots)
    plot(shootingByZonePlot(shootingByZone) + 
           labs(title=paste(player, team, " - Shooting by zone")))  
    
    shootingHeatMap <- shootingHeatMapDataFrame(allPlayerShots)  
    plot(shootingHeatMapPlot(shootingHeatMap) + 
           labs(title=paste(player, team, " - Shooting heat map")))    
  }
}
