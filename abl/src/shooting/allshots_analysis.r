library(plyr)
library(reshape2)

source("src/shooting/shooting.r")

# create a shot chart for a single game
# inspiration:
# Kirk Goldsberry (of course)
# http://flowingdata.com/2012/10/04/more-on-making-heat-maps-in-r/

reportShooting <- function(reportTeamData=TRUE, reportPlayerData=FALSE) {
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
  return(advancedShots)
}

plotByTeam <- function(advancedShots, reportPlayerData) {
  byTeam <- split(advancedShots, advancedShots$team_name)  # why not drop=TRUE?  
  #for (team in c("Redwell Gunners Oberwart")) {
  for(team in names(byTeam)) {  
    allTeamShots <- byTeam[[team]]
    plot <- shotPlot(allTeamShots)
    shootingHeatMap <- shootingHeatMapDataFrame(allTeamShots)  
    plot(plot + labs(title=paste(team, "- All shots")))
    plot(shootingHeatMapPlot(shootingHeatMap) + 
           labs(title=paste(team, " - Shooting heat map")))
    
    byPlayerPlot <- plot + 
      facet_wrap(~name) +
      labs(title=paste(team, "Shooting by player"))
    plot(byPlayerPlot)
    
    if(reportPlayerData) {
      plotByPlayer(allTeamShots)
    }
  }
}

plotByPlayer <- function(allTeamShots, reportPlayerData=FALSE) {
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
