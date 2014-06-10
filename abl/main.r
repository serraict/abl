source("./src/download.r")
source("./src/CalculateAdvancedGameStats.r")
source("./src/ReportTeamRatings.r")

args<-commandArgs(TRUE)

if(length(args) == 0) {
  season = "2013-2014"
} else {
  season <- args[1]  
}

inputDirectory <- GetInputDirectory(season)
CreateAdvancedStatsFiles(inputDirectory)

# read to check results:
regseasTeam <- read.csv2(sprintf("./output/%s_regseas_advanced_team_stats.csv", season))
regseasPlyr <- read.csv2(sprintf("./output/%s_regseas_advanced_player_stats.csv", season))
