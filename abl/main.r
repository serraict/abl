source("./src/download.r")
source("./src/AdvancedTeamStats.r")
source("./src/ReportTeamRatings.r")

args<-commandArgs(TRUE)

if(length(args) == 0) {
  season = "2013-2014"
} else {
  season <- args[1]  
}

inputDirectory <- GetInputDirectory(season)
files <- CreateAdvancedStatsFiles(inputDirectory)

print(files)

# read to check results:
regseasTeam <- read.csv2(files[1])
#regseasPlyr <- read.csv2(sprintf("./output/%s_regseas_advanced_player_stats.csv", season))

#View(regseasTeam)

PrintTeamRatings(regseasTeam, sprintf("./output/%s_report.pdf", season))
