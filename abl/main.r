source("./src/download.r")
source("./src/AdvancedTeamStats.r")
source("./src/AdvancedPlayerStats.r")
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
regseasTeam <- read.csv(files[1])
regseasPlyr <- read.csv(files[2])

View(regseasTeam)
View(regseasPlyr)

#outputFile <- sprintf("./output/%s_report.pdf", season)
#pdf(outputFile, paper="a4r", width=12)
#PrintTeamRatings(regseasTeam)
#dev.off()
