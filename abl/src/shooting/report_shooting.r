source("src/shooting/allshots_analysis.r")
# write.csv(shots, "./output/2013-2014-advanced_shots.csv")

outputFile <- sprintf("./output/%s_shooting_report.pdf", "2013-2014")
pdf(outputFile, paper="a4r", width=12)
advancedShots <- read.csv("./output/2013-2014-advanced_shots.csv")
reportShooting(advancedShots,
               reportTeamData=TRUE, reportPlayerData=TRUE)
dev.off()
