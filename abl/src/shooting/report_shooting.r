source("src/shooting/allshots_analysis.r")

outputFile <- sprintf("./output/%s_shooting_report.pdf", "2013-2014")
pdf(outputFile, paper="a4r", width=12)
advancedShots <- reportShooting()
dev.off()
