source('./src/shooting/allshots_analysis.r')
source('./src/shooting/shooting.r')

shots <- prepareShootingData()

team_stats <- read.csv('./output/2013-2014_advanced_team_stats.csv')
team_offense <- team_stats[,c("game_id", "team_name", "opp_team_name",
                              "Ortg", "FG3A", "FG2A")]

corner.3 <- shots[with(shots, grepl('corner.3', ShootingZone)),]

corner.3.agg <- ddply(corner.3, .(game_id, team_name), summarize, 
                   C3FGA=length(ShootingZone),
                   C3FGM=sum(as.numeric(Made==0)),
                   C3Ast=sum(assisted),
                   C3Points=sum(PointsScored),
                   C3PointsPerShot=mean(PointsScored))

corner.3.for.analysis <- merge(team_offense, corner.3.agg, 
                               by=c("game_id", "team_name"),
                               all.x=TRUE)

corner.3.for.analysis <- transform(corner.3.for.analysis, 
                                   C3FGMratio = C3FGM / (FG2A + FG3A))

p <- ggplot(corner.3.for.analysis, aes(C3FGMratio, Ortg)) + geom_point(alpha=0.7)