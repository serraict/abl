library(ggplot2)
library(reshape2)
library(lattice)
library(gplots)

# assumes that the teamStats data frame is initialized
# this is all plotting; no calculations are done

######################################################################
#
# Utility functions
#
######################################################################

PageWithTrendAndBoxPlot <- function (df, title, medianForComp, yLim) {
  message(sprintf("PageWithTrendAndBoxPlot '%s'... ", title))
  
  p <- ggplot(df, aes(x=game, y=value)) +
    labs(title=title)  +
    geom_hline(yintercept=medianForComp, linetype="dotted") +
    ylim(yLim)
  
  ptrend <- p +
    #stat_smooth(aes(fill = variable, colour=variable), size=1) +
    geom_point(aes(shape=opponent, colour=variable)) + 
    scale_shape_manual(values=as.numeric(df$opponent)) 
   
  pboxplot <- p + 
    geom_boxplot(aes(x=variable, fill=variable))
  
  
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(1, 4)))   
  print(ptrend, vp = viewport(layout.pos.row = 1, layout.pos.col = 1:3))         
  print(pboxplot, vp = viewport(layout.pos.row = 1, layout.pos.col = 4))
  message("Done.")
}

######################################################################
#
# Plot functions
#
######################################################################

OrtgByTeamPlot <- function (teamStats) {
  plt <- ggplot(teamStats, aes(team_name, Ortg)) + 
    geom_boxplot(aes(fill=team_name)) +
    geom_hline(aes(yintercept=median(Ortg)), linetype="dotted") +
    theme(axis.text.x = element_text(angle = -45, hjust = 0)) +    
    labs(title ="Offensive Rating") +
    xlab("") + 
    ylab("Points per 100 possessions")
  return(plt)
}   

drtgByTeamPlot <- function(teamStats) {
  return(ggplot(teamStats, aes(team_name, Drtg)) + 
           geom_boxplot(aes(fill=team_name)) +
           geom_hline(aes(yintercept=median(Drtg)), linetype="dotted") +
           theme(axis.text.x = element_text(angle = -45, hjust = 0)) +    
           labs(title ="Defensive Rating by Team") +
           xlab("") + 
           ylab("Points per 100 possessions"))
}

nrtgByTeamPlot <- function(teamStats) {
  return( ggplot(teamStats, aes(team_name, Nrtg)) + 
            geom_boxplot(aes(fill=team_name)) +
            geom_hline(aes(yintercept=median(Nrtg)), linetype="dotted") +
            theme(axis.text.x = element_text(angle = -45, hjust = 0)) +    
            labs(title ="Net Rating by Team") +
            xlab("") + 
            ylab("Points Difference per 100 possessions"))
}

fourFactorsDf <- function(teamStats) {
  d = data.frame(teamStats$Nrtg, 
                 teamStats$EFGpct, teamStats$ORpct, 
                 teamStats$TOpct, teamStats$FT4f
                 # when evaluating the competion, 
                 # it does not make sense to include opponent stats
                 #teamStats$opp_EFGpct, teamStats$opp_ORpct, 
                 #teamStats$opp_TOpct, teamStats$opp_FT4f
  )
  names(d) <- sub("^teamStats.", "", names(d))
  return(d)
}

fourFactorsForCompetition <- function(teamStats) {
  d <- fourFactorsDf(teamStats)
  
  forPlot <- teamStats[c("game_id","Nrtg","EFGpct","ORpct","TOpct","FT4f",
                         "team_name","home")] 
  forPlot.m <- melt(forPlot, id=c("game_id", "team_name", "home","Nrtg"))
  
  p <- ggplot(forPlot.m, aes(value, Nrtg)) +
    geom_point(alpha=0.4) + 
    stat_smooth(method="lm") +
    facet_wrap(~variable,scales="free")
  
  return(p)
}

fourFactorsForCompetitionCorrelationMatrix <- function(teamStats) {
  d <- fourFactorsDf(teamStats)
  return(cor(d))
}

plotfourFactorsForCompetitionCorrelationMatrix <- function(teamStats) {
  cor_melt <- melt(fourFactorsForCompetitionCorrelationMatrix(teamStats))
  return(ggplot(cor_melt, aes(Var1, Var2, fill=value, label=round(value, 2))) +
         #scale_fill_gradient() +
         geom_tile() +
         geom_text()
  )
}

TeamRatingQuadrantPlot <- function (teamStats) {
  
  agg <- aggregate(cbind(Ortg, Drtg, Nrtg) ~ team_name,
                   dat = teamStats, 
                   FUN = mean)
  
  moff <- mean(teamStats$Ortg)
  x0 <- moff-20
  x1 <- moff+20
  
  plt <- ggplot(agg, aes(x=Ortg, y=Drtg)) + 
    geom_point(aes(colour = factor(team_name), size = Nrtg)) +
    geom_abline(intercept = 0, slope = 1, linetype="dotted") +   
    geom_hline(yintercept = moff, linetype="dotted") +   
    geom_text(data = NULL, x = x0+2, y = moff, size=3, label = "league avg") + 
    geom_vline(xintercept = moff, linetype="dotted") +  
    geom_text(data = NULL, x = moff, y = x1, size=3, label = "league avg") + 
    coord_fixed() +
    labs(title ="Team Ratings") +
    geom_text(aes(label=team_name), vjust=2, size=3, alpha = I(0.6)) +
    xlab("Ortg (Points per 100 possessions)") + 
    ylab("Drtg (Points allowed per 100 possessions)") +
    xlim(x0,x1) + 
    ylim(x0,x1)
  return(plt)
}   

ptsDiffByTeamPlot <- function (teamStats) {
  return(ggplot(teamStats, aes(team_name, (pts-opp_pts))) + 
           geom_boxplot(aes(fill=team_name)) +
           theme(axis.text.x = element_text(angle = -45, hjust = 0)) +            
           geom_hline(aes(yintercept=0), linetype="dotted") +
           geom_hline(aes(yintercept=-6), linetype="dotted") +
           geom_hline(aes(yintercept=6), linetype="dotted") +
           labs(title ="Points Difference") +
           xlab("") + 
           ylab("Points"))
}

paceByTeamPlot <- function(teamStats) {
  return(allTeamsBoxPlot(teamStats, "pace") +
    geom_hline(aes(yintercept=median(pace)), linetype="dotted") +
    labs(title ="Game Pace") +
    ylab("#Possessions per 40 minutes"))
}

toPctPlot <- function(teamStats, opponent=FALSE) { 
  return(allTeamsBoxPlot(teamStats, "TOpct", opponent, "Turnovers per Possession") +  
           geom_hline(aes(yintercept=median(TOpct)), linetype="dotted") +          
           ylab("TO ratio"))
}

FT4fPlot <- function(teamStats, opponent=FALSE) {
  return(allTeamsBoxPlot(teamStats, "FT4f", opponent, "Free throws made per field goal attempt") + 
           geom_boxplot(aes(fill=team_name)) +
           geom_hline(aes(yintercept=median(FT4f)), linetype="dotted") +
           ylab(""))
}

orPctPlot <- function(teamStats, opponent=FALSE){
  return(allTeamsBoxPlot(teamStats, "ORpct", opponent,"Offensive Rebound % (OR%)") + 
           geom_hline(aes(yintercept=median(ORpct)), linetype="dotted") +
           ylab("OR%")   )
}

efgPctPlot <- function(teamStats, opponent=FALSE) {
  return(allTeamsBoxPlot(teamStats, "EFGpct", opponent, "Effective Field Goal % (EFG%)") +
           geom_hline(aes(yintercept=median(EFGpct)), linetype="dotted") +
           ylab("EFG%"))
} 

allTeamsBoxPlot <- function(teamStats, field, opponent=FALSE, title="") {
  if(opponent) { 
    field <- paste("opp_", field, sep="") 
    title <- paste("Opponent", title, sep=" ") 
  }
  aest <- aes_string(x = "team_name", y = field)
  return(ggplot(teamStats, aest) + 
           labs(title=title) +
           geom_boxplot(aes(fill=team_name)) +
           theme(legend.position="none") +
           theme(axis.text.x = element_text(angle = -45, hjust = 0)) + 
           xlab("")
  )
}

shotSelectionFG2A <- function(teamStats, opponent=FALSE) {
  return(allTeamsBoxPlot(teamStats, "FG2Apct", opponent) +
           geom_hline(aes(yintercept=median(FG2Apct)), linetype="dotted") +
           labs(title ="% shooting plays ending with a 2pt attempt")
  )
}

shotSelectionFG3A <- function(teamStats, opponent=FALSE) {
  return(allTeamsBoxPlot(teamStats, "FG3Apct", opponent) +
           geom_hline(aes(yintercept=median(FG3Apct)), linetype="dotted") +
           labs(title ="% shooting plays ending with a 3pt attempt")
  )
}

shotSelectionFTT <- function(teamStats, opponent=FALSE) {
  return(allTeamsBoxPlot(teamStats, "FTTpct", opponent) +
           geom_hline(aes(yintercept=median(FTTpct)), linetype="dotted") +
           labs(title ="% shooting plays ending with 2 or 3 ft")
  )
}

shotSelectionHistory <- function(teamStats) {
  
  dfm <- melt(teamStats,measure.vars=c('FG2Apct','FG3Apct', 'FTTpct'),id.vars = c('game_id', 'team_name', 'opp_team_name'))
  
  return(ggplot(dfm, aes(x=factor(game_id), y=value, fill=variable)) +
           geom_bar(stat="identity", color='black') +
           theme(axis.text.x = element_blank()) +
           facet_wrap(~ team_name, scales = "free_x", ncol=2) 
         
  )
  
}
######################################################################
#
# Output functions
#
######################################################################

PrintTeamRatings <- function(teamStats) {
  message("Creating team rating output file ...")
  
  teams <- sqldf(paste("select team_id, team_name from teamStats",
                       "group by team_id, team_name"))
  nrTeams <- nrow(teams)
  message(sprintf("Found %i teams in the team rating data set ...",nrow(teams)))
  print(teams)
  
  # Offensive and Defensive Ratings - Competition
  print(OrtgByTeamPlot(teamStats))

  print(drtgByTeamPlot(teamStats))

  print(nrtgByTeamPlot(teamStats))
  
  ptsByTeamPlot <- ggplot(teamStats, aes(team_name, pts)) + 
    geom_boxplot(aes(fill=team_name)) +
    geom_hline(aes(yintercept=median(pts)), linetype="dotted") +
    theme(axis.text.x = element_text(angle = -45, hjust = 0)) +    
    labs(title ="Points") +
    xlab("") + 
    ylab("Points")    
  print(ptsByTeamPlot)
  
  ptsAllowedByTeamPlot <- ggplot(teamStats, aes(team_name, opp_pts)) + 
    geom_boxplot(aes(fill=team_name)) +
    geom_hline(aes(yintercept=median(opp_pts)), linetype="dotted") +
    theme(axis.text.x = element_text(angle = -45, hjust = 0)) +    
    labs(title ="Points Allowed") +
    xlab("") + 
    ylab("Points")    
  print(ptsAllowedByTeamPlot)
                              
  print(ptsDiffByTeamPlot(teamStats))
  
  print(paceByTeamPlot(teamStats))
  
  # Performance Indicators - Competition
  
  print(fourFactorsForCompetition(teamStats))
  print(plotfourFactorsForCompetitionCorrelationMatrix(teamStats))
  
  print(efgPctPlot(teamStats))
  
  print(orPctPlot(teamStats))
  
  print(toPctPlot(teamStats))
  
  print(FT4fPlot(teamStats))
  
  print(efgPctPlot(teamStats, TRUE))
  
  print(orPctPlot(teamStats, TRUE))
  
  print(toPctPlot(teamStats, TRUE))
  
  print(FT4fPlot(teamStats, TRUE))
  
  message("Offensive and Defensive Ratings - by team ...")
  
  medianRatingCompetion <- median(teamStats$Ortg)
  yLim <- c(60, 170)
  
  for(i in 1:nrTeams){
    plgID <- teams[i,1]
    plgName <- teams[i,2]
    message(sprintf("processing %s (%i of %i) ...",plgName,i,nrTeams))
    forPlot <- teamStats[which(teamStats$team_id==plgID),]

    forPlot <- forPlot[c("Drtg","Ortg","opp_team_name","home")] 
    forPlot$game = c(1:length(forPlot$Ortg))
    forPlot <- rename.vars(forPlot, c("opp_team_name"), c("opponent"))
    
    forPlot.m <- melt(forPlot, id=c("game", "opponent", "home"))
      
    PageWithTrendAndBoxPlot(forPlot.m, plgName, medianRatingCompetion, yLim)
    
    message(sprintf("processed %s (%i of %i)",plgName,i,nrTeams))
  }  
  
  message("battle of ratio's per team ...")
  
  message("Ratio Details by team ...")
  
  yLim <- c(0, 0.8)
  
  for(i in 1:nrTeams){
    plgID <- teams[i,1]
    plgName <- teams[i,2]
    forPlot <- teamStats[which(teamStats$team_id==plgID),]  
    forPlot <- forPlot[c("opp_team_name","home",
                         "EFGpct","ORpct","TOpct","FT4f",
                         "opp_EFGpct","opp_ORpct","opp_TOpct","opp_FT4f"
                         )] 
    
    forPlot$game = c(1:length(forPlot$EFGpct))
    forPlot <- rename.vars(forPlot, c("opp_team_name"), c("opponent"))

    PageWithTrendAndBoxPlot(melt(forPlot, measure=c("EFGpct", "opp_EFGpct")), 
                            plgName, median(teamStats$EFGpct), yLim)
    PageWithTrendAndBoxPlot(melt(forPlot, measure=c("ORpct", "opp_ORpct")), 
                            plgName, median(teamStats$ORpct), yLim)
    PageWithTrendAndBoxPlot(melt(forPlot, measure=c("TOpct", "opp_TOpct")), 
                            plgName, median(teamStats$TOpct), yLim)
    PageWithTrendAndBoxPlot(melt(forPlot, measure=c("FT4f", "opp_FT4f")), 
                            plgName, median(teamStats$FT4f), yLim)
  }
#   
#   message("Shooting plays (2/3/FT) ...")
#   
#   layout(matrix(c(1,2,3,4,5,2,3,4), 2, 4, byrow=TRUE), widths=c(5,1,1,1))
#   
#   yLim <- c(0, 60)
#   
#   for(i in 1:nrTeams){
#     plgID <- teams[i,1]
#     plgName <- teams[i,2]
#     forPlot <- teamStats[which(teamStats$team_id==plgID),]
#     gameNrs <- c(1:nrow(forPlot))
#     
#     # absolute
#     plot(gameNrs, forPlot$FG2A, 
#          type="o", pch=1, lty=1, col="blue", 
#          xlab=plgName, ylab="#Shots",
#          ylim=yLim)
#     lines(gameNrs, forPlot$FG3A, 
#           type="o", pch=1, lty=1, col="purple", 
#           xlab=plgName, 
#           ylim=yLim)
#     lines(gameNrs, forPlot$FTtrip, 
#           type="o", pch=1, lty=1, col="red", 
#           xlab=plgName, 
#           ylim=yLim)
#     
#     abline(h=mean(forPlot$FG2A), lty=3, col="blue")
#     abline(h=mean(forPlot$FG3A), lty=3, col="purple")
#     abline(h=mean(forPlot$FTtrip), lty=3, col="red")
#     
#     boxplot((forPlot$FG2Apct), data=forPlot, 
#             xlab="FG2A", col="blue", 
#             ylim=c(0.0, 1.0) )
#     abline(h=median(teamStats$FG2Apct), lty=3)
#     
#     boxplot(forPlot$FG3Apct, data=forPlot, 
#             xlab="3FGA", col="purple",
#             ylim=c(0.0, 1.0) )
#     abline(h=median(teamStats$FG3Apct), lty=3)
#     
#     boxplot(forPlot$FTTpct, data=forPlot, 
#             xlab="FT trips", col="red",
#             ylim=c(0.0, 1.0) )
#     abline(h=median(teamStats$FTTpct), lty=3)
#     
#     # relative
#     plot(gameNrs, forPlot$FG2Apct, 
#          type="o", pch=1, lty=1, col="blue", 
#          xlab=plgName, ylab="Shot Selection Ratio",
#          ylim=c(0.0, 1.0))
#     lines(gameNrs, forPlot$FG3Apct, 
#           type="o", pch=1, lty=1, col="purple", 
#           xlab=plgName, 
#           ylim=c(0.0, 1.0))
#     lines(gameNrs, forPlot$FTTpct, 
#           type="o", pch=1, lty=1, col="red", 
#           xlab=plgName, 
#           ylim=c(0.0, 1.0))
#     
#   #   # create a bar plot using ggplot   
#   #   fields <- c("FGA", "FG3A", "FTtrips", "TO")
#   #   plays <- forPlot[fields]
#   #   plays["gameNrs"] <- gameNrs
#   #   meltedPlays <- melt(plays, id=c('gameNrs'))
#   #   qplot(factor(gameNrs), data=meltedPlays, 
#   #         geom="bar", fill=variable, weight=value)
#     
#   }
  
  # print some table to screen
  
  ratingTable <- sprintf("\n\n %-30s %5s %5s %5s %5s %5s \n", 
                         "Team",
                         "pts",
                         "opp",
                         "Ortg",
                         "Drtg",
                         "Nrtg")
  
  for(i in 1:nrTeams){
    plgID <- teams[i,1]
    plgName <- teams[i,2]
    forPlot <- teamStats[which(teamStats$team_id==plgID),]
    gameNrs = c(1:nrow(forPlot))
    
    row <- sprintf("%-30s %5.1f %5.1f %5.1f %5.1f %5.1f \n", 
                   plgName,
                   mean(forPlot$pts),
                   mean(forPlot$opp_pts),
                   mean(forPlot$Ortg),
                   mean(forPlot$Drtg),
                   mean(forPlot$Nrtg))
    ratingTable <- paste (ratingTable, row)
  }
  
  cat(ratingTable)
}                   