library(sqldf)

############
#
# Constants
# 
############

ftaFactor <- 0.46

secondChanceFactor <- 1.07

############
#
# Functions
#
############

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

GetCompetitions <- function(allStats) {
  # 421: regular season, 627: playoffs
  sql <- paste("select cmp_ID, min(wed_Datum) as StartDate,",
               " count(*) as NrGameLines",
               "from allStats",
               "where cmp_ID=421 or cmp_ID=627",
               "group by cmp_ID",
               "order by min(wed_Datum)"
  )

  comps <- sqldf(sql)
  
  startYear <- as.numeric(substr(comps[1,"StartDate"],1,4))
  comps <- transform(comps,
                       Desc = paste("heren",paste(startYear,startYear+1,sep="-"),sep="_"))
  
  # should work too if there's only one competitions, hence the slicing
  compDescriptions <- c("regseas","playoffs")[1:nrow(comps)] 
  
  comps$Desc <- paste(comps$Desc,compDescriptions,sep="_")
  
  return(comps)
}

CreateAdvancedStatsFiles <- function (fileName) {
  sts <- read.csv2(fileName)
  sts <- transform(sts, spl_ID = paste(plg_ID,  
                                       spl_Voornaam, 
                                       spl_tussen,
                                       spl_Achternaam))
  comps <- GetCompetitions(sts)
  nrComps <- nrow(comps)
  
  message(sprintf("Analyzing %i competition(s):", nrComps))
  print(comps)
  
  for(i in 1:nrComps) {
    message(sprintf("Processing %s ... ", comps[i,"Desc"]))
    compStats <- sts[which(sts$cmp_ID==comps[i,"cmp_ID"]),]
    PrintCompetitionStatistics(compStats)
    CreateAdvancedStatsFilesForCompetition(compStats, 
                                           comps[i,"Desc"])
  }
}

PrintCompetitionStatistics <- function(sts) {
  games <- sqldf("select wed_ID from sts group by wed_ID")
  teams <- GetTeams(sts)
  message(sprintf("Processing %i games by %i teams, with %i player stat lines",
                  nrow(games),nrow(teams),nrow(sts)))
  print(teams)
  if(nrow(teams) < 8) {
    warning("Only found ", nrow(teams), " teams - are you missing some teams?")
  }
}

GetTeams <- function(sts) {
  teamsThuis <- sqldf(paste("select plg_ID, thuis_club as plg_Name from sts",
                            "where plg_ID = wed_ThuisPloeg",
                            "group by plg_ID, thuis_club"))
  
  teamsUit <- sqldf(paste("select plg_ID, uit_club as plg_Name from sts",
                            "where plg_ID = wed_UitPloeg",
                            "group by plg_ID, uit_club"))
  
  teams <- rbind(teamsThuis,teamsUit)
  teams <- sqldf(paste("select plg_ID, plg_Name from teams",
                                       "group by plg_ID, plg_Name"))
    
  return(teams)
}

CreateAdvancedStatsFilesForCompetition <- function (sts, compdesc) {
    
  ###############################
  #
  # Prepare teamStats data frame
  #
  ###############################
  
  advancedTeamsStatsOutputFile <- paste("./output/", compdesc, "_advanced_team_stats.csv", sep="")
  advancedPlayerStatsOutputFile <- paste("./output/", compdesc, "_advanced_player_stats.csv", sep="")
      
  teamStats <- GetAdvancedTeamStats(sts) 
  playerStats <- GetAdvancedPlayerStats(sts, teamStats)
  
  
  message("Writing result file ", advancedTeamsStatsOutputFile)
  write.csv2(teamStats, advancedTeamsStatsOutputFile)
  
  message("Writing result file ", advancedPlayerStatsOutputFile)
  write.csv2(playerStats, advancedPlayerStatsOutputFile)
  
  return(c(advancedTeamsStatsOutputFile,advancedPlayerStatsOutputFile))
}

GetAdvancedTeamStats <- function(sts) {
  psData <- data.frame(sts$wed_ID, sts$plg_ID, sts$wed_UitPloeg, sts$wed_ThuisPloeg, 
                       sts$scu_FTA, sts$scu_FTM, sts$scu_FGA, sts$scu_FGM, sts$scu_3PM,  
                       sts$scu_3PA, 
                       sts$scu_OffRebounds, sts$scu_DefRebounds, sts$scu_TurnOvers,
                       sts$scu_Minuten,
                       sts$scu_Fouten, sts$scu_Assists, sts$scu_Steals, sts$scu_Blocks)
  
  # prettify
  names(psData) <- sub("^sts.", "", names(psData))        
  names(psData) <- sub("scu_", "", names(psData))
  names(psData) <- sub("OffRebounds", "OR", names(psData))
  names(psData) <- sub("DefRebounds", "DR", names(psData))
  names(psData) <- sub("TurnOvers", "TO", names(psData))
  names(psData) <- sub("Fouten", "PF", names(psData))
  names(psData) <- sub("Assists", "Ast", names(psData))
  names(psData) <- sub("Steals", "Stl", names(psData))
  names(psData) <- sub("Blocks", "Blk", names(psData))
  names(psData) <- sub("^FG", "FG2", names(psData))
  names(psData) <- sub("3P", "FG3", names(psData))
    
  teams <- GetTeams(sts)
   
  sqlThuis <- paste("select wed_ID, plg_ID, wed_UitPloeg, wed_ThuisPloeg, ", 
                    "max(wed_TeamOffRebThuis) as [OR], ",
                    "max(wed_TeamDefRebThuis) as DR, ", 
                    "max(wed_TeamTurnOverThuis) as [TO] ",
                    "from sts where plg_Id=wed_ThuisPloeg ",
                    "group by wed_Id, plg_ID, wed_UitPloeg, wed_ThuisPloeg")
  
  stsThuis <- sqldf(sqlThuis)
  
  # add zeros for missing columns
  missingCols <- setdiff(names(psData), names(stsThuis))  # get missing cols
  stsThuis[missingCols] <- 0                              # add to stsThuis, fill with 0 
  
  sqlUit <- gsub("Thuis", "Uit", sqlThuis)
  sqlUit <- gsub(", wed_UitPloeg, wed_UitPloeg,", ", wed_UitPloeg, wed_ThuisPloeg,", sqlThuis)  # bug fix :)
  stsUit <- sqldf(sqlUit)
  stsUit[missingCols] <- 0 
  
  # merge the data frames to obtain a frame we can aggregate on by wed_ID and plg_ID
  psData <- rbind(psData, stsThuis, stsUit)

  # aggregate by game and team
  agg <- aggregate(psData[5:14] , by=list(wed_ID=psData$wed_ID, plg_ID=psData$plg_ID, wed_UitPloeg=psData$wed_UitPloeg, wed_ThuisPloeg=psData$wed_ThuisPloeg), FUN=sum)

  # add team name
  agg <- sqldf("select agg.*, teams.plg_Name from agg inner join teams on agg.plg_ID=teams.plg_ID")
  
  agg <- transform(agg, 
                   plg_ShortName = substr(plg_Name,0,8))
  
  # now we join the tables, so that we have opposing numbers on the same game line
  sqlGameLine = paste("select * from agg ",
                      "inner join agg opp on ",
                      "agg.wed_ID=opp.wed_ID and (",
                      "(agg.plg_ID = agg.wed_ThuisPloeg and opp.plg_ID = opp.wed_UitPloeg) or ",
                      "(agg.plg_ID = agg.wed_UitPloeg and opp.plg_ID = opp.wed_ThuisPloeg) ",
                      ")",
                      "",
                      "order by wed_ID"
  )
  
  teamStats <- sqldf(sqlGameLine) 
  
  # pretify columns; opponents columns are prefixed with "opp_"
  nrCols <- dim(teamStats)[2]/2
  oppCols <- paste("opp", names(teamStats)[nrCols+1:nrCols], sep="_")
  names(teamStats)[nrCols+1:nrCols] <- oppCols
  
  # sanity checks ...
  CheckMinutesPlayed(teamStats)

  #######################################################################
  #
  # Calculate performance indicators and add them to the teamStats frame
  #
  #######################################################################
  
  # ft ftrips
  teamStats <- transform(teamStats,
                         FTtrips = ftaFactor*FTA,
                         opp_FTtrips =  ftaFactor*opp_FTA
  )
  
  # pts
  teamStats <- transform(teamStats, 
                         pts = FTM + 2*FG2M + 3*FG3M,
                         opp_pts =  opp_FTM + 2*opp_FG2M + 3*opp_FG3M
  )

  # win or loss
  teamStats <- transform(teamStats,
                         Win = (pts > opp_pts) ,
                         Loss = (pts < opp_pts)
  )
    
  # a play is a turnover, a ft trip or field goal attempt 
  teamStats <- transform(teamStats, 
                         plays = TO + FTtrips + (FG2A + FG3A),
                         opp_plays = opp_TO + opp_FTtrips + (opp_FG2A + opp_FG3A)
  )
  
  # to calculate possessions, we have to take offensive rebounds into account
  teamStats <- transform(teamStats, 
                         ps = plays - secondChanceFactor * (FG2A + FG3A - FG2M - FG3M) * OR / (OR + opp_DR),
                         opp_ps = opp_plays - secondChanceFactor * (opp_FG2A + opp_FG3A - opp_FG2M - opp_FG3M) * opp_OR / (opp_OR + DR)
  )
  
  # check posessions, pace and indicate if the number of possessions is suspicious
  teamStats <- transform(teamStats,
                         avgps = round((ps + opp_ps) / 2),
                         WARNING = abs(ps-opp_ps) > 4.0,
                         pace = (400/(Minuten+opp_Minuten)) * ((ps + opp_ps) / 2))
  
  # offensive and defensive ratings
  # we use the average posessions, because we think that's the best estimate of the actual number of posessions
  teamStats <- transform(teamStats,
                         Ortg = 100 * pts / avgps,
                         Drtg = 100 * opp_pts / avgps,
                         Home =  plg_ID == wed_ThuisPloeg)
  
  # net rating
  teamStats <- transform(teamStats,
                         Nrtg = Ortg - Drtg)
  
  # Four factors: 1-3
  teamStats <- transform(teamStats,
                         EFGpct = (FG2M+1.5*FG3M)/(FG2A+FG3A),
                         ORpct = OR / (OR + opp_DR),
                         TOpct = TO / avgps,
                         FT4f = FTM / (FG2A+FG3A)
  )
  
  teamStats <- transform(teamStats,
                         opp_EFGpct = (opp_FG2M+1.5*opp_FG3M)/(opp_FG2A+opp_FG3A),
                         opp_ORpct = opp_OR / (opp_OR + DR),
                         opp_TOpct = opp_TO / avgps,
                         opp_FT4f = opp_FTM / (opp_FG2A+opp_FG3A)
  )
  
  # shooting distribution
  teamStats <- transform(teamStats,
                         FG2Apct = FG2A / (FG2A + FG3A + FTtrips),
                         FG3Apct = FG3A / (FG2A + FG3A + FTtrips),
                         FTTpct = FTtrips / (FG2A + FG3A + FTtrips)
  )
  
  # shooting percentages
  teamStats <- transform(teamStats,
                         FG2pct = FG2M / FG2A,
                         FG3pct = FG3M/ FG3A,
                         FTpct = FTM / FTA
  )
  
  # # point by category
  # teamStats <- transform(teamStats,
  #                      FG2pts = FG2M*2,
  #                      FG3pts = FG3M*3,
  #                      FTpts = FTM
  #                      )
  # 
  # # point contributions
  # teamStats <- transform(teamStats,
  #                      ContrFG2pts = FG2pts/pts,
  #                      ContrFG3pts = FG3pts/pts,
  #                      ContrFTpts = FTpts/pts
  #                      )
  
  return(teamStats)
}
  
