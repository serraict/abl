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

GetCompetitions <- function(season) {
  return(-1)
}

CreateAdvancedStatsFiles <- function (inputDirectory) {
  
  print(sprintf("processing %s ...", inputDirectory))
  
  # read teams file
  teams <- read.csv(sprintf("%s/01-teams.csv", inputDirectory))
  games <- read.csv(sprintf("%s/04-games.csv", inputDirectory))
  boxscores <- PrettyBoxScores(read.csv(sprintf("%s/05-boxscores-teams.csv", inputDirectory)), games, teams) 
  
  stop()
  
  # assuming a single competition
  PrintCompetitionStatistics(games, teams, boxscores)

  #
  CreateAdvancedStatsFilesForCompetition(games, teams, boxscores)
}

PrintCompetitionStatistics <- function(games, teams, sts) {
  message(sprintf("Processing %i games by %i teams, with %i player stat lines",
                  nrow(games),nrow(teams),nrow(sts)))
  print(teams)
}

PrettyBoxScores <- function(plainBoxScore, games, teams) {

  boxscoreCopy <- plainBoxScore

  names(boxscoreCopy) <- sub("^t_s_ro", "OR", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^t_s_rd", "DR", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^t_s_to", "TO", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^t_s_pf_cm", "PF", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^t_s_as", "Ast", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^t_s_st", "Stl", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^t_s_bl_fv", "Blk", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^t_s_1a", "FTA", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^t_s_1m", "FTM", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^t_s_2a", "FG2A", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^t_s_2m", "FG2M", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^t_s_3a", "FG3A", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^t_s_3m", "FG3M", names(boxscoreCopy))
  
  # remove all columns starting with t_s or s_
  boxscoreCopy <- boxscoreCopy[ , -grep("(^t_s|^s_)", names(boxscoreCopy)) ]
  
  homeBoxscore <- boxscoreCopy
  names(homeBoxscore) <- sub("_b$", "_opp", names(homeBoxscore))
  names(homeBoxscore) <- sub("_a$", "", names(homeBoxscore))
  
  awayBoxscore <- boxscoreCopy
  names(awayBoxscore) <- sub("_a$", "_opp", names(awayBoxscore))  
  names(awayBoxscore) <- sub("_b$", "", names(awayBoxscore))
  
  print(names(boxscoreCopy))
  print(names(homeBoxscore))
  print(names(awayBoxscore))
  
  # merge frames by name
  
  # move _opp to front of column name
  #oppCols <- paste("opp", names(teamStats)[nrCols+1:nrCols], sep="_")
  #names(teamStats)[nrCols+1:nrCols] <- oppCols
  
  
  return(0)
  
  # sanity checks ...
  CheckMinutesPlayed(teamStats)
}

CreateAdvancedStatsFilesForCompetition <- function (games, teams, boxscores, compdesc = "2013-2014") {

  advancedTeamsStatsOutputFile <- paste("./output/", compdesc, "_advanced_team_stats.csv", sep="")
  advancedPlayerStatsOutputFile <- paste("./output/", compdesc, "_advanced_player_stats.csv", sep="")
      
  teamStats <- GetAdvancedTeamStats(games, teams, boxscores) 
  # playerStats <- GetAdvancedPlayerStats(teamStats, compdesc)
   
  message("Writing result file ", advancedTeamsStatsOutputFile)
  # write.csv2(teamStats, advancedTeamsStatsOutputFile)
  
  message("Writing result file ", advancedPlayerStatsOutputFile)
  # write.csv2(playerStats, advancedPlayerStatsOutputFile)
  
  return(c(advancedTeamsStatsOutputFile,advancedPlayerStatsOutputFile))
}

GetAdvancedTeamStats <- function(games, teams, prettyBoxscores) {

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
  
