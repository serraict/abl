PrettyPlayerBoxScores <- function(prettyBoxscore, rawPlayerBoxscore, players) {
  # merge on game_id and team_id to link player data to the correct row
  boxscoreCopy <- merge(prettyBoxscore, rawPlayerBoxscore, by=c("game_id", "team_id"))   
  boxscoreCopy <- merge(players, boxscoreCopy, by="person_id")  

  names(boxscoreCopy) <- sub("^s_ro", "ply_OR", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^s_rd", "ply_DR", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^s_to", "ply_TO", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^s_pf_cm", "ply_PF", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^s_as", "ply_Ast", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^s_st", "ply_Stl", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^s_bl_fv", "ply_Blk", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^s_1a", "ply_FTA", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^s_1m", "ply_FTM", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^s_2a", "ply_FG2A", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^s_2m", "ply_FG2M", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^s_3a", "ply_FG3A", names(boxscoreCopy))
  names(boxscoreCopy) <- sub("^s_3m", "ply_FG3M", names(boxscoreCopy))
  
  boxscoreCopy <- transform(boxscoreCopy, ply_minutes = minutes + seconds/60)

  minutesPerGame <- aggregate(list(team_minutes=boxscoreCopy$ply_minutes), 
                             by=list(game_id=boxscoreCopy$game_id, team_id=boxscoreCopy$team_id), 
                             FUN=sum)
  minutesPerGame <- transform(minutesPerGame, 
                              game_time = GameTimeFromPlayerSummarizedMinutes(team_minutes))
  minutesPerGame <- transform(minutesPerGame, 
                              game_time_warning = (game_time * 5 > team_minutes))
  
  boxscoreCopy <- merge(minutesPerGame, boxscoreCopy, by=c("game_id", "team_id"))
                        
  # remove all columns starting with t_s or s_
  # This remove potentially interesting data such as fouls received,
  # blocks received and plusmninus. If you want to include those,
  # add them to the columns being renamed above.
  boxscoreCopy <- boxscoreCopy[ , -grep("(^s_)", names(boxscoreCopy)) ]
  
  return(boxscoreCopy)  
}

# returns a double with the expected minutes
GameTimeFromPlayerSummarizedMinutes <- function(minutes) {
  # only support 40 minute games for now
  # algorithm: to int, round up to the next multiple of 25 and divide by 5
  to <- 25
  result <- to*(minutes%/%to + as.logical(minutes%%to))
  return(result / 5)
}

GetAdvancedPlayerStats <- function(teamStats, player_boxscore) {

  playerStats <- merge(player_boxscore, 
                       teamStats[,c("game_id", "team_id"
                                    , "plays", "opp_ps"
                                    )],    # include team stats to add here
                       by=c("game_id", "team_id"))  
  
  # minute ratio - what percentage of the game was the player on the floor?
  playerStats <- transform(playerStats,
                           ply_minuteRatio = (ply_minutes) / (game_time)
  )
  

  # how many plays did the player use?
  playerStats <- transform(playerStats,
                           ply_plays = (ply_FG2A + ply_FG3A + ftaFactor * ply_FTA + ply_TO)
  )
  
  # usage percentage is the number of plays deployed by this player,
  # divided by the estimated number of plays that occured while the player was 
  # on the floor
  playerStats <- transform(playerStats,
                           ply_USGpct = (ply_plays) 
                           / (ply_minuteRatio * plays)
  )  
  
  # scoring
  playerStats <- transform(playerStats,
                           ply_PTS = ply_FTM + 2*ply_FG2M + 3*ply_FG3M)
  
  # advanced shooting indicators
  playerStats <- transform(playerStats,
                           ply_PPP = ply_PTS / ply_plays,
                           ply_FTperFG = ply_FTA / (ply_FG2A+ply_FG3A),
                           ply_FG3AperFG =  ply_FG3A / (ply_FG2A+ply_FG3A),
                           ply_EFGpct = (1.5*ply_FG3M + ply_FG2M) / (ply_FG2A+ply_FG3A),
                           ply_TSpct = (ply_PTS / (2 * (ply_FG2A + ply_FG3A + ftaFactor * ply_FTA)))
  )
  
  playerStats <- transform(playerStats,
                           ply_Finishes = (ply_plays + ply_Ast)
  )
  
  playerStats <- transform(playerStats,
                           ply_Astpct = (ply_Ast) 
                           / (ply_minuteRatio * (FG2M + FG3M) - ply_FG2M - ply_FG3M)
  )
  
  playerStats <- transform(playerStats,
                           ply_TRpct = (ply_DR+ply_OR) / (ply_minuteRatio * (DR + OR + opp_DR + opp_OR)),
                           ply_DRpct = (ply_DR) / (ply_minuteRatio * (DR + opp_OR)),
                           ply_ORpct = (ply_OR) / (ply_minuteRatio * (OR + opp_DR))
  )
  
  playerStats <- transform(playerStats,
                           ply_Stlpct = (ply_Stl) 
                           / (ply_minuteRatio * (opp_ps))
  )
  
  # note that block percentage is estimated using 2pt field goal attempts only
  playerStats <- transform(playerStats,
                           ply_Blkpct = (ply_Blk) 
                           / (ply_minuteRatio * (opp_FG2A))
  )
  
  return (playerStats)
}