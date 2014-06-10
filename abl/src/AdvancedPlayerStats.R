GetAdvancedPlayerStats <- function(sts, teamStats) {
  
  #######################################################################
  #
  # Calculate player performanceindicators by game
  # and join this frame on the teamStats
  #
  #######################################################################
  
  playerStats <- data.frame(sts$wed_ID
                            , sts$plg_ID, sts$scu_Minuten
                            , sts$scu_FTA, sts$scu_FTM, sts$scu_FGA, sts$scu_FGM, sts$scu_3PM  
                            , sts$scu_3PA 
                            , sts$scu_OffRebounds, sts$scu_DefRebounds, sts$scu_TurnOvers
                            , sts$spl_ID
                            , sts$scu_Fouten, sts$scu_Assists, sts$scu_Steals, sts$scu_Blocks
  )
  
  # prettify
  names(playerStats) <- sub("^sts.", "", names(playerStats))        
  names(playerStats) <- sub("scu_", "spl_", names(playerStats))
  names(playerStats) <- sub("OffRebounds", "OR", names(playerStats))
  names(playerStats) <- sub("DefRebounds", "DR", names(playerStats))
  names(playerStats) <- sub("TurnOvers", "TO", names(playerStats))
  names(playerStats) <- sub("FG", "FG2", names(playerStats))
  names(playerStats) <- sub("3P", "FG3", names(playerStats))
  names(playerStats) <- sub("Fouten", "PF", names(playerStats))
  names(playerStats) <- sub("Assists", "Ast", names(playerStats))
  names(playerStats) <- sub("Steals", "Stl", names(playerStats))
  names(playerStats) <- sub("Blocks", "Blk", names(playerStats))
  
  sqlPlayersJoinTeam <- paste("select * from teamStats tm ",
                              "inner join playerStats ply on ",
                              "tm.wed_ID = ply.wed_ID and tm.plg_ID = ply.plg_ID ", 
                              "order by wed_ID")
  
  playerStats <- sqldf(sqlPlayersJoinTeam)
  
  
  # minute ratio - what percentage of the game was the player on the floor?
  playerStats <- transform(playerStats,
                           spl_MinutesRatio = (spl_Minuten) / (Minuten / 5)
  )
  
  # how many plays did the player use?
  playerStats <- transform(playerStats,
                           spl_Plays = (spl_FG2A + spl_FG3A + ftaFactor * spl_FTA + spl_TO)
  )
  
  # usage percentage is the number of plays deployed by this player,
  # divided by the estimated number of plays that occured while the player was 
  # on the floor
  playerStats <- transform(playerStats,
                           spl_USGpct = (spl_Plays) 
                           / (spl_MinutesRatio * plays)
  )
  
  # scoring
  playerStats <- transform(playerStats,
                           spl_PTS = spl_FTM + 2*spl_FG2M + 3*spl_FG3M)
  
  # advanced shooting indicators
  playerStats <- transform(playerStats,
                           spl_PPP = spl_PTS / spl_Plays,
                           spl_FTperFG = spl_FTA / (spl_FG2A+spl_FG3A),
                           spl_FG3AperFG =  spl_FG3A / (spl_FG2A+spl_FG3A),
                           spl_EFGpct = (1.5*spl_FG3M + spl_FG2M) / (spl_FG2A+spl_FG3A),
                           spl_TSpct = (spl_PTS / (2 * (spl_FG2A + spl_FG3A + ftaFactor * spl_FTA)))
  )
  
  playerStats <- transform(playerStats,
                           spl_Finishes = (spl_Plays + spl_Ast)
  )
  
  playerStats <- transform(playerStats,
                           spl_Astpct = (spl_Ast) 
                           / (spl_MinutesRatio * (FG2M + FG3M) - spl_FG2M - spl_FG3M)
  )
  
  playerStats <- transform(playerStats,
                           spl_TRpct = (spl_DR+spl_OR) / (spl_MinutesRatio * (DR + OR + opp_DR + opp_OR)),
                           spl_DRpct = (spl_DR) / (spl_MinutesRatio * (DR + opp_OR)),
                           spl_ORpct = (spl_OR) / (spl_MinutesRatio * (OR + opp_DR))
  )
  
  playerStats <- transform(playerStats,
                           spl_Stlpct = (spl_Stl) 
                           / (spl_MinutesRatio * (opp_ps))
  )
  
  # note that block percentage is estimated using 2pt field goal attempts only
  playerStats <- transform(playerStats,
                           spl_Blkpct = (spl_Blk) 
                           / (spl_MinutesRatio * (opp_FG2A))
  )
  
  return (playerStats)
}

CheckMinutesPlayed <- function(sts) {
  
  minutesNotEqual <- sqldf(paste("select wed_ID, plg_name, wed_ThuisPloeg, wed_UitPloeg, Minuten, opp_minuten",
                                 "from sts ",
                                 "where Minuten <> opp_minuten"))
  
  nrGamesWithUnEqualMinutes <- (nrow(minutesNotEqual) / 2)
  if(nrGamesWithUnEqualMinutes > 0) {
    wrn <- sprintf("%i game(s) with unequal minutes per team:", nrGamesWithUnEqualMinutes)
    warning(wrn)
    #uncomment next line for inspection:
    #print(minutesNotEqual)
  }
  
  # Although in every competition there is a certain number of games
  # where there clearly is an administrative error in the minutes played,
  # I choose not to correct for this at the moment.
  # I found two type of minutes played errors; I'll describe both:
  #
  # Ad.1 - 2011-12, Aris-Weert:
  # wed_ID               plg_Name  Minuten opp_Minuten
  # 557264 Basketball Stars Weert      197         200
  # 
  # Here, Weet plays a total of 197 minutes against 200 opp minutes;
  # I assume, because Weert finished the game with 4 or even fewer players.
  # No correction required
  #
  # Ad.2 - 2011-12, Aris-GasTerra
  # wed_ID               plg_Name  Minuten opp_Minuten
  # 557288                BV Aris     173         227
  # 
  # Here it appears as if 27 minutes of Aris were accidentally accounted to Aris.
  # However unless we investigate this further, we can't really reconstruct what
  # happened, or how we should fix this.
  # Maybe GT number 9 player was mixed up with Aris #9? How should we fix this?
  # We can not _systematically_ find the error by simply looking at the data,
  # so fairest seems to be to reditribute the minutes among all player according
  # to the current minutes played ratio. But if we do this, the estimation 
  # parameter (player minutes / team minutes) will not change.
  # and if we keep it unchanged, the accounted error will only be applied to 
  # the players involved in error - unlike the situation where we change 
  # everybody's minutes: then we can _be sure_ everybody will have an error!
}
