# Documentation

[Justinas' mail](https://mail.google.com/mail/u/1/#inbox/145d22fbf645d140)

## Legend

Please find exported files attached. There is pdf specification of play-by-play data and coordinate system of the court. ZIP archive contains the following:
Teams
Persons - person data with no relation to teams. Unfortunatelly, player weight is not available (not entered into the system)
Roster - person relation to teams. You can track player transfers between teams by start/end date fields.
Games
Boxscores-teams - team data from boxscores. The following fields are available:
t_*_a - total statistics of home team
t_*_b - total statistics of away team
s_*_a - team statistics (not assigned to any player) of home team
s_*_b - team statistics (not assigned to any player) of away team
Boxscores-players - player data from boxscores
Play-by-play - please check pdf document for meanings of log_param_* fields for different types of actions. This file does not contain player ids - you need to map actions by player numbers from boxscores.
List of statistical categories:

```
2m - 2 points made             : FG2M
2a - 2 points attempted        : FG2A
3m - 3 points made             : FG3M 
3a - 3 points attempted        : FG3A
fgm - field goals made         : -
fga - field goals attempted    : -
1m - free throws made          : FTM
1a - free throws attempted     : FTA
ro - offensive rebounds        : OR
rd - defensive rebounds        : DR
rt - total rebounds            : - 
as - assists                   : AST
pf_cm - fouls commited         : PF
pf_rv - fouls received         : -
bl_fv - blocks in favor        : BLK
bl_ag - blocks against         : -
to - turnovers                 : TO
st - steals                    : ST
pts - points                   : pts
eq - efficiency                : -
plusminus - +/- statistics     : -
```

## Calculations


### Team statistics

read home team game frame (boxscores-teams.csv)
let teamId = team id of home team
rename away team stats to opp_[stat]
let home = true
let WIN = pts_home>pts_away

read away team game frame (boxscores-teams.csv)   #same file
let teamId = team id of away team
rename home team stats to opp_[stat]
let home = false
let WIN = pts_away>pts_home

Note: duration of game an only be detemrined by summing player minutes; take this into account when calculating the pace.

The team fram can be identical to the team fram used for the calculations for the DBL; so calculations can and be reused.






