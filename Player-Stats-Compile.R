
############################################################################################################################################################################
######## 0.A SYSTEM PREP
############################################################################################################################################################################
library(ggplot2);library(dplyr); library(DataCombine)
library(glmnet); library(nhlscrapr); library(caret); library(RMySQL); library(readr); library(reshape2); library(rvest)
library(twitteR);library(httr); library(data.table); library("reshape2")


source("/Users/colander1/Documents/CWA/R Code/operations.R")

skater.name.xwalk <- read_csv("https://raw.githubusercontent.com/C92Anderson/xG-Model/master/skater_name_xwalk.csv")

### Load player elo ratings
library(ggplot2)
library(RMySQL)
library(dplyr)
library(scales)

conn <- dbConnect(MySQL(), user='ca_elo_games', password='cprice31!',
                  host='mysql.crowdscoutsports.com', db='nhl_all')
on.exit(dbDisconnect(conn))


### Load Gaolie/Skater Roster with Handedness
skater.roster <- dbGetQuery(conn, "SELECT distinct upper(playerName) as Player, 
                            playerId as shooterID,  
                            playerPositionCode as `Player.Position`,
                            playerShootsCatches as Shoots,
                            playerBirthDate as shooterDOB
                            FROM hockey_roster_info AS B
                            WHERE playerPositionCode != 'G'") %>%
  filter(!shooterID %in% c(8474744,8466208,8471747,8468436,8466155,8476979,8471221)) %>%
  unique() %>%
  mutate_all(funs(gsub("MATTHEW ","MATT ", .))) %>%
  mutate_all(funs(gsub("PIERRE-ALEXANDRE ","PA ", .))) 


goalie.roster <- dbGetQuery(conn, "SELECT distinct upper(playerName) as `SA.Goalie`, 
                            playerId as goalieID,  
                            playerHeight as goalieHeight, 
                            playerShootsCatches as Catches,
                            playerBirthDate as goalieDOB
                            FROM hockey_roster_info AS B
                            WHERE playerPositionCode = 'G'") %>% 
  unique()


player_elo <- dbGetQuery(conn, "SELECT ID, upper(Player) as Player,
                         CASE WHEN Date <= '2016-10-01' THEN '20152016'
                              WHEN Date <= '2017-10-01' THEN '20162017'
                              WHEN Date <= '2018-10-01' THEN '20172018'
                              WHEN Date <= '2019-10-01' THEN '20182019'
                              WHEN Date <= '2020-10-01' THEN '20192020'
                              WHEN Date <= '2021-10-01' THEN '20202021' 
                              WHEN Date <= '2022-10-01' THEN '20212022' END AS season, avg(score) as Score
                         FROM hockey_daily_elo_py AS A
                         WHERE Date >= '2015-10-01'
                         GROUP BY 1,2,3")

roster_current <- dbGetQuery(conn, "SELECT distinct upper(player_name) as Player, team, '20162017' as season, nhl_id as ID,  pos as Pos FROM hockey_roster_v1 AS A") %>%
  mutate(Player = gsub("MATTHEW ","MATT ", Player))

roster <- dbGetQuery(conn, "SELECT distinct upper(player_name) as Player, nhl_id as ID,  pos as Pos, team FROM hockey_roster_v1 AS A")

draft_all <- dbGetQuery(conn, "SELECT distinct upper(playerName) as Player, playerId as ID1,  playerPositionCode as Pos1 FROM hockey_roster_info AS B")
    
roster_all <- roster %>%
            full_join(draft_all, by="Player") %>%
            mutate(Player = gsub("MATTHEW ","MATT ", Player),
                   Pos = ifelse(!is.na(Pos),Pos,Pos1),
                   ID = ifelse(!is.na(ID),ID,ID1),
                   Pos = ifelse(Player %in% c("MIKKO LEHTONEN","ALEXANDRE PICARD","SEAN COLLINS"),"F",Pos)) %>%
            select(Player, ID, Pos) %>%
            filter(!ID %in% c("8466155","8466208","8471747","8471221","8474744")) %>%  ## Double Mike Brown, Petr Sykora, MIKKO LEHTONEN, ALEXANDRE PICARD, SEAN COLLINS
            distinct()

## Replacement Level Player by Year
replacement <- dbGetQuery(conn, "SELECT
                         CASE WHEN Date <= '2016-10-01' THEN '20152016'
                         WHEN Date <= '2017-10-01' THEN '20162017'
                         WHEN Date <= '2018-10-01' THEN '20172018'
                         WHEN Date <= '2019-10-01' THEN '20182019'
                         WHEN Date <= '2020-10-01' THEN '20192020'
                         WHEN Date <= '2021-10-01' THEN '20202021' 
                         WHEN Date <= '2022-10-01' THEN '20212022' END AS season, (1500 - min(elo)) / (max(elo) - min(elo)) * 100 as replacement
                         FROM hockey_daily_elo_py
                         WHERE Date >= '2015-10-01'
                         GROUP BY 1")

replacement <- mean(replacement$replacement) %>% as.character()
replacement <- 35

## Get prior player level predictions
crowdscout_data_predictions <- dbGetQuery(conn, "SELECT *
                         FROM crowdscout_data_predictions")

save(crowdscout_data_predictions, file="~/Documents/CWA/Hockey Data/crowdscout_data_predictions.RData")
save(player_elo, file="~/Documents/CWA/Hockey Data/player_elo.RData")

# Load Scored Data
load("~/Documents/CWA/Hockey Data/xG.scored.data.RData")
load("~/Documents/CWA/Hockey Data/pbp.all.raw.RData")

### Join xG to all PBP data
pbp.all.xG <- scored.data %>%
  select(etype, season, gcode, seconds, event, xG, xG.team) %>%
  right_join(unique(pbp.all.raw), by = c("season","gcode","seconds","etype","event")) %>%
  mutate(goal = ifelse(etype=="GOAL",1,0),
         xG = ifelse(is.na(xG),0,as.numeric(xG)),
         ## Duration
         same.period = ifelse(gcode == lag(gcode) & period == lag(period), 1, 0),                    
         duration = ifelse(same.period == 1, seconds - lag(seconds), 0) ) %>%
  # Remove Regular Season Shootouts
  filter(!(period== "5" & substr(gcode,1,1) == "2")) 
  

### Data
player.data <- pbp.all.xG #%>% filter(season %in% c("20142015","20152016","20162017"))

### Standardize Player Names
pnm <- player.data  %>%
  select(h1,h2,h3,h4,h5,h6,a1,a2,a3,a4,a5,a6, away.G,home.G, ev.player.1, ev.player.2, ev.player.3) %>%
  mutate_all(funs(toupper(trimws(substr( . , 3, nchar( . )))))) %>%
  mutate_all(funs(gsub("MATTHEW ","MATT ", .))) %>%
  mutate_all(funs(gsub("PIERRE-ALEXANDRE ","PA ", .))) 

#################################### 
### Quality of Teammates/Competition
#################################### 
player.level.quality <- crowdscout_data_predictions[c("Player","season","Predicted.CS.RF")] %>%
                    group_by(Player,season) %>%  ### Collapse on Mikko Ratonen & Alex Picard
                    summarise(Predicted.CS.RF = mean(Predicted.CS.RF))

quality.onice <- cbind(season = player.data$season, gcode = player.data$gcode, pnm, duration = player.data$duration) %>%
  select(-c(away.G,home.G, ev.player.1, ev.player.2, ev.player.3)) %>%
  #left_join(replacement, by="season") %>%
  cbind(replacement) %>%
  mutate(replacement = ifelse(is.na(replacement),mean(replacement$replacement),replacement)) %>%
  ## H1
  left_join(player.level.quality, by = c("h1" = "Player", "season" = "season")) %>%
  mutate(h1_elo = ifelse(is.na(Predicted.CS.RF) & nchar(h1) > 1,replacement,Predicted.CS.RF)) %>% select(-Predicted.CS.RF) %>%
  ## H2
  left_join(player.level.quality, by = c("h2" = "Player", "season" = "season")) %>%
  mutate(h2_elo = ifelse(is.na(Predicted.CS.RF) & nchar(h2) > 1,replacement,Predicted.CS.RF)) %>% select(-Predicted.CS.RF) %>%
  ## H3
  left_join(player.level.quality, by = c("h3" = "Player", "season" = "season")) %>%
  mutate(h3_elo = ifelse(is.na(Predicted.CS.RF) & nchar(h3) > 1,replacement,Predicted.CS.RF)) %>% select(-Predicted.CS.RF) %>%
  ## H4
  left_join(player.level.quality, by = c("h4" = "Player", "season" = "season")) %>%
  mutate(h4_elo = ifelse(is.na(Predicted.CS.RF) & nchar(h4) > 1,replacement,Predicted.CS.RF)) %>% select(-Predicted.CS.RF) %>%
  ## H5
  left_join(player.level.quality, by = c("h5" = "Player", "season" = "season")) %>%
  mutate(h5_elo = ifelse(is.na(Predicted.CS.RF) & nchar(h5) > 1,replacement,Predicted.CS.RF)) %>% select(-Predicted.CS.RF) %>%
  ## H6
  left_join(player.level.quality, by = c("h6" = "Player", "season" = "season")) %>%
  mutate(h6_elo = ifelse(is.na(Predicted.CS.RF) & nchar(h6) > 1,replacement,Predicted.CS.RF)) %>% select(-Predicted.CS.RF) %>%
  ## A1
  left_join(player.level.quality, by = c("a1" = "Player", "season" = "season")) %>%
  mutate(a1_elo = ifelse(is.na(Predicted.CS.RF) & nchar(a1) > 1,replacement,Predicted.CS.RF)) %>% select(-Predicted.CS.RF) %>%
  ## A2
  left_join(player.level.quality, by = c("a2" = "Player", "season" = "season")) %>%
  mutate(a2_elo = ifelse(is.na(Predicted.CS.RF) & nchar(a2) > 1,replacement,Predicted.CS.RF)) %>% select(-Predicted.CS.RF) %>%
  ## A3
  left_join(player.level.quality, by = c("a3" = "Player", "season" = "season")) %>%
  mutate(a3_elo = ifelse(is.na(Predicted.CS.RF) & nchar(a3) > 1,replacement,Predicted.CS.RF)) %>% select(-Predicted.CS.RF) %>%
  ## A4
  left_join(player.level.quality, by = c("a4" = "Player", "season" = "season")) %>%
  mutate(a4_elo = ifelse(is.na(Predicted.CS.RF) & nchar(a4) > 1,replacement,Predicted.CS.RF)) %>% select(-Predicted.CS.RF) %>%
  ## A5
  left_join(player.level.quality, by = c("a5" = "Player", "season" = "season")) %>%
  mutate(a5_elo = ifelse(is.na(Predicted.CS.RF) & nchar(a5) > 1,replacement,Predicted.CS.RF)) %>% select(-Predicted.CS.RF) %>%
  ## A6
  left_join(player.level.quality, by = c("a6" = "Player", "season" = "season")) %>%
  mutate(a6_elo = ifelse(is.na(Predicted.CS.RF) & nchar(a6) > 1,replacement,Predicted.CS.RF)) %>% select(-Predicted.CS.RF)


### Find Unique Skaters
skater.list <- unique(c(unique(c(pnm[1])[[1]]),
                          unique(c(pnm[2])[[1]]),
                          unique(c(pnm[3])[[1]]),
                          unique(c(pnm[4])[[1]]),
                          unique(c(pnm[5])[[1]]),
                          unique(c(pnm[6])[[1]]),
                          unique(c(pnm[7])[[1]]),
                          unique(c(pnm[8])[[1]]),
                          unique(c(pnm[9])[[1]]),
                          unique(c(pnm[10])[[1]]),
                          unique(c(pnm[11])[[1]]),
                          unique(c(pnm[12])[[1]]))) 
  
skater.list <- skater.list[skater.list != ""]

skater.list %>% as.data.frame() %>%
          inner_join(skater.name.xwalk, c("."="Player1")) %>%
          arrange(Player_clean) %>%
          print()
          
quality.onice_elos <- quality.onice %>%
          select(ends_with("elo"))

### Clean Data               
player.data.clean <- player.data %>% 
      select(season, gcode, refdate, period, ev.team, homezone, etype, seconds, xG.team, xG, hometeam, awayteam, home.score, away.score) %>%
      cbind(pnm, quality.onice_elos) %>%
      mutate(awaystate = ifelse(nchar(a4) == 0, 3,
                     ifelse(nchar(a5) == 0, 4,
                            ifelse(nchar(a6) == 0, 5,
                                   6))),
            homestate = ifelse(nchar(h4) == 0, 3,
                               ifelse(nchar(h5) == 0, 4,
                                      ifelse(nchar(h6) == 0, 5,        
                                             6))),
            home.gamestate = paste0(homestate,"v",awaystate),
            home.gamestate = ifelse(home.gamestate %in% c("3v5","3v4","3v6","4v5","4v6","5v6","4v5"),"SH",
                               ifelse(home.gamestate %in% c("6v3","6v4","5v3","6v5","5v4","4v3"),"PP",
                                      ifelse(home.gamestate %in% c("5v5","6v6","4v4","3v3"),"EV",
                                             home.gamestate))),
            ice.time = ifelse(lag(gcode) != gcode | is.na(lag(gcode)), 0, seconds - lag(seconds))) 
  
##############################################################################
##############################################################################    
###For each skater count
##############################################################################
##############################################################################
skater.stats <- function(i) {
    
    print(i)
  
    j <- skater.name.xwalk %>% filter(Player_clean == i) %>% select(Player1) %>% as.character()
    ### All shifts player is on-ice for
    player.ice <- player.data.clean %>%
        filter(a1 %in% c(i,j) | a2 %in% c(i,j) | a3 %in% c(i,j) | a4 %in% c(i,j) | a5 %in% c(i,j) | a6 %in% c(i,j) 
               | h1 %in% c(i,j) | h2 %in% c(i,j) | h3 %in% c(i,j) | h4 %in% c(i,j) | h5 %in% c(i,j) | h6 %in% c(i,j) ) %>%
        mutate(player.team = ifelse(a1 %in% c(i,j) | a2 %in% c(i,j) | a3 %in% c(i,j) | a4 %in% c(i,j) | a5 %in% c(i,j) | a6 %in% c(i,j),
                                     awayteam, hometeam),
               player.team.state = ifelse(player.team == hometeam | home.gamestate == "EV",home.gamestate,
                                   ifelse(home.gamestate == "SH" & player.team != hometeam,"PP",
                                   ifelse(home.gamestate == "PP" & player.team != hometeam,"SH",
                                          ""))))
  
    ### Find all shifts with player  
    player.bystrength.prep <- player.ice %>% 
      mutate(PlayerVenue = ifelse(player.team == hometeam,"Home",
                           ifelse(player.team == awayteam, "Away","")),
             Team.Score = ifelse(player.team == hometeam, home.score, away.score),
             Opposition.Score = ifelse(player.team != hometeam, home.score, away.score),
             Team.Strength = ifelse(player.team == hometeam, homestate, awaystate),
             Opposition.Strength = ifelse(player.team != hometeam, homestate, awaystate),
             Player.Score.State = Team.Score - Opposition.Score,
             FO.Shift = ifelse(etype == "FAC", 1, 0),
             OTF.Shift = ifelse((seconds - lag(seconds)) != ice.time & FO.Shift != 1, 1, 0),
             OTF.Shift = ifelse(is.na(OTF.Shift),0,OTF.Shift),
             Shift.Start = ifelse(FO.Shift == 1 | OTF.Shift == 1, 1, 0),
             Player.Shift.Number = cumsum(Shift.Start),
             Off.FO.Shift = ifelse(FO.Shift == 1 & ((PlayerVenue == "Home" & homezone == "Off") | (PlayerVenue == "Away" & homezone == "Def")),1,0),
             Def.FO.Shift = ifelse(FO.Shift == 1 & ((PlayerVenue == "Home" & homezone == "Def") | (PlayerVenue == "Away" & homezone == "Off")),1,0),
             Neu.FO.Shift = ifelse(FO.Shift == 1 & homezone == "Neu",1,0),
             G = ifelse(etype == "GOAL" & ev.player.1 %in% c(i,j), 1, 0),
             A1 = ifelse(etype == "GOAL" & ev.player.2 %in% c(i,j), 1, 0),
             A2 = ifelse(etype == "GOAL" & ev.player.3 %in% c(i,j), 1, 0),
             ixG = ifelse(ev.player.1 %in% c(i,j), xG, 0),
             xGF = ifelse(player.team == ev.team, xG.team, 0),
             xGA = ifelse(player.team != ev.team, xG.team, 0),
             
             GF = ifelse(player.team == ev.team & etype %in% c("GOAL"),1,0),
             GA = ifelse(player.team != ev.team & etype %in% c("GOAL"),1,0),
             
             CF = ifelse(player.team == ev.team & etype %in% c("BLOCK","MISS","SHOT","GOAL"),1,0),
             CA = ifelse(player.team != ev.team & etype %in% c("BLOCK","MISS","SHOT","GOAL"),1,0))
      
      player.bystrength <- player.bystrength.prep %>%
          group_by(season, player.team.state) %>%
          summarise(Games.Played = uniqueN(gcode),
                    TOI = sum(ice.time),
                    
                Total.Shifts = sum(FO.Shift + OTF.Shift, na.rm = TRUE),
                OTF.Shift.Share = sum(OTF.Shift) / sum(FO.Shift + OTF.Shift),
                Off.FO.Shift.Share = sum(Off.FO.Shift) / sum(FO.Shift + OTF.Shift),
                Def.FO.Shift.Share = sum(Def.FO.Shift) / sum(FO.Shift + OTF.Shift),
                Neu.FO.Shift.Share = sum(Neu.FO.Shift) / sum(FO.Shift + OTF.Shift),
                ixG60 = sum(ixG, na.rm = TRUE) / (sum(ice.time) / 3600),
                G60 = sum(G, na.rm = TRUE) / (sum(ice.time) / 3600),
                A160 = sum(A1, na.rm = TRUE) / (sum(ice.time) / 3600),
                A260 = sum(A2, na.rm = TRUE) / (sum(ice.time) / 3600),
                
                xGF60 = sum(xGF, na.rm = TRUE) / (sum(ice.time) / 3600),
                xGA60 = sum(xGA, na.rm = TRUE) / (sum(ice.time) / 3600),
                CF60 = sum(CF, na.rm = TRUE) / (sum(ice.time) / 3600),
                CA60 = sum(CA, na.rm = TRUE) / (sum(ice.time) / 3600))   
    
      player.bystrength.venue <- player.bystrength.prep %>%
        group_by(season, player.team.state, PlayerVenue) %>%
        summarise(ixG60 = sum(ixG, na.rm = TRUE) / (sum(ice.time) / 3600),
                  G60 = sum(G, na.rm = TRUE) / (sum(ice.time) / 3600),
                  P160 = (sum(G, na.rm = TRUE) + sum(A1, na.rm = TRUE)) / (sum(ice.time) / 3600),
                  P260 = (sum(G, na.rm = TRUE) + sum(A1, na.rm = TRUE) + sum(A2, na.rm = TRUE)) / (sum(ice.time) / 3600),
                  
                  xGF60 = sum(xGF, na.rm = TRUE) / (sum(ice.time) / 3600),
                  xGA60 = sum(xGA, na.rm = TRUE) / (sum(ice.time) / 3600)) %>%
        melt(id.vars = c("season","player.team.state","PlayerVenue")) %>%
        mutate(variable = paste0(variable,"_",PlayerVenue)) %>%
        dcast(season + player.team.state ~ variable, value.var = "value")
      
    team.player.games <- player.ice %>%
        select(player.team, season, gcode) %>%
        unique() 
    
    ### Find all shifts for team without player
    team_without.bystrength <- player.data.clean %>%
        inner_join(team.player.games, by=c("season","gcode")) %>%
        filter(a1 != i & a2 != i & a3 != i & a4 != i & a5 != i & a6 != i 
             & h1 != i & h2 != i & h3 != i & h4 != i & h5 != i & h6 != i) %>%
        mutate(player.team.state = ifelse(player.team == hometeam | home.gamestate == "EV",home.gamestate,
                                        ifelse(home.gamestate == "SH" & player.team != hometeam,"PP",
                                               ifelse(home.gamestate == "PP" & player.team != hometeam,"SH",
                                                      ""))),
               xGF = ifelse(player.team == ev.team, xG.team, 0),
               xGA = ifelse(player.team != ev.team, xG.team, 0),
               CF = ifelse(player.team == ev.team & etype %in% c("BLOCK","MISS","SHOT","GOAL"),1,0),
               CA = ifelse(player.team != ev.team & etype %in% c("BLOCK","MISS","SHOT","GOAL"),1,0)) %>%
        group_by(season, player.team.state) %>%
        summarise(team.time.wo = sum(ice.time),
                  xGF60_teamWO = sum(xGF, na.rm = TRUE) / (sum(ice.time) / 3600),
                  xGA60_teamWO = sum(xGA, na.rm = TRUE) / (sum(ice.time) / 3600),
                  CF60_teamWO = sum(CF, na.rm = TRUE) / (sum(ice.time) / 3600),
                  CA60_teamWO = sum(CA, na.rm = TRUE) / (sum(ice.time) / 3600)) 
 
    player.QOT.QOC <- cbind(player.data.clean[c("awayteam","hometeam","home.gamestate","ice.time","xG","ev.team")],quality.onice) %>%
            inner_join(team.player.games, by=c("season","gcode")) %>%
            mutate(player.team.state = ifelse(player.team == hometeam | home.gamestate == "EV",home.gamestate,
                                       ifelse(home.gamestate == "SH" & player.team != hometeam,"PP",
                                       ifelse(home.gamestate == "SH" & player.team == hometeam,"SH",
                                       ifelse(home.gamestate == "PP" & player.team == hometeam,"PP",
                                       ifelse(home.gamestate == "PP" & player.team != hometeam,"SH",
                                                      ""))))),
                   PlayerOnIce = ifelse(a1 %in% c(i,j) | a2 %in% c(i,j) | a3 %in% c(i,j) | a4 %in% c(i,j) | a5 %in% c(i,j) | a6 %in% c(i,j)
                        | h1 %in% c(i,j) | h2 %in% c(i,j) | h3 %in% c(i,j) | h4 %in% c(i,j) | h5 %in% c(i,j) | h6 %in% c(i,j), "Player", "TeamWO"),
                   h1_elo = ifelse(h1 %in% c(i,j), NA,h1_elo),
                   h2_elo = ifelse(h2 %in% c(i,j), NA,h2_elo),
                   h3_elo = ifelse(h3 %in% c(i,j), NA,h3_elo),
                   h4_elo = ifelse(h4 %in% c(i,j), NA,h4_elo),
                   h5_elo = ifelse(h5 %in% c(i,j), NA,h5_elo),
                   h6_elo = ifelse(h6 %in% c(i,j), NA,h6_elo),
                   a1_elo = ifelse(a1 %in% c(i,j), NA,a1_elo),
                   a2_elo = ifelse(a2 %in% c(i,j), NA,a2_elo),
                   a3_elo = ifelse(a3 %in% c(i,j), NA,a3_elo),
                   a4_elo = ifelse(a4 %in% c(i,j), NA,a4_elo),
                   a5_elo = ifelse(a5 %in% c(i,j), NA,a5_elo),
                   a6_elo = ifelse(a6 %in% c(i,j), NA,a6_elo),
                   PlayerVenue = ifelse(player.team == hometeam,"Home",
                                 ifelse(player.team == awayteam, "Away",""))) %>%
                         ungroup()

        away.players <- player.QOT.QOC %>% select(ends_with("elo")) %>% select(starts_with("a")) %>% rowMeans(na.rm = T) %>% as.data.frame()
        colnames(away.players) <- "away.players"
        home.players <- player.QOT.QOC %>% select(ends_with("elo")) %>% select(starts_with("h")) %>% rowMeans(na.rm = T) %>% as.data.frame()
        colnames(home.players) <- "home.players"

       ## Quality of Team / Competition
        QOC.QOT.data <- player.QOT.QOC %>%
                  select(-starts_with("a"),
                         -starts_with("h")) %>%
                  data.frame(. , away.players, home.players) %>%
                  mutate(Teammates = ifelse(PlayerVenue == "Home", home.players, away.players),
                         Competition = ifelse(PlayerVenue == "Away", home.players, away.players),
                         xG.For = ifelse(player.team == ev.team, xG, 0),
                         xG.Against = ifelse(player.team != ev.team, xG, 0),
                         xG.Diff = xG.For - xG.Against,
                         xG.Pct = xG.For / (xG.For + xG.Against)) %>%
                  filter(Teammates > 0 & Competition > 0) 

        QOC.QOT.sums <- QOC.QOT.data %>%
          group_by(season, player.team.state, PlayerOnIce) %>%
                  summarise(Teammates = weighted.mean(Teammates, w = ice.time),
                            Competition = weighted.mean(Competition, w = ice.time)) %>%
                  melt(id.vars = c("season","player.team.state","PlayerOnIce")) %>%
                  mutate(variable = paste0(PlayerOnIce,"_",variable)) %>%
                  dcast(season + player.team.state ~ variable, value.var = "value")

        ## Home/Away Competition
        QOC.QOT.sums.venue <- QOC.QOT.data %>%
          ungroup() %>%
          group_by(season, PlayerVenue, player.team.state, PlayerOnIce) %>%
          summarise(Teammates = weighted.mean(Teammates, w = ice.time),
                    Competition = weighted.mean(Competition, w = ice.time)) %>%
          melt(id.vars = c("season","player.team.state","PlayerOnIce","PlayerVenue")) %>%
          mutate(variable = paste0(PlayerOnIce,"_",variable,"_",PlayerVenue)) %>%
          dcast(season + player.team.state ~ variable, value.var = "value")

        # ### 1D Density
        # QOC.QOT.distro <- QOC.QOT.data %>%
        #   filter(season == "20162017") %>%
        #   select(season,player.team.state,PlayerOnIce, Competition, Teammates) %>%
        #   melt(id.vars = c("season","player.team.state","PlayerOnIce")) %>%
        #     mutate(variable = paste0(PlayerOnIce,"_",variable)) %>%
        #     ggplot() +
        #     geom_density(aes(value, group=variable, color=variable, linetype=variable)) +
        #     facet_wrap(~season + player.team.state) +
        #     labs(title=paste0(i, " - Quality of Teammates and Competition"), x="Quality of Competition/Teammates (Predicted Crowdscout Score, 0-100)",
        #                       y="Density", color="",fill="",linetype="") +
        #   theme(legend.position = "top")
        # 
        # ### 2D Geom Density
        # QOC.QOT.matrix1 <- QOC.QOT.data %>%
        #   #filter(season == "20162017") %>%
        #   filter(player.team.state == "EV") %>%
        #   mutate(Competition = round(Competition,0),Teammates = round(Teammates,0)) %>%
        #   select(season,player.team.state,PlayerOnIce,PlayerVenue, Competition, Teammates) %>%
        #   melt(id.vars = c("season","player.team.state","PlayerOnIce","PlayerVenue","Competition","Teammates")) %>%
        #   ggplot() +
        #   annotate("segment",x=25,xend=75,y=25,yend=75, color="grey70") +
        #   annotate("text",x=25,y=75, label="Sheltered", color="grey50", size=2.5) +
        #   annotate("text",x=75,y=25, label="Buried", color="grey50", size=2.5) +
        #   geom_density_2d(aes(x=Competition, y=Teammates, group=PlayerVenue,color=PlayerVenue, linetype=PlayerVenue)) +
        #   facet_wrap(season ~ player.team.state) +
        #   labs(title=paste0(i, " - Quality of Teammates and Competition"), x="Quality of Competition (Predicted Crowdscout Score, 0-100)",
        #        y="Quality of Teammates (Predicted Crowdscout Score, 0-100)", color="",linetype="") +
        #   theme(legend.position = "top")
        # 
        # ### 2D with xG
        # QOC.QOT.matrix2 <- QOC.QOT.data %>%
        #   filter(season == "20162017") %>%
        #   mutate(Competition = round(Competition,0),Teammates = round(Teammates,0)) %>%
        #   group_by(season, player.team.state, PlayerOnIce, Competition, Teammates) %>%
        #   summarise(ice.time = sum(ice.time),
        #             xG.Diff = sum(xG.For) - sum(xG.Against)) %>%
        #   ggplot() +
        #   annotate("segment",x=25,xend=75,y=25,yend=75, color="grey70") +
        #   annotate("text",x=25,y=75, label="Sheltered", color="grey50", size=2.5) +
        #   annotate("text",x=75,y=25, label="Buried", color="grey50", size=2.5) +
        # geom_point(aes(x=Competition, y=Teammates, size=ice.time / 60, color=xG.Diff, alpha=ice.time / 60)) +
        #   facet_wrap(~season + player.team.state) +
        #   labs(title=paste0(i, " - Quality of Teammates and Competition"), x="Quality of Competition (Predicted Crowdscout Score, 0-100)",
        #        y="Quality of Teammates (Predicted Crowdscout Score, 0-100)", color="Absolute xG Differential", size="Time on Ice",alpha="Time on Ice") +
        #   scale_color_gradient2(low="coral4",mid="grey50",high="darkorchid4", midpoint = 0) +
        #   theme(legend.position = "top")

            
     ## Adjust by team 
     player.bystrength.adj <- player.bystrength %>%
          inner_join(team_without.bystrength, by=c("season","player.team.state")) %>%
          inner_join(player.bystrength.venue, by=c("season","player.team.state")) %>%
          inner_join(QOC.QOT.sums, by=c("season","player.team.state")) %>%
          inner_join(QOC.QOT.sums.venue, by=c("season","player.team.state")) %>%
          mutate(Player = i,
                 Share.of.Ice = TOI / (TOI + team.time.wo))
     
     
     #### Player Shift Level Data
     
     # player.shift.level.prep <- player.bystrength.prep %>%
     #      mutate( xGF = ifelse(is.na(xGF),0, xGF),
     #              xGA = ifelse(is.na(xGF),0, xGA),
     #              h1_elo = ifelse(h1 %in% c(i,j), NA,h1_elo),
     #              h2_elo = ifelse(h2 %in% c(i,j), NA,h2_elo),
     #              h3_elo = ifelse(h3 %in% c(i,j), NA,h3_elo),
     #              h4_elo = ifelse(h4 %in% c(i,j), NA,h4_elo),
     #              h5_elo = ifelse(h5 %in% c(i,j), NA,h5_elo),
     #              h6_elo = ifelse(h6 %in% c(i,j), NA,h6_elo),
     #              a1_elo = ifelse(a1 %in% c(i,j), NA,a1_elo),
     #              a2_elo = ifelse(a2 %in% c(i,j), NA,a2_elo),
     #              a3_elo = ifelse(a3 %in% c(i,j), NA,a3_elo),
     #              a4_elo = ifelse(a4 %in% c(i,j), NA,a4_elo),
     #              a5_elo = ifelse(a5 %in% c(i,j), NA,a5_elo),
     #              a6_elo = ifelse(a6 %in% c(i,j), NA,a6_elo))
     # 
     # away.players.mean <- player.shift.level.prep %>% select(ends_with("elo")) %>% select(starts_with("a")) %>% rowMeans(na.rm = T) %>% as.data.frame()
     # home.players.mean <- player.shift.level.prep %>% select(ends_with("elo")) %>% select(starts_with("h")) %>% rowMeans(na.rm = T) %>% as.data.frame()
     # 
     # away.players.max <- player.shift.level.prep %>% rowwise() %>% mutate(away.players.max = max(a1_elo, a2_elo, a3_elo, a4_elo, a5_elo, a6_elo, na.rm = T)) %>% select(away.players.max)
     # home.players.max <- player.shift.level.prep %>% rowwise() %>% mutate(home.players.max = max(h1_elo, h2_elo, h3_elo, h4_elo, h5_elo, h6_elo, na.rm = T)) %>% select(home.players.max)
     # 
     # away.players.min <- player.shift.level.prep %>% rowwise() %>% mutate(away.players.min = min(a1_elo, a2_elo, a3_elo, a4_elo, a5_elo, a6_elo, na.rm = T)) %>% select(away.players.min)
     # home.players.min <- player.shift.level.prep %>% rowwise() %>% mutate(home.players.min = min(h1_elo, h2_elo, h3_elo, h4_elo, h5_elo, h6_elo, na.rm = T)) %>% select(home.players.min)
     # 
     # 
     # player.shift.level <- data.frame(Player = i,
     #                                  Away.Players.Mean = away.players.mean$.,
     #                                  Home.Players.Mean = home.players.mean$.,
     #                                  away.players.max,
     #                                  home.players.max,
     #                                  away.players.min,
     #                                  home.players.min) %>%
     #      cbind(player.shift.level.prep) %>%
     #      filter(ice.time > 0) %>%
     #      mutate(Mean.Teammates = ifelse(player.team == hometeam, Home.Players.Mean, Away.Players.Mean),
     #             Mean.Competition = ifelse(player.team != hometeam, Home.Players.Mean, Away.Players.Mean),
     #             
     #             Max.Teammates = ifelse(player.team == hometeam, home.players.max, away.players.max),
     #             Max.Competition = ifelse(player.team != hometeam, home.players.max, away.players.max),
     #            
     #             Min.Teammates = ifelse(player.team == hometeam, home.players.min, away.players.min),
     #             Min.Competition = ifelse(player.team != hometeam, home.players.min, away.players.min)) %>%
     #      group_by(Player, season, player.team, gcode, period, PlayerVenue, Player.Shift.Number) %>%
     #      summarise(xGD = sum(xGF, na.rm = T) - sum(xGA, na.rm = T),
     #                Player.Score.State = mean(Player.Score.State),
     #                Team.Score = mean(Team.Score),
     #                Opposition.Score = mean(Opposition.Score),
     #                
     #                Team.Strength = weighted.mean(Team.Strength, w = ice.time, na.rm = T),
     #                Opposition.Strength = weighted.mean(Opposition.Strength, w = ice.time, na.rm = T),
     #                
     #                Mean.Teammates = weighted.mean(Mean.Teammates, w = ice.time, na.rm = T),
     #                Mean.Competition = weighted.mean(Mean.Competition, w = ice.time, na.rm = T),
     # 
     #                Max.Teammates = weighted.mean(Max.Teammates, w = ice.time, na.rm = T),
     #                Max.Competition = weighted.mean(Max.Competition, w = ice.time, na.rm = T),
     #                
     #                Min.Teammates = weighted.mean(Min.Teammates, w = ice.time, na.rm = T),
     #                Min.Competition = weighted.mean(Min.Competition, w = ice.time, na.rm = T),
     #                
     #                Game.Minute = weighted.mean(seconds, w = ice.time) / 60,
     #                
     #                GF = sum(GF,  na.rm=T),
     #                GA = sum(GA,  na.rm=T),
     #                
     #                G = sum(G, na.rm=T),
     #                ixG = sum(ixG, na.rm=T),
     #                
     #                Off.FO.Shift = sum(Off.FO.Shift),
     #                Def.FO.Shift = sum(Def.FO.Shift),
     #                Neu.FO.Shift = sum(Neu.FO.Shift),
     #                OTF.Shift = sum(OTF.Shift))
     #    
       #return(player.shift.level)   
       return(player.bystrength.adj)
       #return(list(player.bystrength.adj,QOC.QOT.distro, QOC.QOT.matrix1, QOC.QOT.matrix2))
     
}

#skater.shift.level <- plyr::rbind.fill(lapply(FUN=skater.stats,skater.list))
#save(skater.shift.level, file="~/Documents/CWA/Hockey Data/skater.shift.level.RData")

#multiplot(skater.stats("TRAVIS HAMONIC")[[1]],skater.stats("BRANDON SAAD")[[3]])

skater.season.stats <- plyr::rbind.fill(lapply(FUN=skater.stats,skater.list))

## Long 
skater.season.stats.long <- skater.season.stats %>%
                          #left_join(skater.name.xwalk, by=c("Player"="Player1")) %>%
                          #mutate(Player = ifelse(is.na(Player_clean),Player,Player_clean)) %>%
                          inner_join(roster_all, by="Player") %>%
                          select(-ID) %>%
                          mutate(Pos = ifelse(Pos == "D","D","F"),
                                 xGA60 = -1 * xGA60,
                                 xGA60_teamWO = -1 * xGA60_teamWO,
                                 xGF60_Rel = xGF60 - xGF60_teamWO,
                                 xGA60_Rel = xGA60 - xGA60_teamWO,
                                 xGD60 = xGF60 + xGA60,
                                 xGD60_teamWO = xGF60_teamWO + xGA60_teamWO,
                                 P160 = G60 + A160,
                                 P60 = P160 + A260) 
    

skater.season.stats.long[is.na(skater.season.stats.long)] <- 0

save(skater.season.stats.long, file="~/Documents/CWA/Hockey Data/skater.season.stats.long.RData")
load("~/Documents/CWA/Hockey Data/skater.season.stats.long.RData")

skater.season.stats.long <- unique(skater.season.stats.long)

skater.season.stats.wide <- dcast(melt(skater.season.stats.long, 
                                       id.vars=c("Player", "season","Pos","player.team.state")), 
                                  Player + season + Pos ~ variable + player.team.state) %>%
                mutate( Competition_Diff_EV = Player_Competition_EV - TeamWO_Competition_EV,
                        Competition_Diff_PP = Player_Competition_PP - TeamWO_Competition_PP,
                        Teammates_Diff_EV = Player_Teammates_EV - TeamWO_Teammates_EV,
                        Teammates_Diff_PP = Player_Teammates_PP - TeamWO_Teammates_PP) %>%
                select(-starts_with("C"))

skater.season.stats.wide[is.na(skater.season.stats.wide)] <- 0
skater.season.stats.wide[mapply(is.nan, skater.season.stats.wide)] <- 0
skater.season.stats.wide[mapply(is.infinite, skater.season.stats.wide)] <- 0

### Predicted CrowdScout Scores
crowdscout.data <- player_elo %>% 
            inner_join(skater.season.stats.wide, by=c("Player", "season")) 

cs.model.data <- crowdscout.data %>%
            filter(Games.Played_EV > 60) %>%
            mutate(Pos = as.factor(Pos)) %>%
            select(-c(Player, ID),
                   -starts_with("team.time"),
                   -starts_with("xGF60_teamWO"),
                   -starts_with("xGA60_teamWO"),
                   -starts_with("xGD60_teamWO"),
                   -starts_with("TOI"),
                   -starts_with("Games.Played_PP"),-starts_with("Games.Played_SH"),
                   -starts_with("Neu.FO"),
                   -starts_with("xGD60"),
                   -starts_with("P160"),
                   -starts_with("A260"),
                   -starts_with("TeamWO_"),
                   -ends_with("Away_EV"),-ends_with("Away_SH"),-ends_with("Away_PP"),
                   -ends_with("Home_EV"),-ends_with("Home_SH"),-ends_with("Home_PP"),
                   -ends_with("Shift.Share_PP"),-ends_with("Shift.Share_SH"),
                   -ends_with("Competition_SH"),-ends_with("Teammates_SH"),-ends_with("Competition_PP"),
                   -starts_with("Total.Shifts"))


#########################
# Pre Process
######################### 
preProcValues.train <- cs.model.data %>% 
                      group_by(season) %>% 
                      preProcess(method = c("center", "scale")) %>% 
                      predict(cs.model.data) %>%
                      select(-c(season))

preProcValues.all <- skater.season.stats.wide %>% 
                      group_by(season) %>% 
                      preProcess(skater.season.stats.wide, method = c("center", "scale")) %>% 
                      predict(skater.season.stats.wide) %>%
                      select(-c(season))

#########################
#### RFE 
#########################
# 
# # ensure the results are repeatable
# set.seed(7)
# library(plyr); library(dplyr);library(mlbench); library(caret)
# 
# # define the control using a random forest selection function
# control <- rfeControl(functions=rfFuncs, method="cv", number=10)
# # run the RFE algorithm
# results <- rfe(preProcValues.train[2:ncol(preProcValues.train)], preProcValues.train[,1], sizes=c(2:ncol(preProcValues.train)), rfeControl=control)
# # summarize the results
# print(results)
# # list the chosen features
# rfe.features <- predictors(results)
# 
# 
# ###test linear combos
# linear.combos.ds <- as.data.frame(preProcValues.train[names(preProcValues.train) %in% rfe.features], stringsAsFactors = TRUE )
# linear.combos <- findLinearCombos(linear.combos.ds[c(4:ncol(linear.combos.ds))])
# 
# ## Model Data
# cs.model.data.rfe <- cs.model.data[names(preProcValues.train) %in% c("Score",rfe.features)]  #%>%sample_n(100)
# 
#########################
# Random Forest
#########################
train_control <- trainControl(method="cv", number=10)

cs.model.rf <- caret::train( Score ~ . ,
                             data=preProcValues.train, trControl=train_control, method="rf",
                             tuneLength=6, ntree=500, importance = TRUE)

# var importance
varimp.scales <- varImp(cs.model.rf, scale=FALSE) 
varimp.scales %>% 
  ggplot() + 
  labs(title="Random Forest Model Predicting CrowdScout Score\nVariable Importance") +
  geom_bar(fill="purple", stat= "identity")

Predicted.CS.RF <- predict(cs.model.rf, newdata=preProcValues.all)

## Measurements
cs.error <- predict(cs.model.rf, preProcValues.train) - scale(cs.model.data$Score)

## Brier Score
mean(cs.error^2)
# 17.8437
# 0.2967288

# Mean Absolute Error
mean(abs(cs.error))
# 3.267889
# 0.4305746

# RMSE
sqrt(mean(cs.error^2))
# 4.22418
# 0.5447282

#########################
# GLM
#########################
train_control <- trainControl(method="cv", number=10)

cs.model.lm <- caret::train( Score ~ . ,
                             data=preProcValues.train, trControl=train_control, method="lm", importance = TRUE)

# var importance
varimp.scales <- varImp(cs.model.lm, scale=FALSE) 
varimp.scales %>% 
  ggplot() + 
  labs(title="GLM Model Predicting CrowdScout Score\nVariable Importance") +
  geom_bar(fill="purple", stat= "identity")

Predicted.CS.LM <- predict(cs.model.lm, newdata=preProcValues.all)

## Measurements
cs.error <- predict(cs.model.lm, preProcValues.train) - scale(cs.model.data$Score)

## Brier Score
mean(cs.error^2)
# 20.11
# 0.2967288

# Mean Absolute Error
mean(abs(cs.error))
# 3.491116
# 0.4305746

# RMSE
sqrt(mean(cs.error^2))
# 4.488268
# 0.5447282

#########################
# Neural Network
#########################
# my.grid <- expand.grid(.decay = c(0.5, 0.1), .size = c(5, 6, 7))
# cs.model.nn <- train(Score ~ ., data = cs.model.data,# preProc=c("center", "scale"),
#                      method = "nnet", maxit = 1000, tuneGrid = my.grid, trace = F, linout = 1)    
# 
# varImp(cs.model.nn, scale=FALSE) %>% ggplot() + 
#   labs(title="Neural Net Predicting CrowdScout Score\nVariable Importance") +
#   geom_bar(fill="purple", stat= "identity")
# 
# Predicted.CS.NN <- predict(cs.model.nn, newdata=skater.season.stats.wide)

#########################
## Predicted Scores
#########################
crowdscout.data.pred <- cbind(Predicted.CS.RF, Predicted.CS.LM, skater.season.stats.wide)

max.min <- crowdscout.data.pred %>%
          dplyr::group_by(season) %>%
          summarise(max.RF = max(Predicted.CS.RF), min.RF = min(Predicted.CS.RF),
                    max.LM = max(Predicted.CS.LM), min.LM = min(Predicted.CS.LM))

crowdscout.data.pred.scaled <- crowdscout.data.pred %>%
          left_join(max.min, by = "season") %>%
          mutate(Predicted.CS.RF.Scaled = 100 * ((Predicted.CS.RF - min.RF) / (max.RF - min.RF)),
                 Predicted.CS.LM.Scaled = 100 * ((Predicted.CS.LM - min.LM) / (max.LM - min.LM)),
                 Predicted.CS = (Predicted.CS.RF.Scaled + Predicted.CS.LM.Scaled) / 2) %>%
          select(Player,  season, Predicted.CS, Predicted.CS.RF,Predicted.CS.RF.Scaled, Predicted.CS.LM, Predicted.CS.LM.Scaled, everything()) %>%
          left_join(skater.roster, by = "Player") %>%
          select(Player, shooterID, season, everything(),
                 -c(Predicted.CS.RF,Predicted.CS.LM)) %>%
          filter(!is.na(shooterID))

check_duplicates <- crowdscout.data.pred.scaled %>% 
    group_by(shooterID, season) %>% 
    dplyr::summarize(count = n()) %>% 
    filter( count > 1) %>% 
    inner_join(crowdscout.data.pred.scaled, by = c("shooterID", "season")) %>% 
    select(shooterID,season,count,Player,Predicted.CS,Pos) 

save(crowdscout.data.pred.scaled, file="~/Documents/CWA/Hockey Data/crowdscout.data.pred.scaled")
load("~/Documents/CWA/Hockey Data/crowdscout.data.pred.scaled")

###################
#####PCA
###################

crowdscout.data.pred.pca <- crowdscout.data.pred.scaled %>% 
        mutate(D = ifelse(Player.Position == "D",1,0),
               C = ifelse(Player.Position == "C",1,0),
               W = ifelse(!Player.Position %in% c("C","D"),1,0))


pca.vars <- c("Total.Shifts_EV","Total.Shifts_PP","Total.Shifts_SH","OTF.Shift.Share_EV","OTF.Shift.Share_PP",
              "OTF.Shift.Share_SH","Off.FO.Shift.Share_EV","Off.FO.Shift.Share_PP","Off.FO.Shift.Share_SH","Def.FO.Shift.Share_EV","Def.FO.Shift.Share_PP",
              "Def.FO.Shift.Share_SH","ixG60_EV","ixG60_PP","ixG60_SH","G60_EV","G60_PP",
              "G60_SH","A160_EV","A160_PP","A160_SH","xGF60_EV","xGF60_PP",
              "xGF60_SH","xGA60_EV","xGA60_PP","xGA60_SH","Player_Competition_EV","Player_Teammates_EV",
              "Player_Teammates_PP","Share.of.Ice_EV","Share.of.Ice_PP","Share.of.Ice_SH","xGF60_Rel_EV","xGF60_Rel_PP",
              "xGF60_Rel_SH","xGA60_Rel_EV","xGA60_Rel_PP","xGA60_Rel_SH","P60_EV","P60_PP",
              "P60_SH","Teammates_Diff_EV","Teammates_Diff_PP","D","C","W")

# Scale Each Season
scale.season <- function(year) {
  
  season.data <- crowdscout.data.pred.pca %>% 
                      filter(season == year)
  
  season.scaled <- as.data.frame(cbind(season.data[,c("Player","shooterID","season","Pos","Predicted.CS")], scale(season.data[,pca.vars])))
                
  print(sapply(season.scaled, var))
  return(season.scaled)
    
}

## Unique Seasons
seasons <- unique(crowdscout.data.pred.pca$season)

## Scale Each Season Separately
crowdscout.data.pred.pca2 <- plyr::rbind.fill(lapply(FUN=scale.season,seasons))

crowdscout.data.pred.pca2[is.na(crowdscout.data.pred.pca2[,pca.vars]),pca.vars] <- 0

# Verify variance is uniform
plot(sapply(crowdscout.data.pred.pca2[,pca.vars], var))

# Proceed with principal components
pc <- princomp(crowdscout.data.pred.pca2[,pca.vars])
plot(pc)
plot(pc, type='l')
summary(pc) # 3 components is both 'elbow' and explains >85% variance

pca2 <- prcomp(crowdscout.data.pred.pca2[,pca.vars],center = TRUE,scale. = TRUE)
loadings <- as.data.frame(pca2$rotation[,1:3])

# Get principal component vectors using prcomp instead of princomp
pc <- prcomp(crowdscout.data.pred.pca2[,pca.vars])
# First for principal components
comp <- data.frame(pc$x[,1:3])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

check.comps <- cbind(crowdscout.data.pred.pca2[,c("Player","shooterID","season")], comp)

# Determine number of clusters 
wss <- (nrow(crowdscout.data.pred.pca2[,pca.vars])-1)*sum(apply(crowdscout.data.pred.pca2[,pca.vars],2,var)) 

for (i in 2:25) wss[i] <- sum(kmeans(crowdscout.data.pred.pca2[,pca.vars], centers=i, nstart = 25, iter.max = 1000)$withinss) 
plot(1:25, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


# From scree plot elbow occurs at k = 12
# Apply k-means with k=12
k <- kmeans(comp, 12, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=as.factor(k$clust), pch=16) 

# Cluster sizes
Cluster = k$clust
ClusterCenter = as.data.frame(k$centers)

cluster.xwalk <- data.frame(cbind(Cluster,crowdscout.data.pred.pca2[,c("Player","Pos","shooterID", "season", "Predicted.CS")])) %>%
                  mutate(Fwd = ifelse(Pos == "D",0,1)) %>% 
                  group_by(Cluster) %>% 
                  summarise(Predicted.CS = mean(Predicted.CS), 
                            Fwd.Share = mean(Fwd), Count = n()) %>%
                  mutate(Pos = ifelse(Fwd.Share < 0.15,"D",
                               ifelse(Fwd.Share > 0.85,"F","Both"))) %>%
                  arrange(Cluster) %>%
                  cbind(ClusterCenter) %>%
                  group_by(Pos) %>%
                  mutate(ClusterRank = rank(-Predicted.CS),
                         ClusterName = ifelse(Pos == "F" & ClusterRank == 1,"All-Around Skilled Offensive Driver",
                                       ifelse(Pos == "D" & ClusterRank == 1,"All-Around Skilled Defensive Driver",
                                              
                                       ifelse(Pos == "F" & ClusterRank == 2,"Matchup Capable Skilled Offensive Driver",
                                       ifelse(Pos == "D" & ClusterRank == 2,"Matchup Dependent Skilled Defensive Player",
                                              
                                       ifelse(Pos == "F" & ClusterRank == 3,"All-Around Matchup Capable Offensive Driver",
                                       ifelse(Pos == "D" & ClusterRank == 3,"Matchup Capable Defensive Player",
                                              
                                       ifelse(Pos == "F" & ClusterRank == 4,"Matchup Capable Defensive Forward",
                                       ifelse(Pos == "D" & ClusterRank == 4,"Matchup Dependent Defensive Depth",
                                              
                                       ifelse(Pos == "F" & ClusterRank == 5,"Matchup Capable Offensive Depth",
                                       ifelse(Pos == "F" & ClusterRank == 6,"Defensive Depth",
                                       ifelse(Pos == "F" & ClusterRank == 7,"Depth Defensive Forward","Defensive Depth"))))))))))))
                                              

# First cluster
player.season.clusters <- crowdscout.data.pred.pca2 %>% 
          select(Player, shooterID, season, Pos, Predicted.CS) %>%
          cbind(Cluster) %>%
          left_join(cluster.xwalk[, c("Cluster","ClusterName")], by = "Cluster") %>%
          mutate(Name = paste0(sapply(strsplit(as.character(Player), ' '), function(x) x[length(x)]))) %>% #,substr(season,7,8)))
            group_by(season, Pos) %>%
            mutate(Pos.Rank = rank(-Predicted.CS),
                   DepthChart = ifelse(Pos == "D" & Pos.Rank < 60,"1P D",
                           ifelse(Pos == "D" & Pos.Rank < 120,"2P D",
                           ifelse(Pos == "D" & Pos.Rank < 180,"3P D",
                           ifelse(Pos == "D","Other D",
                           ifelse(Pos != "D" & Pos.Rank < 90,"1L Fwd",
                           ifelse(Pos != "D" & Pos.Rank < 180,"2L Fwd",
                           ifelse(Pos != "D" & Pos.Rank < 270,"3L Fwd",
                           ifelse(Pos != "D" & Pos.Rank < 360,"4L Fwd",
                           ifelse(Pos != "D","Other Fwd","Other"))))))))))

talents <- player.season.clusters %>% mutate(Fwd = ifelse(Pos == "D",0,1)) %>% group_by(Cluster) %>% 
  summarise(Predicted.CS = mean(Predicted.CS), Fwd.Share = mean(Fwd), Count = n())

### Join
crowdscout_data_predictions <- player.season.clusters %>%
          ungroup() %>%
          select(Player, shooterID,season,Cluster,ClusterName,Pos.Rank,DepthChart) %>%
          inner_join(crowdscout.data.pred.scaled, by = c("Player","shooterID","season")) %>%
          mutate(TOI = (TOI_EV + TOI_SH + TOI_PP)/ 3600,
                G60 = ((G60_EV * (TOI_EV / 3600 )) + (G60_PP * (TOI_PP / 3600 )) + (G60_SH * (TOI_SH / 3600 ))) / TOI,
                P60 = ((P60_EV * (TOI_EV / 3600 )) + (P60_PP * (TOI_PP / 3600 )) + (P60_SH * (TOI_SH / 3600 ))) / TOI)

save(crowdscout_data_predictions, file="~/Documents/CWA/Hockey Data/crowdscout_data_predictions")
load("~/Documents/CWA/Hockey Data/crowdscout_data_predictions")


### Write Database
library(RMySQL)

conn <- dbConnect(MySQL(), user='ca_elo_games', password='cprice31!', host='mysql.crowdscoutsports.com', db='nhl_all')
on.exit(dbDisconnect(conn))

dbWriteTable(conn, 'crowdscout_data_predictions', as.data.frame(crowdscout_data_predictions), overwrite=T,row.names = FALSE)
dbWriteTable(conn, 'skater_season_stats_long', as.data.frame(skater.season.stats.long), overwrite=T,row.names = FALSE)
dbWriteTable(conn, 'player_elo', as.data.frame(player_elo), overwrite=T,row.names = FALSE)



player.cluster.career <- player.season.clusters %>%
              group_by(Player, ClusterName) %>%
              summarise(SeasonCnt = uniqueN(season)) %>%
              group_by(Player) %>%
              summarise(DistinctClusters = uniqueN(ClusterName),
                        SeasonCnt = sum(SeasonCnt))

player.ability.career <- player.season.clusters %>%
  group_by(Player) %>%
   summarise(SeasonCnt = uniqueN(season),
             DistinctClusters = uniqueN(ClusterName),
            Mean = mean(Predicted.CS),
            StdDev = sd(Predicted.CS))


Predicted.CS

player.list <- c("ALES HEMSKY","KRIS VERSTEEG","KRIS RUSSELL","DAN GIRARDI","PATRICK WIERCOICH","SCOTT HARTNELL","RYAN MURPHY","TRAVIS HAMONIC","BEAU BENNETT","LANCE BOUMA","CAM FOWLER","MARC-EDOUARD VLASIC","PAUL POSTMA","MARTIN HANZAL","COLIN WILSON","TYLER PITLICK","JORDAN OESTERLE","KENNY AGOSTINO","JOSH JOORIS","BENOIT POULIOT","JUSSI JOKINEN","MICHAEL CAMMALLERI","KEVIN SHATTENKIRK","ALEXANDER RADULOV","KARL ALZNER","MARTIN HANZAL","JOE THORNTON","JUSTIN WILLIAMS","RADIM VRBATA","PATRICK MARLEAU",
                 "ANDREI MARKOV","JAROMIR JAGR","BRIAN BOYLE","SAM GAGNER","NICK BONINO","MICHAEL STONE","MIKE FISHER","RON HAINSEY","THOMAS VANEK","MICHAEL DEL ZOTTO","DMITRY KULIKOV","DREW STAFFORD","JOHNNY ODUYA","PATRICK SHARP","CODY FRANSON","TREVOR DALEY","NAIL YAKUPOV","JORDAN WEAL","MATT HUNWICK")

player.list <- c("JUSSI JOKINEN",
                 "JAROMIR JAGR","THOMAS VANEK","JIRI HUDLER","DANIEL WINNIK","DREW STAFFORD","SHANE DOAN","MIKE FISHER","MATT CULLEN","DAVID DESHARNAIS")

player.list <- c("BRIAN CAMPBELL")

player.season.clusters %>%
    #filter(season == "20162017") %>%
    mutate(PlayerListed = ifelse(Player %in% player.list,paste0(Player,substr(season,7,8)),"")) %>%
    ggplot(aes(y=Predicted.CS,x=reorder(ClusterName,-Predicted.CS), color=as.factor(DepthChart), shape=as.factor(Pos), 
               label=PlayerListed, color=as.factor(ClusterName)), size=20) +
    #geom_point(position = position_jitterdodge()) +
    #geom_repel_text(angle = 45) +
    geom_point(size=5, alpha=0.5) +
    ggrepel::geom_label_repel(angle = 45, segment.color = 'grey50') +
    guides(colour = guide_legend(override.aes = list(size=8))) +
    #coord_flip() +
    labs(title = "Player Season Clusters and Estimated Ability", y="Predicted CrowdScout Score (0-100)", shape="Position",color="Depth Chart",x="") +
    theme_standard() + 
    scale_color_discrete()


team_ability <- player.season.clusters %>%
      inner_join(roster_current[,c("Player","team")], by = "Player") %>%
      mutate(Driver = ifelse(ClusterName %in% c("All-Around Skilled Defensive Driver",
                                                "All-Around Skilled Offensive Driver",
                                                "All-Around Matchup Offensive Driver"),1,0)) %>%
      group_by(season, team, Driver) %>%
      summarise(Predicted.CS = mean(Predicted.CS),
                Count = n())
      

team_season_clusters <- function(tm, year) {
  
p <- player.season.clusters %>%
    inner_join(roster_current[,c("Player","team")], by = "Player") %>%
    filter(!Player %in% c("DENNIS WIDEMAN")) %>%
    mutate(PlayerListed = ifelse(team %in% tm & season %in% year,paste0(Player,substr(season,7,8)),"")) %>%
    ggplot(aes(y=Predicted.CS,x=reorder(ClusterName,Predicted.CS), color=as.factor(DepthChart), shape=as.factor(Pos), label=PlayerListed, color=as.factor(ClusterName))) +
    geom_point() +
    ggrepel::geom_label_repel(angle = 45, segment.color = 'grey50') +
    #coord_flip() +
    labs(title = paste0(tm," Player Season Clusters and Estimated Ability, ", year), y="Predicted CrowdScout Score (0-100)", shape="Position",color="Depth Chart",x="") +
  theme_standard() + scale_color_gdocs()
  
  return(p)
}

team_season_clusters("CAR","20162017")

player.season.clusters %>%
    filter(season == "20162017") %>%
    mutate(Player)
    mutate(ID = as.numeric(shooterID)) %>%
    inner_join(roster_current, by="ID") %>%
    ggplot(aes(x=team,y=ClusterName, color=Predicted.CS.RF, label=Name, shape=Player.Position)) +
      geom_point(position = position_jitterdodge()) +
      geom_text(check_overlap = TRUE, angle = 45) +
      scale_color_gradient2(low="firebrick2",mid="grey50",high="forestgreen", midpoint = 40) +
      labs(title = "Player Season Clusters and Estimated Ability by Team, 2016-17", color="Predicted CrowdScout Score (0-100)")


team.clusters <- player.season.clusters %>%
  mutate(ID = as.numeric(shooterID)) %>%
  inner_join(roster_current, by="ID") %>%
  group_by(team, ClusterName) %>%
  summarise(player_count = n()) %>%
  dcast(team ~ ClusterName)


##############
### OVER/UNDER RATED
over.under.rated <- data.frame(Player = crowdscout.data$Player,
                               season = crowdscout.data$season,
                               GP = crowdscout.data$Games.Played_EV,
                               TOI = crowdscout.data$TOI_EV,
                               Score = cs.model.data$Score,
                               Predicted.CS.LM = predict(cs.model.lm, newdata=cs.model.data.rfe),
                               Predicted.CS.RF = predict(cs.model.rf, newdata=cs.model.data.rfe)) %>%
  filter(season == "20162017") %>%
  mutate(Predicted.CS = ((Predicted.CS.LM + Predicted.CS.RF) / 2))

max <- max(over.under.rated$Predicted.CS)
min <- min(over.under.rated$Predicted.CS)

over.under.rated2 <- over.under.rated %>%
  mutate(Predicted.CS.Scaled = (Predicted.CS - min) / (max - min) * 100,
         OverRated = Score - Predicted.CS.Scaled) %>% 
  select(Player, season, Score, GP, TOI, Predicted.CS.Scaled, OverRated)

team.over.under.rated <- over.under.rated2 %>%
  inner_join(roster_current, by = "Player") %>%
  mutate(Pos1 = ifelse(Pos == "D","D","F")) %>%
  group_by(team, Pos1) %>%
  summarise(TeamPredicted = weighted.mean(Predicted.CS.Scaled, w = TOI),
            TeamScore = weighted.mean(Score, w = TOI),
            Overrated = TeamScore - TeamPredicted) %>%
  ungroup() %>%
  mutate(Overrated.Index = scale(Overrated))


team.over.under.rated %>%
  ggplot(aes(x=reorder(team,-Overrated.Index) , y=Overrated.Index, group=Pos1, fill=Pos1)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(x="",y="Raw Team CrowdScout Rating Less Predicted CrowdScout Rating, Scaled & Weighted for 2016-17 EV Ice-time",
       title="CrowdScout Over/Underrated Teams and Position Groups",fill="Position Group") +
  coord_flip() +
  annotate("text",x=15, y=-2, label="Under-rated by Crowd\nOn-ice Results Superior to Rating", color="grey50") +
  annotate("text",x=15, y=2, label="Over-rated by Crowd\nOn-ice Results Inferior to Rating", color="grey50")


######PLOT PREDICT VS ACTUAL
ggplot() +
    annotate("segment",x=0,y=0,yend=100,xend=100, color="grey50", alpha=0.33) +
    geom_point(aes(x=predict(cs.model.rf, cs.model.data), y=cs.model.data$Score, color=cs.model.data$Pos, alpha=cs.model.data$Games.Played_EV)) +
    geom_smooth(aes(x=predict(cs.model.rf, cs.model.data), y=cs.model.data$Score, color=cs.model.data$Pos),method = "lm", se = FALSE) +
    geom_text(aes(x=predict(cs.model.rf, cs.model.data), y=cs.model.data$Score, 
                  label = paste0(crowdscout.data$Player,substr(crowdscout.data$season,7,8))), color = "grey50", check_overlap = TRUE) +
    labs(title="Actual vs Predicted CrowdScout Score\nRandom Forest Model Explaining CrowdScout Score Based on 128 On-Ice Variables",
         x="Predicted CrowdScout Score", y="Actual CrowdScout Score", color="Position", alpha="Games Played")


######PLOT PREDICT VS ACTUAL
crowdscout.data.pred %>%
  ggplot() +
  geom_density(aes(x=Predicted.CS.RF, group=interaction(1, season), color=season))
  annotate("segment",x=0,y=0,yend=100,xend=100, color="grey50", alpha=0.33) +
  geom_point(aes(x=predict(cs.model.rf, cs.model.data), y=cs.model.data$Score, color=cs.model.data$Pos, alpha=cs.model.data$Games.Played_EV)) +
  geom_smooth(aes(x=predict(cs.model.rf, cs.model.data), y=cs.model.data$Score, color=cs.model.data$Pos),method = "lm", se = FALSE) +
  geom_text(aes(x=predict(cs.model.rf, cs.model.data), y=cs.model.data$Score, 
                label = paste0(crowdscout.data$Player,substr(crowdscout.data$season,7,8))), color = "grey50", check_overlap = TRUE) +
  labs(title="Actual vs Predicted CrowdScout Score\nRandom Forest Model Explaining CrowdScout Score Based on 128 On-Ice Variables",
       x="Predicted CrowdScout Score", y="Actual CrowdScout Score", color="Position", alpha="Games Played")



partners <- player.QOT.QOC %>% 
  filter(season == "20162017" & PlayerOnIce == "Player" & player.team.state == "SH") %>% 
  mutate(PK.unit = ifelse(player.team == hometeam,paste0(h1,', ',h2,', ',h3,', ',h4),paste0(a1,', ',a2,', ',a3,', ',a4)),
         xG.For = ifelse(player.team == ev.team, xG, 0),
         xG.Against = ifelse(player.team != ev.team, xG, 0)) %>%
  group_by(PK.unit) %>%
  summarise(Minutes = sum(ice.time) / 60, xG.Pct = sum(xG.For) / (sum(xG.For) + sum(xG.Against)))



coaches.matching <- crowdscout.data %>%
        filter(season == "20162017") %>%
        inner_join(roster_current, by = "Player")

team.list <- unique(coaches.matching$team)

team.matching.fun <- function(tm.nm) {
  
  team.matching <- coaches.matching %>%
          filter(team == tm.nm  & Player_Competition_Home_EV > 0 & Player_Competition_Away_EV > 0) 
  
  team.matching %>%
        ggplot(aes(x=Player_Competition_Home_EV, y=Player_Competition_Away_EV, label=Player, color=Pos.x, size=TOI_EV / 60)) +
        annotate("segment",x=45,y=45,xend=50,yend=50, color="grey50", size=1.5, alpha=0.6) +
        annotate("text",x=45,y=49, label="Easier Matchups at Home", color="grey50") +
        annotate("text",x=49,y=45, label="Tougher Matchups at Home", color="grey50") +
        geom_point() +
        ggrepel::geom_text_repel() +
        #geom_smooth(method = "lm", se = F) +
        labs(title = paste0(tm.nm," Even Strength Player Competition Home vs Away, 2015-16"),x="Player Competition Mean Strength (Home)",
         y="Player Competition Mean Strength (Away)",color="Position", size="EV TOI")

}  

team.matching.fun("TOR")


coaches.matching2 <- coaches.matching %>%
      filter(Player_Competition_Home_EV > 0 & Player_Competition_Away_EV > 0 & (TOI_EV / 60) > 60) %>%
      mutate(Home.Lean.Comp = Player_Competition_Home_EV - Player_Competition_Away_EV,
             Home.Lean.Team = Player_Teammates_Away_EV - Player_Teammates_Home_EV,
             Coach.Match.Impact = abs(Home.Lean.Comp) + abs(Home.Lean.Team),
             Coach.Comp.Impact = abs(Home.Lean.Comp) ,
             Coach.Team.Impact = abs(Home.Lean.Team))

coaches.matching2 %>% 
  mutate(LName = sapply(strsplit(as.character(Player), ' '), function(x) x[length(x)])) %>%
  ggplot(aes(x=Home.Lean.Comp, y=Home.Lean.Team, group=team, color=Pos.x, size=TOI_EV / 60)) +
  annotate("segment",x=-5,y=-5,xend=5,yend=5, color="grey50", size=1.5, alpha=0.6) +
  annotate("segment",x=0,y=-5,xend=0,yend=5, color="grey50", size=1.5, alpha=0.6) +
  annotate("segment",y=0,x=-5,yend=0,xend=5, color="grey50", size=1.5, alpha=0.6) +
  annotate("text",x=-5,y=5, label="Better Teammates,\nWeaker Competition at Home", color="grey50") +
  annotate("text",x=5, y=-5, label="Weaker Teammate,\nBetter Competition at Home", color="grey50") +
  annotate("text",x=-5,y=-5, label="Weaker Teammates,\nWeaker Competition at Home", color="grey50") +
  annotate("text",x=5,y=5, label="Better Teammates,\nBetter Competition at Home", color="grey50") +
  geom_text(alpha=0.7, aes(label=LName)) +
  #geom_smooth(method=lm, se=FALSE) +
  #directlabels::geom_dl(method = "last.qp", aes(label=team)) +
  labs(title = "Even Strength Player Competition & Teammates by Team Venue, 2016-17",
       x="Difference in Competition Home vs Away",
       y="Difference in Teammates Home vs Away",color="Position", size="EV TOI")


coaches.matching.abs <- coaches.matching2 %>%
    group_by(team) %>%
    summarise(#Coach.Match.Impact = weighted.mean(Coach.Match.Impact, TOI_EV),
              Coach.Comp.Impact = weighted.mean(Coach.Comp.Impact, TOI_EV),
              Coach.Team.Impact = weighted.mean(Coach.Team.Impact, TOI_EV))

coaches.matching.abs %>%
    data.frame() %>%
    melt(id = c("team")) %>%
    ggplot(aes(x=reorder(team,value), y=value, group=variable, fill=variable)) +
    geom_bar(stat = "identity", position = "stack") +
    coord_flip() +
  labs(title = "Even Strength Team Difference in Competition & Teammates by Team Venue, 2016-17\nWeighted by Player EV TOI",
       x="", color="",
       y="Weighted Average of Absolute Difference in Home/Away Competetion & Teammates", size="EV TOI")

coaches.matching.abs %>%
  ggplot(aes(x=Coach.Team.Impact, y=Coach.Comp.Impact, label=team)) +
  annotate("segment",x=0,y=0,xend=1.5,yend=1.5, color="grey50", size=1.5, alpha=0.6) +
  annotate("text",x=0.1, y=1, label="Coach Active Matching\nCompetition at Home Relative to Away", color="grey50") +
  annotate("text",x=1,y=0.1, label="Coach Active Adjusting\nTeammates at Home Relative to Away", color="grey50") +
  geom_point(color="grey50") +
  ggrepel::geom_text_repel() +
  labs(title = "Absolute Difference in Home vs Away Teammates & Competition, 2016-17\nWeighted by Player EV TOI",
       x="Absolute Difference in Teammates, Home vs Away\n(weighted by Player EV TOI)", color="",
       y="Absolute Difference in Competition, Home vs Away\n(weighted by Player EV TOI)", size="EV TOI")



matchup.models <- coaches.matching %>% 
  group_by(team) %>% 
  do(model = lm(Player_Competition_EV ~ Player_Teammates_EV, data = .)) %>% unlist()


coaches.matching %>% 
    mutate(LName = sapply(strsplit(as.character(Player), ' '), function(x) x[length(x)])) %>%
    ggplot(aes(x=Player_Teammates_EV, y=Player_Competition_EV, group=team, color=team, size=TOI_EV / 60)) +
    geom_text(alpha=0.4, aes(label=LName)) +
    geom_smooth(method=lm, se=FALSE) +
    #directlabels::geom_dl(method = "last.qp", aes(label=team)) +
    labs(title = "Even Strength Player Competition & Teammates by Team, 2016-17",x="Player Teammate Mean Strength (0-100)",
         y="Player Competition Mean Strength (0-100)",color="Team", size="EV TOI")



scoring.surplus <- skater.season.stats.wide %>%
              mutate(total_ixG = (ixG60_EV * (TOI_EV / 3600 )) + (ixG60_PP * (TOI_PP / 3600 )) + (ixG60_SH * (TOI_SH / 3600 )),
                     total_G = (G60_EV * (TOI_EV / 3600 )) + (G60_PP * (TOI_PP / 3600 )) + (G60_SH * (TOI_SH / 3600 )),
                     Surplus.Goals = round(total_G - total_ixG,1)) %>%
                     select(Player, season, Pos, total_ixG, total_G, Surplus.Goals)
        

scoring.surplus.career <- skater.season.stats.wide %>%
  filter(season %in% c("20142015","20152016","20162017")) %>%
  group_by(Player) %>%
  summarise(total_ixG = sum((ixG60_EV * (TOI_EV / 3600 )) + (ixG60_PP * (TOI_PP / 3600 )) + (ixG60_SH * (TOI_SH / 3600 ))),
         total_G = sum((G60_EV * (TOI_EV / 3600 )) + (G60_PP * (TOI_PP / 3600 )) + (G60_SH * (TOI_SH / 3600 )))) %>%
  mutate(Surplus.Goals = round(total_G - total_ixG,1))


player.events <- pbp.all.xG %>%
        filter(season == "20162017") %>%
        group_by(ev.team, ev.player.1)
