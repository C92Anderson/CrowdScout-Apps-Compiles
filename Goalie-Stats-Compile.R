############################################################################################################################################################################
######## 0.A SYSTEM PREP
############################################################################################################################################################################
library(ggplot2);library(dplyr); library(DataCombine)
library(glmnet); library(nhlscrapr); library(caret); library(RMySQL); library(readr); library(reshape2); library(rvest)
library(twitteR);library(httr); library(ggrepel)

load("~/Documents/CWA/Hockey Data/xG.scored.data.RData")

library(ggplot2)
library(RMySQL)
library(dplyr)
library(scales)

conn <- dbConnect(MySQL(), user='ca_elo_games', password='cprice31!',
                  host='mysql.crowdscoutsports.com', db='nhl_all')
on.exit(dbDisconnect(conn))

goalie.roster <- dbGetQuery(conn, "SELECT distinct upper(playerName) as `SA.Goalie`, playerId as nhl_id, playerHeight as height,  
                            playerShootsCatches as Catches, playerBirthDate as dob FROM `nhl_all`.`hockey_goalies_roster` AS A")


goalie.roster <- goalie.roster %>%
  mutate(SA.Goalie = ifelse(SA.Goalie == "MATT MURRAY","MATTHEW MURRAY",
                     ifelse(SA.Goalie == "OLIE KOLZIG","OLAF KOLZIG",
                     ifelse(SA.Goalie == "STEPHEN VALIQUETTE","STEVE VALIQUETTE",
                     ifelse(SA.Goalie == "THOMAS MCCOLLUM","TOM MCCOLLUM",
                     ifelse(SA.Goalie == "JEAN-SEBASTIEN AUBIN", "J-SEBASTIEN AUBIN",
                    ifelse(SA.Goalie == "ALEXANDER PECHURSKIY","ALEXANDER PECHURSKI",
                                   SA.Goalie))))))) %>%
  unique()


############################################################################################################################################################################
########GOALIE SEASON LEVEL
############################################################################################################################################################################

goalie.data.prep <- scored.data %>%
  filter(season != "20072008") %>%
  mutate(SA.Goalie = ifelse(SA.Goalie == "STEPHEN VALIQUETTE","STEVE VALIQUETTE",
                     ifelse(SA.Goalie == "THOMAS MCCOLLUM","TOM MCCOLLUM",
                                   SA.Goalie)),
          awaystate = ifelse(nchar(a4) == 0, 3,
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
         SA.Goalie.gamestate = ifelse(ev.team != hometeam & home.gamestate == "PP", "PP",
                               ifelse(ev.team != hometeam & home.gamestate == "SH", "SH",
                               ifelse(ev.team == hometeam & home.gamestate == "PP", "SH",
                               ifelse(ev.team == hometeam & home.gamestate == "SH", "PP",
                               ifelse(home.gamestate == "EV","EV",""))))),
         Danger.Band = ifelse(xG <= quantile(scored.data$xG, c(.33, .66))[1], "LowDanger",
                              ifelse(xG <= quantile(scored.data$xG, c(.33, .66))[2], "MiddleDanger",
                                     "HighDanger")),
         NonRebound.Shot = ifelse(is.Rebound == 0, 1, 0),
         xG.FirstShot = ifelse(is.Rebound == 0, xG, 0)) 

rebound.goal.probability <- goalie.data.prep %>% 
  filter(is.Rebound == 1) %>%
  group_by(season) %>%
  summarise(rebound.goal.probability = sum(as.numeric(goal)-1) / n())

## Overall   
goalie.season <- goalie.data.prep %>%
      left_join(rebound.goal.probability, by = "season") %>%
      group_by(SA.Goalie, season) %>%
      summarise(xG.Lift.100Shots = (sum(xG) -sum(as.numeric(goal)-1)) / (n() / 100),
                Surplus.Pts = (sum(xG) -sum(as.numeric(goal)-1)) / (1 / 0.3603384),
                Games.Played = uniqueN(gcode),
                xGA = sum(xG),
                GA = sum(as.numeric(goal)-1),
                SA = n(),
                xG.xR.Lift.100Shots = ((sum(xG.FirstShot) + sum(pred.rebound * rebound.goal.probability)) -sum(as.numeric(goal)-1)) / (sum(NonRebound.Shot) / 100),
                NonRebound.Shots = sum(NonRebound.Shot),
                xG.FirstShot = sum(xG.FirstShot),
                xG.RB = sum(pred.rebound * rebound.goal.probability),
                Rebounds.Prevented.100Shots = (sum(pred.rebound) - sum(as.numeric(is.Rebound)-1)) / (sum(NonRebound.Shot) / 100),
                RBA = sum(as.numeric(is.Rebound)-1),
                xRBA = sum(pred.rebound),
                Mean.Shot.Distance = mean(shot.dist), 
                Mean.Shot.Angle = mean(shot.angle))

## Goalie Season Strength
goalie.season.strength <- goalie.data.prep %>%
      group_by(SA.Goalie, season, SA.Goalie.gamestate) %>%
      summarise(xG.Lift.100Shots = (sum(xG) -sum(as.numeric(goal)-1)) / (n() / 100),
                xGA = sum(xG),
                GA = sum(as.numeric(goal)-1),
                Rebounds.Prevented.100Shots = (sum(pred.rebound) - sum(as.numeric(is.Rebound)-1)) / ( sum(NonRebound.Shot) / 100),
                RBA = sum(as.numeric(is.Rebound)-1),
                xRBA = sum(pred.rebound),
                Mean.Shot.Distance = mean(shot.dist), 
                Mean.Shot.Angle = mean(shot.angle)) %>%
      data.frame() %>%
      melt(id.vars = c("SA.Goalie","season","SA.Goalie.gamestate")) %>%
      mutate(variable = paste0(variable,"_",SA.Goalie.gamestate)) %>%
      dcast(SA.Goalie + season ~ variable, value.var = "value")

## Goalie Season Rushes
goalie.season.rush <- goalie.data.prep %>%
  group_by(SA.Goalie, season, is.Rush) %>%
  summarise(xG.Lift.100Shots = (sum(xG) -sum(as.numeric(goal)-1)) / (n() / 100),
            xGA = sum(xG),
            GA = sum(as.numeric(goal)-1),
            SA = n(),
            Mean.Shot.Distance = mean(shot.dist), 
            Mean.Shot.Angle = mean(shot.angle)) %>%
  data.frame() %>%
  melt(id.vars = c("SA.Goalie","season","is.Rush")) %>%
  mutate(variable = ifelse(is.Rush == 1, paste0(variable,"_Rush"),
                    ifelse(is.Rush == 0, paste0(variable,"_NonRush"),""))) %>%
  dcast(SA.Goalie + season ~ variable, value.var = "value")

## Goalie Season Rebounds
goalie.season.rebound <- goalie.data.prep %>%
  group_by(SA.Goalie, season, is.Rebound) %>%
  summarise(xG.Lift.100Shots = (sum(xG) -sum(as.numeric(goal)-1)) / (n() / 100),
            xGA = sum(xG),
            GA = sum(as.numeric(goal)-1),
            SA = n()) %>%
  data.frame() %>%
  melt(id.vars = c("SA.Goalie","season","is.Rebound")) %>%
  mutate(variable = ifelse(is.Rebound == 1, paste0(variable,"_Rebound"),
                           ifelse(is.Rebound == 0, paste0(variable,"_NonRebound"),""))) %>%
  dcast(SA.Goalie + season ~ variable, value.var = "value")

## Goalie Season Shot Type
goalie.season.shottype <- goalie.data.prep %>%
  group_by(SA.Goalie, season, shot.type) %>%
  summarise(xG.Lift.100Shots = (sum(xG) -sum(as.numeric(goal)-1)) / (n() / 100),
            SA = n(),
            Rebounds.Prevented.100Shots = (sum(pred.rebound) - sum(as.numeric(is.Rebound)-1)) / ( sum(NonRebound.Shot) / 100)) %>%
  data.frame() %>%
  melt(id.vars = c("SA.Goalie","season","shot.type")) %>%
  mutate(variable = paste0(variable,"_",shot.type)) %>%
  dcast(SA.Goalie + season ~ variable, value.var = "value")

## Goalie Season Danger
goalie.season.danger <- goalie.data.prep %>%
  group_by(SA.Goalie, season, Danger.Band) %>%
  summarise(xG.Lift.100Shots = (sum(xG) -sum(as.numeric(goal)-1)) / (n() / 100),
            SA = n(),
            Rebounds.Prevented.100Shots = (sum(pred.rebound) - sum(as.numeric(is.Rebound)-1)) / ( sum(NonRebound.Shot)  / 100)) %>%
  data.frame() %>%
  melt(id.vars = c("SA.Goalie","season","Danger.Band")) %>%
  mutate(variable = paste0(variable,"_",Danger.Band)) %>%
  dcast(SA.Goalie + season ~ variable, value.var = "value")

## Goalie Season Handededness
goalie.season.handedness <- goalie.data.prep %>%
  group_by(SA.Goalie, season, Handed.Class2) %>%
  summarise(xG.Lift.100Shots = (sum(xG) -sum(as.numeric(goal)-1)) / (n() / 100),
            SA = n(),
            Rebounds.Prevented.100Shots = (sum(pred.rebound) - sum(as.numeric(is.Rebound)-1)) / ( sum(NonRebound.Shot)  / 100)) %>%
  data.frame() %>%
  melt(id.vars = c("SA.Goalie","season","Handed.Class2")) %>%
  mutate(variable = paste0(variable,"_",Handed.Class2)) %>%
  dcast(SA.Goalie + season ~ variable, value.var = "value")

## Replacement Level
replacement.level <- goalie.season %>%
      filter(SA < 200) %>%
      group_by() %>%
      summarise(xG.Lift.100Shots.ReplacementLevel = (sum(xGA) -sum(GA)) / (sum(SA) / 100)) %>%
      as.numeric()


## Join
goalie.season.all <- goalie.roster %>%
              inner_join(goalie.season, by = "SA.Goalie") %>%
              left_join(goalie.season.strength, by = c("SA.Goalie","season")) %>%
              left_join(goalie.season.danger, by = c("SA.Goalie","season")) %>%
              left_join(goalie.season.rush, by = c("SA.Goalie","season")) %>%
              left_join(goalie.season.handedness, by = c("SA.Goalie","season")) %>%
              left_join(goalie.season.rebound, by = c("SA.Goalie","season")) %>%
              left_join(goalie.season.shottype, by = c("SA.Goalie","season")) %>%
              mutate(Age.Season.Start = round((as.Date(paste0("10/1/",as.numeric(substr(season,1,4))), format = "%m/%d/%Y") - as.Date(dob)) / 365.25,1),
                     Surplus.Pts.AboveReplacement = (SA / 100) * ((xG.Lift.100Shots - replacement.level) / (1/0.3603384)))
                     
goalie.season.all[is.na(goalie.season.all)] <- 0            

############################################################################################################################################################################
########GOALIE BAYES
############################################################################################################################################################################

calcBetaMode <- function(aa, bb) { BetaMode <- (aa - 1)/(aa + bb - 2); return(BetaMode); }
calcBetaMean <- function(aa, bb) { BetaMean <- (aa)/(aa + bb); return(BetaMean); }
calcBetaSd   <- function(aa, bb) { BetaSd <- sqrt((aa * bb)/(((aa + bb)^2) * (aa + bb + 1))); return(BetaSd); }


goalie.season.betas <- goalie.data.prep %>%
      left_join(rebound.goal.probability, by = c("season")) %>%
      group_by(SA.Goalie, season) %>%
      summarise(xG.Lift.100Shots = (sum(xG) -sum(as.numeric(goal)-1)) / (n() / 100),
                NonRebound.Shots = sum(NonRebound.Shot),
                Shots = n(),
                xG.FirstShot = sum(xG.FirstShot),
                xG = sum(xG),
                xG.RB = sum(pred.rebound * rebound.goal.probability),
                xG.raw = sum(xG),
                #beta1000_a = (1 - ( (xG.FirstShot + xG.RB) / NonRebound.Shots)) * 1000,
                beta1000_a = (1 - (xG / Shots)) * 1000,
                beta1000_b = 1000 - beta1000_a,

                # Overall
                #likelihood_a = sum(NonRebound.Shot) - sum(as.numeric(goal)-1) + 1,  ## Saves + 1
                likelihood_a = sum(Shots) - sum(as.numeric(goal)-1) + 1,  ## Saves + 1
                likelihood_b = sum(as.numeric(goal)-1) + 1,  ## Goals + 1

                posterior1000_a = beta1000_a + (likelihood_a - 1),  ## Success + Beta A
                posterior1000_b = beta1000_b +  (likelihood_b - 1) , ## Goals + Beta B
       
                prior1000_mean      = calcBetaMean(beta1000_a, beta1000_b),
                posterior1000_mean  = calcBetaMean(posterior1000_a, posterior1000_b),
       
                posterior1000_lift = posterior1000_mean - prior1000_mean)


### Write Database
library(RMySQL)

conn <- dbConnect(MySQL(), user='ca_elo_games', password='cprice31!', host='mysql.crowdscoutsports.com', db='nhl_all')
on.exit(dbDisconnect(conn))

save(goalie.season.all, file="~/Documents/CWA/Hockey Data/goalie_season_stats.RData")
save(goalie.season.betas, file="~/Documents/CWA/Hockey Data/goalie_season_betas.RData")

dbWriteTable(conn, 'goalie_season_stats', as.data.frame(goalie.season.all), overwrite=T,row.names = FALSE)
dbWriteTable(conn, 'goalie_season_betas', as.data.frame(goalie.season.betas), overwrite=T,row.names = FALSE)





### Classify xG 
xGLift_Pctl <-quantile(goalie.season.all[goalie.season.all$NonRebound.Shots > 200, "xG.xR.Lift.100Shots"], probs = c(0.10,0.33, 0.5, 0.75,0.90))

goalie.performance <- goalie.season.all[goalie.season.all$NonRebound.Shots > 200, c("xG.xR.Lift.100Shots","SA.Goalie","season","NonRebound.Shots")] %>%
        mutate(p90 = ifelse(xG.xR.Lift.100Shots > as.numeric(xGLift_Pctl[5]),1,0),
               p75 = ifelse(xG.xR.Lift.100Shots > as.numeric(xGLift_Pctl[4]),1,0),
               p50 = ifelse(xG.xR.Lift.100Shots > as.numeric(xGLift_Pctl[3]),1,0),
               p33 = ifelse(xG.xR.Lift.100Shots > as.numeric(xGLift_Pctl[2]),1,0),
               p10 = ifelse(xG.xR.Lift.100Shots > as.numeric(xGLift_Pctl[1]),1,0))

goalie.performance %>% 
  group_by(season) %>% 
  summarise(p90 = sum(p90), p75 = sum(p75), p50 = sum(p50), p33 = sum(p33), p10 = sum(p10))

goalie.performance %>% 
  summarise(p90 = sum(p90) / uniqueN(season), p75 = sum(p75) / uniqueN(season), p50 = sum(p50) / uniqueN(season), p33 = sum(p33) / uniqueN(season), p10 = sum(p10) / uniqueN(season))

### Classify Bayesian Goalies
Bayesian_Pctl <-quantile(goalie.season.betas$posterior1000_lift, probs = c(0.10,0.33, 0.66, 0.8,0.90))

bayes.goalie.performance <- goalie.season.betas[ , c("posterior1000_lift","SA.Goalie","season","NonRebound.Shots")] %>%
  mutate(p90 = ifelse(posterior1000_lift > as.numeric(Bayesian_Pctl[5]),1,0),
         p80 = ifelse(posterior1000_lift > as.numeric(Bayesian_Pctl[4]),1,0),
         p66 = ifelse(posterior1000_lift > as.numeric(Bayesian_Pctl[3]),1,0),
         p33 = ifelse(posterior1000_lift > as.numeric(Bayesian_Pctl[2]),1,0),
         p10 = ifelse(posterior1000_lift > as.numeric(Bayesian_Pctl[1]),1,0))

bayes.goalie.performance %>% 
  group_by(season) %>% 
  summarise(p90 = sum(p90), p80 = sum(p80), p66 = sum(p66), p33 = sum(p33), p10 = sum(p10))

bayes.goalie.performance %>% 
  summarise(p90 = sum(p90) / uniqueN(season), p80 = sum(p80) / uniqueN(season), p66 = sum(p66) / uniqueN(season), p33 = sum(p33) / uniqueN(season), p10 = sum(p10) / uniqueN(season))

###################
#####Correlation

goalie.seasons <- goalie.season.betas %>% 
  select(SA.Goalie, season, posterior1000_lift) %>%
  left_join(goalie.season.all, by=c("SA.Goalie","season")) %>%
  filter(SA > 250) %>%
  mutate(SvPct = (SA - GA)/ SA) %>%
  ungroup() %>%
  select(-c(nhl_id, height, Catches, dob, season))
  #select(SA.Goalie, season, posterior1000_lift, SvPct, SA, xG.xR.Lift.100Shots) %>%
  #group_by(SA.Goalie) %>%
  mutate(lag.posterior1000_lift = ifelse(SA.Goalie == lag(SA.Goalie), lag(posterior1000_lift), NA),
         lag.SvPct = ifelse(SA.Goalie == lag(SA.Goalie), lag(SvPct), NA),
         lag.xG.xR.Lift.100Shots = ifelse(SA.Goalie == lag(SA.Goalie), lag(xG.xR.Lift.100Shots), NA)) %>%
  filter(!is.na(lag.posterior1000_lift))

for(i in colnames(goalie.seasons)) {
  
  print(paste(i))
}
  data <- goalie.seasons %>%
        mutate(lag.var = ifelse(SA.Goalie == lag(SA.Goalie), lag(i), NA)) %>%
        filter(!is.na(i))
        
    data <- data[c("lag.var", i)]

  print(head(data)) }
  corr <- corr(as.matrix(data))
  print(paste0(i,": ",corr))
}
  

Bayes.corr <- corr(as.matrix(goalie.seasons[c("lag.posterior1000_lift", "posterior1000_lift")]))
xG.xR.corr <- corr(as.matrix(goalie.seasons[c("lag.xG.xR.Lift.100Shots", "xG.xR.Lift.100Shots")]))

Sv.corr <- corr(as.matrix(goalie.seasons[c("lag.SvPct", "SvPct")]))



###################
#####PCA
###################

goalie.pca <- goalie.season.betas %>% 
  select(SA.Goalie, season, posterior1000_lift) %>%
  left_join(goalie.season.all, by=c("SA.Goalie","season")) %>%
  filter(SA > 250) %>%
  ungroup() %>%
  select(-c(SA.Goalie, nhl_id, season, Catches, dob,Age.Season.Start, Rebounds.Prevented.100Shots_Backhand))

# Scale
goalie.pca2 <- data.frame(scale(goalie.pca))
goalie.pca2[is.na(goalie.pca2)] <- 0

# Verify variance is uniform
plot(sapply(goalie.pca2, var))

# Proceed with principal components
g.pc <- princomp(goalie.pca2)
plot(g.pc)
plot(g.pc, type = "l")
summary(g.pc) # 3 components is both 'elbow' and explains >85% variance

g.pca2 <- prcomp(goalie.pca2,center = TRUE,scale. = TRUE)
loadings <- as.data.frame(g.pca2$rotation[,1:3])

# Get principal component vectors using prcomp instead of princomp
g.pc <- prcomp(goalie.pca2)
# First for principal components
comp <- data.frame(g.pc$x[,1:3])
# Plot
plot(comp, pch=16, col=rgb(0,0,0,0.5))

g.pc$

# Determine number of clusters 
wss <- (nrow(goalie.pca2)-1)*sum(apply(goalie.pca2,2,var)) 

for (i in 2:25) wss[i] <- sum(kmeans(goalie.pca2, centers=i, nstart = 25, iter.max = 1000)$withinss) 
plot(1:25, wss, type="b", xlab="Number of Clusters", ylab="Within groups sum of squares")


# From scree plot elbow occurs at k = 12
# Apply k-means with k=12
k <- kmeans(comp, 4, nstart=25, iter.max=1000)
library(RColorBrewer)
library(scales)
palette(alpha(brewer.pal(9,'Set1'), 0.5))
plot(comp, col=as.factor(k$clust), pch=16)


# Cluster sizes
Cluster = k$clust

cluster.xwalk <- data.frame(Cluster = as.numeric(1:max(k$clust)),
                            ClusterName = c('Matchup Dependant Depth',
                                            'Depth Defensive Player',
                                            'Depth Offensive Player',
                                            'Offensive Driver',
                                            'Matchup Dependant Defensive Player',
                                            'Matchup Dependant Offensive Player',
                                            'Depth Player',
                                            'Offensive Matchup Player ',
                                            'Depth Player',
                                            'Defensive Matchup Player',
                                            'Offensive Driver'))

# First cluster
goalie.season.clusters <- goalie.season.all %>% 
  filter(SA > 250) %>%
  select(SA.Goalie, season, xG.Lift.100Shots, Games.Played, Surplus.Pts) %>%
  cbind(Cluster) %>%
  left_join(cluster.xwalk, by = "Cluster") %>%
  mutate(Name = paste0(sapply(strsplit(as.character(Player), ' '), function(x) x[length(x)])))#,substr(season,7,8)))

player.list <- c(i)

goalie.season.clusters %>%
  #mutate(PlayerListed = ifelse(Player %in% player.list,Player,"")) %>%
  ggplot(aes(y=as.numeric(Surplus.Pts),x=as.factor(Cluster), label=SA.Goalie, color=as.factor(Cluster))) +
  #geom_point(position = position_jitterdodge()) +
  geom_text(angle = 45, check_overlap = TRUE) +
  geom_point() +
  #ggrepel::geom_label_repel(angle = 45, segment.color = 'grey50') +
  coord_flip() +
  labs(title = "Player Season Clusters and Estimated Ability, 2016-17", y="Predicted CrowdScout Score (0-100)", shape="Position",color="Cluster",x="")


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


