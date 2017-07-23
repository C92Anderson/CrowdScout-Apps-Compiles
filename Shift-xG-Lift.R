
############################################################################################################################################################################
######## 0.A SYSTEM PREP
############################################################################################################################################################################
library(ggplot2);library(dplyr); library(DataCombine)
library(glmnet); library(nhlscrapr); library(caret); library(RMySQL); library(readr); library(reshape2); library(rvest)
library(twitteR);library(httr); library(data.table); library("reshape2");
library(ggplot2);library(dplyr); library(DataCombine)
library(glmnet); library(nhlscrapr); library(caret); library(RMySQL); library(readr); library(reshape2); library(rvest)
library(twitteR);library(httr); library(data.table); library("reshape2");
theme_set(theme_bw())

txt <- element_text(size = 18, colour = "grey25", face = "plain")
bold_txt <- element_text(size = 20, colour = "navy", face = "bold")

theme_standard <- function(base_size = 16, base_family = "") {
  theme_bw(base_size = base_size, base_family = base_family) +
    theme(
      strip.background = element_blank(), 
      
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_line( colour = "white", size = 2), 
      
      panel.background = element_rect(fill="grey90"),
      plot.background = element_rect(fill="grey90"),
      legend.background = element_rect(fill="grey90"),
      legend.key = element_rect(fill="grey90", size = 20),
      legend.key.size = unit(1,"cm"),
      
      panel.border = element_blank(), 
      
      line = element_line( colour = "white", size = 2),
      axis.text.x = element_text(angle = 90, hjust = 1),
      text = txt, 
      plot.title = bold_txt, 
      
      axis.title = txt, 
      axis.text = txt, 
      
      legend.title = bold_txt, 
      legend.text = txt ) 
}

 p + theme_standard() + scale_color_gdocs()
   
   
   
load("~/Documents/CWA/Hockey Data/skater.shift.level.RData")

### Calculate shift expected xG
skater.shift.level.clean <- skater.shift.level %>%
            left_join(roster_all, by="Player") %>%
            mutate(GD = GF - GA,
                   Pos = ifelse(Pos == "C","C",
                         ifelse(Pos == "D","D","W")))

# Remove Nan, Inf
library(data.table)
for (j in 1:ncol(skater.shift.level.clean)) set(skater.shift.level.clean, which(is.infinite(skater.shift.level.clean[[j]])), j, NA)
for (j in 1:ncol(skater.shift.level.clean)) set(skater.shift.level.clean, which(is.nan(skater.shift.level.clean[[j]])), j, NA)

skater.shift.level.clean <- skater.shift.level.clean[complete.cases(skater.shift.level.clean),]


## Shift-Level Model
shift.xG.model <- lm(data = skater.shift.level.clean,
                     xGD ~ PlayerVenue + Team.Score + Opposition.Score + Team.Strength + Opposition.Strength + #Pos +
                      Mean.Teammates + Mean.Competition + Max.Teammates + Max.Competition + Min.Teammates + Min.Competition + #Game.Minute +
                       Off.FO.Shift + Def.FO.Shift + Neu.FO.Shift + OTF.Shift)


shift.xG.model %>% summary()
shift.xG.model %>% varImp(scale=F)

varimp.scales <- varImp(shift.xG.model, scale=TRUE) 
varimp.scales %>%
  
  arrange(Overall)
  ggplot(aes(varimp.scales$Overall)) + 
  labs(title="Random Forest Model Predicting CrowdScout Score\nVariable Importance") +
  geom_bar(fill="purple", stat= "identity")

## Predict Shot Angle
expected.xG.shift <- predict(shift.xG.model, skater.shift.level.clean) 

## Combine Data and Impute
skater.shift.level.clean <- cbind(expected.xG.shift,skater.shift.level.clean) 

shift.error <- skater.shift.level.clean$expected.xG.shift - skater.shift.level.clean$xGD

# Mean Absolute Error
mean(abs(shift.error))
# 0.04997407

# RMSE
sqrt(mean(shift.error^2))
## 0.09217408

save(skater.shift.level.clean, file="~/Documents/CWA/Hockey Data/skater.shift.level.clean.RData")
load("~/Documents/CWA/Hockey Data/skater.shift.level.clean.RData")



# train_control <- trainControl(method="cv", number=10)
# 
# shift.xG.model.rf <- caret::train(xGD ~ PlayerVenue + Team.Score + Opposition.Score + Team.Strength + Opposition.Strength + Pos +
#                                      Mean.Teammates + Mean.Competition + Max.Teammates + Max.Competition + Min.Teammates + Min.Competition + #Game.Minute +
#                                      Off.FO.Shift + Def.FO.Shift + Neu.FO.Shift + OTF.Shift,
#                                    data=skater.shift.level.clean, trControl=train_control, method="lm",
#                                    tuneLength=6, ntree=500, importance = TRUE)
# 
# 


### Cumulative Impact
player.impact <- skater.shift.level.clean %>%
          #filter(season %in% c("20162017")) %>%
          group_by(Player) %>%
          arrange(Player, season, Player.Shift.Number) %>%
          mutate(#Team.GF = sum(GF, na.rm=T),
                 #Team.GA = sum(GA, na.rm=T),
                 #Actual.GD = Team.GF - Team.GA,
                   
                 #Player.G = sum(G, na.rm=T),
                 #Player.ixG = sum(ixG, na.rm=T),
                 #Player.Shooting = Player.G - Player.ixG,
            
                 Player.Shift.Number = Player.Shift.Number - min(Player.Shift.Number),
                 
                 cum.xGD = cumsum(xGD),
                 cum.expected.xGD = cumsum(expected.xG.shift),
                 Shift.xG.Lift = cum.xGD - cum.expected.xGD) %>%
          ungroup() %>%
          group_by(Player, season, gcode) %>%
          do(tail(., n=1)) %>%
          arrange(Player, season, Player.Shift.Number)
    
  
  players <- c("JOSH JOORIS","JUSSI JOKINEN","MICHAEL CAMMALLERI","KEVIN SHATTENKIRK","ALEXANDER RADULOV","KARL ALZNER","MARTIN HANZAL","JOE THORNTON","JUSTIN WILLIAMS","RADIM VRBATA","PATRICK MARLEAU",
                     "ANDREI MARKOV","JAROMIR JAGR","BRIAN BOYLE","SAM GAGNER","NICK BONINO","MICHAEL STONE","MIKE FISHER","RON HAINSEY","THOMAS VANEK","MICHAEL DEL ZOTTO","DMITRY KULIKOV","DREW STAFFORD","JOHNNY ODUYA","PATRICK SHARP","CODY FRANSON","TREVOR DALEY","NAIL YAKUPOV","JORDAN WEAL","MATT HUNWICK")

  players <- c("JACCOB SLAVIN")
  
  select.players <- player.impact %>%
            filter(Player %in% players)
  
  
  # Overlay select goalies on all goalies limited to season  
  ggplot(data=player.impact, aes(x=Player.Shift.Number,y=Shift.xG.Lift, group=Player)) + 
    theme_standard() + scale_color_gdocs() +
    geom_line(color="grey70") +
    annotate("segment",x=0,y=0,xend=max(player.impact$Player.Shift.Number),yend=0, size = 2, color = "white") +
    geom_line(data=select.players,size=2.5,aes(x=Player.Shift.Number,y=Shift.xG.Lift,color=as.factor(select.players$Player))) +
    geom_line(data=select.players,size=0.3,aes(x=Player.Shift.Number,y=Shift.xG.Lift,alpha=as.factor(select.players$season)), color="grey25") +
    #geom_point(data=select.players,size=1,aes(x=Player.Shift.Number,y=Shift.xG.Lift,shape=as.factor(select.players$season),alpha=as.factor(select.players$season))) + 
    labs(shape ="Season", color="Player") +
    labs(title="Player Shift-Level xG Impact, xG Team Differential - Expected xG Team Differential\nTeam xG explained by Venue, Team Strength, Score, Shift Start, Teammates & Competition\nxG Adjusted for Multiplicativity") +
    labs(x="Cumulative Player Shifts", y="Player xG Team Differential - Expected xG Team Differential",alpha="Season") +
    #ggrepel::geom_text_repel(data=last.game,aes(x=cum.Shots,y=QREAM,label = SA.Goalie),
    #                         point.padding = unit(0.5, "lines"),
    #                         segment.color = 'black') +
    annotate("text", x = 1, y = (max(player.impact$Shift.xG.Lift) * 0.5), hjust=0, size = 5, label = "Positive xG Impact\nControlling for Use") +
    annotate("text", x = 1, y = -1 * (max(player.impact$Shift.xG.Lift) * 0.5), hjust=0,size = 5, label = "Negative xG Impact\nControlling for Use") +
    annotate("text", x = 1, y = (max(player.impact$Shift.xG.Lift) * 0.8), hjust=0,size = 5, label = "@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model")
   

player.season.xGD <- skater.shift.level.clean %>%
          filter(Team.Strength == Opposition.Strength) %>%
          group_by(Player, Pos, season) %>%
          summarise(xG.Lift = sum(xGD, na.rm=T) - sum(expected.xG.shift, na.rm=T),
                    Shifts = max(Player.Shift.Number) - min(Player.Shift.Number),
                    Lift.per100Shift = xG.Lift / (Shifts / 100),
                    xGD.per100Shift = sum(xGD, na.rm=T) / (Shifts / 100),
                    expected.xGD.per100Shift = sum(expected.xG.shift, na.rm=T) / (Shifts / 100))


### 5v5 Coach Usage

player.season.xGD %>%
    filter(season == "20162017") %>%
    inner_join(roster_current, by = "Player") %>%
    mutate(Pos = ifelse(Pos.y == "D","D","F"),
           Name = sapply(strsplit(as.character(Player), ' '), function(x) x[length(x)])) %>%
    ggplot() +
    #geom_boxplot(aes(x=reorder(team,expected.xGD.per100Shift),y=expected.xGD.per100Shift, group=interaction(team,Pos), color=Pos)) +
    geom_point(aes(x=reorder(team,expected.xGD.per100Shift),y=expected.xGD.per100Shift,  size = Shifts, color = as.factor(Pos)), alpha=0.4) +
    geom_text(aes(x=reorder(team,expected.xGD.per100Shift),y=expected.xGD.per100Shift, label=Name, color = as.factor(Pos)), angle=90, alpha=0.4, check_overlap = TRUE) +
    annotate("segment", x=0.5, xend=29.5, y=0, yend=0, color="grey50") +
  
    annotate("text", x = 15, y = 0.25, hjust=0, color ="grey50", size = 2.5, label = "Positive Expected xG Impact\nControlling for Use\nEasier Minutes") +
    annotate("text", x = 15, y = -0.5, hjust=0, color ="grey50", size = 2.5, label = "Negative Expected xG Impact\nControlling for Use\nTougher Minutes") +
    labs(title = "Player Expected xG Differential by Team, 2016-17", x="", color = "Position",
         y="Expected xG Team Differential per 100 Shifts\nTeam xG explained by Venue, Team Strength, Score, Shift Start, Teammates & Competition\nxG Adjusted for Multiplicativity ") +
    coord_flip()


## 5v5 Results
player.season.xGD %>%
  filter(season == "20162017" & Shifts > 500) %>%
  inner_join(roster_current, by = "Player") %>%
  mutate(Pos = ifelse(Pos.y == "D","D","F"),
         Name = sapply(strsplit(as.character(Player), ' '), function(x) x[length(x)])) %>%
  ggplot() +
  #geom_boxplot(aes(x=reorder(team,expected.xGD.per100Shift),y=expected.xGD.per100Shift, group=interaction(team,Pos), color=Pos)) +
  geom_point(aes(x=reorder(team,Lift.per100Shift),y=Lift.per100Shift,  size = Shifts, color = as.factor(Pos)), alpha=0.4) +
  geom_text(aes(x=reorder(team,Lift.per100Shift),y=Lift.per100Shift, label=Name, color = as.factor(Pos)), angle=90, alpha=0.4, check_overlap = TRUE) +
  annotate("segment", x=0.5, xend=29.5, y=0, yend=0, color="grey50") +
  
  annotate("text", x = 15, y = 0.75, hjust=0, color ="grey50", size = 2.5, label = "Positive xG Impact\nControlling for Use") +
  annotate("text", x = 15, y = -0.75, hjust=0, color ="grey50", size = 2.5, label = "Negative xG Impact\nControlling for Use") +
  labs(title = "Player Actual - Expected xG Differential by Team, 2016-17", x="", color = "Position",
       y="Actual - Expected xG Team Differential per 100 Shifts\nTeam xG explained by Venue, Team Strength, Score, Shift Start, Teammates & Competition\nxG Adjusted for Multiplicativity ") +
  coord_flip()

### Player xG Componenets

### Player Componenets
player <- c("BRIAN CAMPBELL")

player.components <- player.impact %>%
  filter(Player %in% player)

ggplot() +
  theme_standard() + scale_color_gdocs() +
  geom_line(data=player.components,size=2.5,aes(x=Player.Shift.Number,y=Shift.xG.Lift,color="Player Impact")) +
  geom_line(data=player.components,size=0.75,aes(x=Player.Shift.Number,y=Shift.xG.Lift,alpha=as.factor(player.components$season))) +
  
  geom_line(data=player.components,size=2.5,aes(x=Player.Shift.Number,y=cum.expected.xGD,color="Expected xG Differential")) +
  geom_line(data=player.components,size=0.75,aes(x=Player.Shift.Number,y=cum.expected.xGD,alpha=as.factor(player.components$season))) +
    
  geom_line(data=player.components,size=2.5,aes(x=Player.Shift.Number,y=cum.xGD,color="Actual xG Differential")) +
  geom_line(data=player.components,size=0.07,aes(x=Player.Shift.Number,y=cum.xGD,alpha=as.factor(player.components$season))) +
    
  #geom_point(data=select.players,size=1,aes(x=Player.Shift.Number,y=Shift.xG.Lift,shape=as.factor(select.players$season),alpha=as.factor(select.players$season))) + 
  labs(shape ="Season", color="Component") +
  #annotate("segment",x=0,y=0,xend=max(player.components$Player.Shift.Number),yend=0, color="white") +
  geom_hline(yintercept = 0, color = "white") +
  labs(title=paste0(paste(player, sep=",", collapse=", ")," Shift-Level xG Impact, xG Team Differential - Expected xG Team Differential\nTeam xG explained by Venue, Team Strength, Score, Shift Start, Teammates & Competition\nxG Adjusted for Multiplicativity")) +
  labs(x="Cumulative Player Shifts", y="Player xG Team Differential - Expected xG Team Differential",alpha="Season") +

  annotate("text", x = max(player.components$Player.Shift.Number) * 0.8, y = 5, hjust=1, color ="grey25", size = 5, label = "Positive xG Impact\nControlling for Use") +
  annotate("text", x = max(player.components$Player.Shift.Number) * 0.8, y = -5, hjust=1,color ="grey25", size = 5, label = "Negative xG Impact\nControlling for Use") +
  annotate("text", x = 1, y = max(abs(player.components$Shift.xG.Lift),abs(player.components$cum.xGD)) * 0.8, hjust=0, size = 6, label = "@CrowdScoutSprts\nxG Model built using nhlscrapr\ngithub.com/C92Anderson/xG-Model") +
  facet_wrap(~Player)

#### Repeatability
skater.shift.level.clean$Rand  <- rbinom(nrow(skater.shift.level.clean), 1, 0.5)

shift.splits <- skater.shift.level.clean %>%
        filter(season == "20162017") %>%
        group_by(Player, Rand) %>%
          summarise(xG.Lift = sum(xGD, na.rm=T) - sum(expected.xG.shift, na.rm=T),
                    GD.per100Shifts = sum(GD) / (n() / 100),
                    Shifts = n(),
                    Lift.per100Shift = xG.Lift / (Shifts / 100))

xG.shift.splits <- shift.splits %>%
    dcast(Player ~ Rand, value.var="Lift.per100Shift")

xG.shift.splits <-  skater.shift.level.clean %>%
  group_by(Player) %>%
  summarise(Shifts = n()) %>%
  inner_join(xG.shift.splits, by="Player") %>%
  filter(Shifts > 1000)
  
xGLift.corr <- corr(as.matrix(xG.shift.splits[c("0", "1")]), w = (xG.shift.splits$Shifts))



GD.shift.splits <- shift.splits %>%
  dcast(Player ~ Rand, value.var="GD.per100Shifts")

GD.shift.splits <-  skater.shift.level.clean %>%
  group_by(Player) %>%
  summarise(Shifts = n()) %>%
  inner_join(GD.shift.splits, by="Player") 

GDLift.corr <- corr(as.matrix(GD.shift.splits[c("0", "1")]))


# Plot Sox
xG.shift.splits %>%
  filter(Shifts > 1000) %>%
  mutate(Player2 = ifelse(Player %in% c("SIDNEY CROSBY","CONNOR MCDAVID",players),Player,"")) %>%
  ggplot() +
  annotate("segment",x=-1,xend=1,y=0,yend=0) +
  annotate("segment",y=-1,yend=1,x=0,xend=0) +
  geom_point(aes(x=`0`,y=`1`,size=Shifts, color=Shifts)) +
  geom_text(aes(x=`0`,y=`1`,label=Player2)) +
  scale_color_gradient2(low="light grey",high="blue", guide = FALSE) +
  annotate("segment", x = -1, y = -1, xend = 1, yend = 1) +
  annotate("text", x = -1.5, y = 1.5, hjust = 0, label = paste0("Intra-season correlation: ", round(xGLift.corr,2))) +
  labs(title="Intra-Season Correlation - Player Shift-Level xG Impact, xG Team Differential - Expected xG Team Differential\nTeam xG explained by Venue, Team Strength, Score, Shift Start, Teammates & Competition\n@CrowdScoutSprts - github.com/C92Anderson/xG-Model") +
  labs(x="Player xG Team Differential - Expected xG Team Differential / 100 Shifts - Random Split 1",y="Player xG Team Differential - Expected xG Team Differential  / 100 Shifts -Random Split 2",size="Total Shifts") +
  #xlim(-2,2) +
  #ylim(-2,2) +
  theme(panel.background = element_blank()) 



# Plot Sox
GD.shift.splits %>%
  filter(Shifts > 1000) %>%
  mutate(Player2 = ifelse(Player %in% c("SIDNEY CROSBY","CONNOR MCDAVID",players),Player,"")) %>%
  ggplot() +
  annotate("segment",x=-1,xend=1,y=0,yend=0) +
  annotate("segment",y=-1,yend=1,x=0,xend=0) +
  geom_point(aes(x=`0`,y=`1`,size=Shifts, color=Shifts)) +
  geom_text(aes(x=`0`,y=`1`,label=Player2)) +
  scale_color_gradient2(low="light grey",high="blue", guide = FALSE) +
  annotate("segment", x = -1, y = -1, xend = 1, yend = 1) +
  annotate("text", x = -1.5, y = 1.5, hjust = 0, label = paste0("Intra-season correlation: ", round(GDLift.corr,2))) +
  labs(title="Intra-Season Correlation - Player Shift-Level xG Impact, xG Team Differential - Expected xG Team Differential\nTeam xG explained by Venue, Team Strength, Score, Shift Start, Teammates & Competition\n@CrowdScoutSprts - github.com/C92Anderson/xG-Model") +
  labs(x="Player xG Team Differential - Expected xG Team Differential / 100 Shifts - Random Split 1",y="Player xG Team Differential - Expected xG Team Differential  / 100 Shifts -Random Split 2",size="Total Shifts") +
  #xlim(-2,2) +
  #ylim(-2,2) +
  theme(panel.background = element_blank()) 
