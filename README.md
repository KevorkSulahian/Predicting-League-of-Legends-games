# Predicting-League-of-Legends-games
This is a R language algorithm that predicts league of legends games
made for my class Sport Analytics 270.

### Let us start with the Librarys which I have used in the Code
```r
library(ggplot2)
library(dplyr)
library(gridExtra)
library(grid)
library(RColorBrewer)
library(fmsb)
library(ggrepel)
library(ggthemes)
library(ggdendro)
library(NbClust)
library(factoextra)
library(viridis)
library(plyr)
library(BradleyTerry2)
```
### Preparing the Data

```
players <- read.csv(file='LCSSummer.csv', sep=',', stringsAsFactors = F)
colnames(players)<-c("Name","Role","Team","Results","Total.Points",
                     "Avg.Points.Per.Game","Game.Played","Kill","Death","Assists",
                     "Creep.Kill","ten.A","triple.quadra.penta.kills")

players$triple.quadra.penta.kills<-NULL


#remove rows for players who never played
players <- players %>% dplyr::filter(Game.Played>0)


#make index for summary/details datasets
players$my_index<-ifelse(players$Name=="",2,1)


#function to impute the Name/Role in missing rows
#knowing that the first row -> summary --> not empty

for(i in 1:nrow(players)){
  if(players$Name[i]!=""){
    currentName <- players$Name[i]
    currentRole <- players$Role[i]
  }
  else if (players$Name[i]==""){
    players$Name[i] <- currentName
    players$Role[i] <- currentRole
  }       
}


#players_details
## players_details is the detail of each player on every unique game they have played
players_details <- players %>% dplyr::filter(my_index==2) %>% dplyr::select(-c(my_index))
players_details$Team1<-sapply(players_details$Team, function(x) strsplit(x," ")[[1]][1])
players_details$Team2<-sapply(players_details$Team, function(x) strsplit(x," ")[[1]][3])
players_details$Result<- sapply(players_details$Results, function(x) strsplit(x," ")[[1]][1])
players_details$KDA<-ifelse(players_details$Death>0,players_details$Kill*players_details$Assists/players_details$Death,players_details$Kill)
players_details$Multi<-ifelse(players_details$triple.kills>0 | players_details$quadra.kills>0 | players_details$penta.kills>0,'yes','no')
players_details$Team<-NULL
players_details$Results<-NULL


#players_summary
## players_summary is the sum of all the games each player has played
players_summary <- players %>% dplyr::filter(my_index==1) %>% dplyr::select(-c(my_index))

players_summary$KDA <- (players_summary$Kill + players_summary$Assists) / players_summary$Death
```
### Now we can start creating some graphs

```
listPlot1<-list()
for(i in 1:length(roles)){
  listPlot1[[i]]<-players_summary %>% 
    dplyr::filter(Role==roles[i]) %>% 
    ggplot(aes(x= reorder(Name,KDA),y= KDA,fill=Team)) + 
    geom_bar(stat='identity',width=.5) + coord_flip() + 
    scale_fill_manual(values=colorRampPalette(brewer.pal(9, "Set1"))(length(unique(players_summary$Team)))) + 
    theme_fivethirtyeight() + ggtitle(paste0('KDA\n',roles[i])) + 
    theme(axis.text.y = element_text(size=8),
          legend.title=element_blank(),legend.direction='vertical',
          legend.position='right',
          legend.text=element_text(size=8),
          legend.key.size = unit(.3, "cm"))
}
do.call(grid.arrange, c(listPlot1, ncol=2))
```
![](https://github.com/KevorkSulahian/Predicting-League-of-Legends-games/blob/master/2.png)

#### Now another graph just for overall KDA
```
# KDA overall
ggplot(players_summary,aes( x = Death , y= reorder(Name,Kill +Assists))) +
  geom_point(aes(color=Team), size = 3, alpha = 0.75) +
  scale_color_manual(values=colorRampPalette(brewer.pal(8, "Set1"))(length(unique(players_summary$Team)))) + 
  theme_fivethirtyeight()  + 
  ggtitle('x = Death,y = Kill + Assist') +
  theme(legend.position='right',legend.direction='vertical')
 ```
 ![](https://github.com/KevorkSulahian/Predicting-League-of-Legends-games/blob/master/3.png)

# PREDICTION TIME!!!  :heavy_check_mark: :heavy_check_mark:
## Calculating using the Pythagorean formula
### First we need to calculate the Win ratio

```
## Turning win and loss into numbers easier to count
players_details$Result[players_details$Result=="Win"]<- 1
players_details$Result[players_details$Result=="Loss"]<- 0
players_details$Result <- as.numeric(as.character(players_details$Result))
a <-aggregate(players_details$Result, by=list(name=players_details$Name), FUN=sum)
a
#for loose
players_details$Result[players_details$Result==1]<- 0
players_details$Result[players_details$Result==0]<- 1
players_details$Result <- as.numeric(as.character(players_details$Result))
b <-aggregate(players_details$Result, by=list(name=players_details$Name), FUN=sum)
b

#ordering the players summary to add the result of games
players_summary <-players_summary[order(players_summary$Name), ]
players_summary$SeriesWon <- a
players_summary$Series<- b
```
### Getting the winning percentage 

```
#calculating the WP of players
players_summary$WP<-players_summary$SeriesWon[2] / players_summary$Series[2]
players_summary$WP <- as.numeric(unlist(players_summary$WP))

mod <- lm(WP~KDA, data=players_summary)
summary(mod)

coefficients(mod)
```
####intercept: If run differential is 0, thus the team makes and allows equal number of points then you expect to win 50% of the games.
####slope: 1 unit increase in run differential increases the winning percentage by 0.1

### exponent is needed in order to complete the formula
Link for exponent [formula](https://en.wikipedia.org/wiki/Pythagorean_expectation)

```
exponent <- 1.50 + log((sum(players_summary$Kill) + sum(players_summary$Death))/sum(players_summary$Series[2])) + 0.45
```
exponent = 4.418451

## calculating the pythagorean WP

```
players_summary$WP_P <- players_summary$Kill^exponent/(players_summary$Kill^exponent + players_summary$Death^exponent)

```
## Calculating the errors

```
errors <- players_summary$WP - players_summary$WP_P
plot(errors, pch=16)
```
![](https://github.com/KevorkSulahian/Predicting-League-of-Legends-games/blob/master/4.png)

### Percentage of our errors
```
sqrt(mean(errors^2))
```
##### We got 37% error percentage, yes this is not a low rate. In the future we need to make our formulas better and include more elements

#### Deleting 2D's from our DB
```
players_summary$SeriesWon <- NULL
players_summary$Series <- NULL
```
### Now lets calculate the WP for all teams

```
team_summary <- ddply(players_summary, .(Team), summarize, kill = sum(Kill), death = sum(Death))
team_summary$WP <- team_summary$kill^exponent/(team_summary$kill^exponent + team_summary$death^exponent)

```
|                  Team  |kill| death|         WP|
| ---------------------- |:--:|:----:|----------:|
|1                Cloud9 | 634|   470| 0.78959969|
|2  Counter Logic Gaming | 612|   463| 0.77430317|
|3              Echo Fox |423 |   567| 0.21508381|
|4              FlyQuest | 496|   706| 0.17366289|
|5                Fnatic | 415|   205| 0.95755582|
|6            G2 Esports | 321|   249| 0.75440295|
|7                   H2K | 359|   225| 0.88739432|
|8             Immortals | 657|   523| 0.73260112|
|9               Misfits | 400|   346| 0.65493184|
|10   Mysterious Monkeys | 241|   436| 0.06789660|
|11    Ninjas in Pyjamas | 235|   395| 0.09157928|
|12             Phoenix1 | 565|   632| 0.37868164|
|13               ROCCAT | 282|   422| 0.14417110|
|14               Splyce | 362|   301| 0.69324927|
|15        Team Dignitas | 569|   590| 0.46005184|
|16            Team Envy | 462|   487| 0.44204936|
|17          Team Liquid | 561|   650| 0.34284909|
|18        Team Vitality | 225|   282| 0.26939079|
|19                  TSM | 581|   480| 0.69925819|
|20     Unicorns of Love | 416|   414| 0.50532324|

## Calculating using the Bradley Terry Model

```
BTM <- players_details %>%
  group_by(Name, Team1, Team2) %>%
  summarise(kill = sum(Kill), death = sum(Death))

BTM <- ddply(players_details, .(Name, Team1, Team2), summarize, kill = sum(Kill), death = sum(Death))

```
Note that sometimes one of the BTM's do not work but both are supposed to return the same values.

```
head(BTM, n= 5)
```
|                  Name  |Team1|Team2|Kill|Death|
| ---------------------- |:--:|:----:|:--:|----:|
|1                Adrian | DIG|   C9 | 2  |7    |
|2                Adrian | DIG|   CLG| 6  |13   |
|3             Adrian    |DIG |   FOX| 2  |7    |
|4              Adrian   | DIG|   IMT| 2  |4    |
|5                Adrian | DIG|   P1 | 2  |10   |

```
model<- BTm(cbind(Kill,Death),Team1,Team2,data=players_details, id="")
summary(model)
```
#### We will get from the Summary
```
Call:
BTm(outcome = cbind(Kill, Death), player1 = Team1, player2 = Team2, 
    id = "GAME_", data = players_details)

Deviance Residuals: 
    Min       1Q   Median       3Q      Max  
-4.3738  -1.2777  -0.2204   0.9815   4.3768  

Coefficients: (1 not defined because of singularities)
         Estimate Std. Error z value Pr(>|z|)    
GAME_CLG -0.09771    0.05574  -1.753 0.079619 .  
GAME_DIG -0.33734    0.05701  -5.917 3.28e-09 ***
GAME_FLY -0.61498    0.05751 -10.694  < 2e-16 ***
GAME_FNC  0.78349    0.08597   9.114  < 2e-16 ***
GAME_FOX -0.56081    0.06038  -9.287  < 2e-16 ***
GAME_G2   0.43675    0.08444   5.172 2.31e-07 ***
GAME_H2K  0.63266    0.08316   7.608 2.79e-14 ***
GAME_IMT -0.07386    0.05563  -1.328 0.184226    
GAME_MM  -0.36835    0.08258  -4.461 8.17e-06 ***
GAME_MSF  0.31410    0.07767   4.044 5.26e-05 ***
GAME_NIP -0.28852    0.08368  -3.448 0.000565 ***
GAME_NV  -0.32442    0.05928  -5.472 4.44e-08 ***
GAME_P1  -0.43747    0.05808  -7.533 4.96e-14 ***
GAME_ROC -0.16373    0.08292  -1.975 0.048313 *  
GAME_SPY  0.34782    0.08206   4.238 2.25e-05 ***
GAME_TL  -0.43706    0.05984  -7.304 2.80e-13 ***
GAME_TSM -0.11203    0.05838  -1.919 0.054983 .  
GAME_UOL  0.17971    0.07945   2.262 0.023697 *  
GAME_VIT       NA         NA      NA       NA    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

(Dispersion parameter for binomial family taken to be 1)

    Null deviance: 4437.5  on 1495  degrees of freedom
Residual deviance: 3733.7  on 1477  degrees of freedom
AIC: 7438.8

Number of Fisher Scoring iterations: 4
```
### Get the coefficients in order to start predicting
```
coef <- model$coefficients
sort(coef, decreasing = T) # sorts from the best to the worst team


length(unique(players_details$Team1))
# 20 teams overall

length(coef)
# and we 19 coefs
```
|Team  |Ability|s.e.    |
| ---- |:-----:|:------:|
|1  FNC| 0.78  |    0.08|
|2  H2k| 0.63  |    0.08|
|3  G2 | 0.43  |0.08    |
|4  SPY| 0.34  |    0.08|
|5  MSF| 0.31  |    0.07|
|6  UOL| 0.17  |    0.07|
|7  C9 | 0.00  |    0.00|
|8  IMT| -0.07 |    0.05|
|9  CLG| -0.09 |    0.05|
|10 TSM| -0.11 |    0.05|
|11 ROC| -0.16 |    0.08|

#### Lets first calculate Automatically
```
TeamSoloMid <- data.frame(Team1=rep("TSM", 2),
                          Team2=c("Cloud9", "H2K")) ## we are tying to get TSM versus two team Cloud 9 and H2k

team <- model$coefficients
TeamSoloMid$Team1<- factor(TeamSoloMid$Team1,levels=team_summary$Team) #setting the levels

TeamSoloMid$Team2<-factor(TeamSoloMid$Team2,levels=team_summary$Team) # setting the leves

TSM_P <- predict(model, newdata = TeamSoloMid, level = 2, type = "response",scale=NULL)

TSM_P # gives us that the probability of tsm winning against Cloud 9 is 47% and vs H2K is 32% (But we all know NA > EU )  :sweat_smile: 

# now we try to get the win percentage for both teams
TSM_df <- data.frame(TeamSoloMid, Team1= TSM_P, Team2 = 1- TSM_P)

TSM_df
```
|Team1|Team2  |Team1$WP|Team2$WP|
| --- |:-----:|:------:|-------:|
|1 TSM| Cloud9|     47%|     53%|
|2 TSM| H2K   |     32%|     68%|

#### Second We calculate Manually
``` 
TSM_ab <- coef1[rownames(coef1)=="TSM",1]
CLG_ab<- coef1[rownames(coef1)=="CLG",1]
TL_ab <- coef1[rownames(coef1)=="TL",1]

exp(TSM_ab)/(exp(TSM_ab)+(exp(CLG_ab)))
# 49% chance of TSM winning
exp(TSM_ab)/(exp(TSM_ab)+(exp(TL_ab)))
# 58% chance of TSM winning
```

## predicition based on elo Rating
#### Note that we are using different files elo.r for this section 
## the idea
### Assign performance rating to each team. This rating is going to show the ability of the team to win
### After each game update the rating of the player based on the outcome.
### Using the ability scores to predict the match outcome of the game.

```
team_details <- ddply(players_details, .(Team1, Team2, Result), summarize, kill = sum(Kill), death = sum(Death))
team_details$League <- c("Summer Split")
```

### Starting with Elo's
```


elos<-elo.run(score(kill,death)~ Team1+ Team2, data=team_details,k=20)

elos_df<-as.data.frame(elos)

head(elos_df, n = 20) # 20 teams only

dim(elos_df) #DF has only 460 entries

# getting the last teams who played in the DF

LCS_last<-tail(elos_df,n=4)
LCS_last[order(LCS_last$elo,decreasing = T),2:3]

```
#### Since the DF which I have was not complete I had to create a virtural round to predic the games from
#### the round is called round.csv
```
Home Opponent home_win_prob away_win_prob
1  NIP       G2          0.31          0.69
2   P1      DIG          0.47          0.53
```
#### Defining the Home and Away team
```
elo_h<-left_join(Round,LCS_last,by=c("Home"="team"))

elo_h

elo_a<-left_join(Round,LCS_last,by=c("Opponent"="team"))

elo_a

elo_a<-elo_a[,"elo"]

elo_h<-elo_h[,"elo"]

ht_W <- elo.prob(elo_h, elo_a)

# if the DB was complete and the round was big we could have used this to get the prob's for all the games
Round$home_win_prob <- c(0.5505317)
Round$away_win_prob<- 1- Round$home_win_prob
```
### Calculating the elo for each game
```
LCS<-as.data.frame(elo.run(score(kill,death)~
                              Team1+Team2,data=team_details,k=20))
```
### Calculating the G where it is the score difference. I used the same formula from the website Eloratings.net
```
team_details$G<-ifelse(abs(team_details$kill - team_details$death)<=1, 1,
                    ifelse(abs(team_details$kill - team_details$death)==2, 1.5,
                           (abs(team_details$kill - team_details$death)+11)/8))
```
### just in case if the data was complete we could have calculated the K factor, so I added League myself
```
team_details$K<- ifelse(team_details$League == "",20,
                     ifelse(team_details$League == "promotion",30,
                            ifelse(team_details$League == "Round Robinr", 40,
                                   ifelse(team_details$League == "Summer Split" || "Winter Split",50,
                                          ifelse(team_details$League == "World Championship", 60)))))
```
### and the final elo
```
LCS2<-as.data.frame(elo.run(score(kill,death) ~ Team1 + Team2+
                              k(team_details$K * team_details$G), data =team_details))
```
|Game  |Team   | Elo    |
| ---- |:-----:|:------:|
|0     | C9    |    1500|
|0     | CLG   |    1500|
|0     | DIG   |1500    |
|1     | C9    |1390.625|
|1     | CLG   |1609.375|
|1     | DIG   |1586.345|


### divivded the LCS DB into 2 (home, away) I figured that all home teams were odd numbers in LCS2 and all away teams were even hat is why I did it this way.

```
LCS2 <- tail(LCS2, -20)
## the first 20 which are the teams before they played any match were deleted
elo_home <- LCS2[seq(1, nrow(LCS2), 2),]
elo_away <- LCS2[seq(0, nrow(LCS2), 2),]
team_details$ELO_H<-  elo_home$elo
team_details$ELO_A<-  elo_away$elo
## Calculating the Home Prob
team_details$Home_Prob <- elo.prob(team_details$ELO_H, team_details$ELO_A)
## calculating the actual score
team_details$error <- abs(team_details$Home_Prob - team_details$Result)
sqrt(mean(team_details$error^2))
# 37 percent error which is same as the Pythagorean formula error
```
### the prob's based on the Elo rating can be found in the team_details Data

So we come to an end. Hope you liked what I've done and I can't wait to hear your opinions and see your comments.
Thanks for reading
