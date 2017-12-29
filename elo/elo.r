library(dplyr)
library(plyr)
library(elo)

## Preparing the data
players <- read.csv(file='LCSSummer.csv', sep=',', stringsAsFactors = F)
colnames(players)<-c("Name","Role","Team","Results","Total.Points",
                     "Avg.Points.Per.Game","Game.Played","Kill","Death","Assists",
                     "Creep.Kill","ten.A","triple.quadra.penta.kills")

players$triple.quadra.penta.kills<-NULL

players <- players %>% dplyr::filter(Game.Played>0)

players$my_index<-ifelse(players$Name=="",2,1)


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

players_details <- players %>% dplyr::filter(my_index==2) %>% dplyr::select(-c(my_index))
players_details$Team1<-sapply(players_details$Team, function(x) strsplit(x," ")[[1]][1])
players_details$Team2<-sapply(players_details$Team, function(x) strsplit(x," ")[[1]][3])
players_details$Result<- sapply(players_details$Results, function(x) strsplit(x," ")[[1]][1])
#players_details$KDA<-ifelse(players_details$Death>0,players_details$Kill*players_details$Assists/players_details$Death,players_details$Kill)
#players_details$Multi<-ifelse(players_details$triple.kills>0 | players_details$quadra.kills>0 | players_details$penta.kills>0,'yes','no')
players_details$Team<-NULL
players_details$Results<-NULL
players_details$ten.A <- NULL
players_details$Result[players_details$Result=="Win"]<- 1
players_details$Result[players_details$Result=="Loss"]<- 0
players_details$Result <- as.numeric(as.character(players_details$Result))
team_details <- ddply(players_details, .(Team1, Team2, Result), summarize, kill = sum(Kill), death = sum(Death))
team_details$League <- c("Summer Split")


elos<-elo.run(score(kill,death)~ Team1+ Team2, data=team_details,k=20)

elos_df<-as.data.frame(elos)

head(elos_df, n = 20) # 20 teams only

dim(elos_df) #DF has only 380 entries

LCS_last<-tail(elos_df,n=2)
LCS_last[order(LCS_last$elo,decreasing = T),2:3]
# I created this round
Round<-read.csv("Round.csv")

elo_h<-left_join(Round,LCS_last,by=c("Home"="team"))

elo_h

elo_a<-left_join(Round,LCS_last,by=c("Opponent"="team"))

elo_a

elo_a<-elo_a[,"elo"]

elo_h<-elo_h[,"elo"]

ht_W <- elo.prob(elo_h, elo_a)

Round$home_win_prob <- c(0.5505317)
Round$away_win_prob<- 1- Round$home_win_prob

LCS<-as.data.frame(elo.run(score(kill,death)~
                              Team1+Team2,data=team_details,k=20))

team_details$G<-ifelse(abs(team_details$kill - team_details$death)<=1, 1,
                    ifelse(abs(team_details$kill - team_details$death)==2, 1.5,
                           (abs(team_details$kill - team_details$death)+11)/8))


#just in case if the data was complete we could have calculated the K factor, so I added League myself
team_details$K<- ifelse(team_details$League == "",20,
                     ifelse(team_details$League == "promotion",30,
                            ifelse(team_details$League == "Round Robinr", 40,
                                   ifelse(team_details$League == "Summer Split" || "Winter Split",50,
                                          ifelse(team_details$League == "World Championship", 60)))))

LCS2<-as.data.frame(elo.run(score(kill,death) ~ Team1 + Team2+
                              k(team_details$K * team_details$G), data =team_details))

View(LCS2)



## divivded the LCS DB into 2 (home, away) I figured that all home teams were odd numbers in LCS2 and all away teams were even
## that is why I did it this way
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


team_details$Result_of_prob <-ifelse(team_details$Home_Prob>0.5,1,0) 

table(Predicted=team_details$Result_of_prob, Actual=team_details$Result)
