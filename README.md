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

## Deconding multiple kills (explanation of multiple kills are above)
## ( DONT FORGET   TO EXPLAIN WHAT MULTIPLE KILLS ARE)
players$triple.kills<-sapply(players$triple.quadra.penta.kills, function(x) as.numeric(gsub(" ","",strsplit(x,'/')[[1]][1])))
players$quadra.kills<-sapply(players$triple.quadra.penta.kills, function(x) as.numeric(gsub(" ","",strsplit(x,'/')[[1]][2])))
players$penta.kills<-sapply(players$triple.quadra.penta.kills, function(x) as.numeric(gsub(" ","",strsplit(x,'/')[[1]][3])))


#convert creep.kill(CS) to numeric and remove old column
players$Creep.Kill<-sapply(players$Creep.Kill, function(x) as.numeric(gsub(",","",x)))
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

#players_summary<- normalizeCols(players_summary)
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
> 

