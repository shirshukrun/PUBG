setwd("C:/R/PUBG")
pubg.raw<-read.csv('PUBG.csv')
str(pubg.raw)
summary(pubg.raw)

#create data to work on
pubg <- pubg.raw

#function to determine players level (devided by quartile statistics analysed in excel)
playerLevel <- function(x){
  if(x<2) "beginner"
  else if(x>5.56) "expert"
  else "intermediate"
}

pubg$Level <- sapply(pubg$solo_WinRatio, playerLevel)
str(pubg)

#check if there are NA's in the data
table(is.na(pubg))


beginner.filter <- pubg$Level == 'beginner'
intermediate.filter <- pubg$Level == 'intermediate'
expert.filter <- pubg$Level == 'expert'

table(beginner.filter)
table(intermediate.filter)
table(expert.filter)

?unique
unique(pubg$tracker_id, incomparables = FALSE)
unique(pubg$player_name, incomparables = FALSE)


pubg.prepared <- pubg
pubg.prepared$player_name <- NULL
pubg.prepared$tracker_id <- as.factor(pubg.prepared$tracker_id)
#pubg.prepared$solo_WinRatio <- NULL

str(pubg.prepared)

library(ggplot2)
#distribution of target column
ggplot(pubg, aes(Level)) + geom_bar()

#check features
#we will mark the influence between 1-5

#does 'solo_KillDeathRatio' affect Level?
ggplot(pubg, aes(Level ,solo_KillDeathRatio)) + geom_boxplot()#1

#does 'solo_TimeSurvived' affect Level?
ggplot(pubg, aes(Level ,solo_TimeSurvived)) + geom_boxplot()#2

#does 'solo_RoundsPlayed' affect Level?
ggplot(pubg, aes(Level ,solo_RoundsPlayed)) + geom_boxplot()#2

#does 'solo_Wins' affect Level?
ggplot(pubg, aes(Level ,solo_Wins)) + geom_boxplot()#1

#does 'solo_WinTop10Ratio' affect Level?
ggplot(pubg, aes(Level ,solo_WinTop10Ratio)) + geom_boxplot()#4

#does 'solo_Losses' affect Level?
ggplot(pubg, aes(Level ,solo_Losses)) + geom_boxplot()#1

#does 'solo_Rating' affect Level?
ggplot(pubg, aes(Level ,solo_Rating)) + geom_boxplot()#3

#does 'solo_Kills' affect Level?
ggplot(pubg, aes(Level ,solo_Kills)) + geom_boxplot()#1
pubg.prepared$solo_Kills <- NULL

#does 'solo_HeadshotKillRatio' affect Level?
ggplot(pubg, aes(Level ,solo_HeadshotKillRatio)) + geom_boxplot()#2

#does 'solo_WeeklyKills' affect Level?
ggplot(pubg, aes(Level ,solo_WeeklyKills)) + geom_boxplot()#1
pubg.prepared$solo_WeeklyKills <- NULL

#does 'solo_WeeklyKills' affect Level?
ggplot(pubg, aes(Level ,solo_WeeklyKills)) + geom_boxplot()#1

#see ralation between TimeSurvived|Kills|WinTop10Ratio
ggplot(data = pubg.prepared) + 
  geom_point(mapping = aes(x = solo_TimeSurvived, y = solo_WinTop10Ratio, color = solo_Kills))


#changing column names 
install.packages("tidyverse")
library(tidyverse)

#Show correlations 
#install.packages("corrplot")
library(corrplot)

#check corr
pubg.corr <- pubg.prepared
pubg.corr$Level <- NULL
pubg.corr$er_id <- NULL

names(pubg.corr) <- substring(names(pubg.corr), 6)
cor(pubg.corr)
mat <- cor(pubg.corr)
corrplot(mat, type = "upper" ,tl.pos = "td", 
         method = 'circle', tl.cex = 0.5, tl.col = 'black',
         order = "hclust", diag = FALSE)


#remove columns with high correlation
cor(pubg.corr$RoundsPlayed, pubg.corr$Losses)
pubg.corr$Losses <- NULL
cor(pubg.corr$Kills, pubg.corr$DamageDealt)
pubg.corr$DamageDealt <- NULL


#check the relation between the target column to other columns
plot(pubg.prepared$Level,pubg.prepared$solo_TimeSurvived)

table(pubg.corr$TimeSurvived)
hist(pubg.corr$KillDeathRatio)
hist(pubg.corr$WinRatio)#########################
hist(pubg.corr$TimeSurvived)
hist(pubg.corr$RoundsPlayed)
hist(pubg.corr$Wins)
hist(pubg.corr$WinTop10Ratio)
hist(pubg.corr$Top10Ratio)
hist(pubg.corr$Losses)
hist(pubg.corr$Rating)
hist(pubg.corr$Kills)
hist(pubg.corr$HeadshotKillRatio)
hist(pubg.corr$WeeklyKills)
hist(pubg.corr$RoundMostKills)
hist(pubg.corr$MaxKillStreaks)
hist(pubg.corr$AvgSurvivalTime)
hist(pubg.corr$WinPoints)
hist(pubg.corr$Heals)












