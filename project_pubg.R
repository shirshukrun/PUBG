setwd("C:/R/PUBG")
pubg.raw<-read.csv('PUBG.csv')
str(pubg.raw)
summary(pubg.raw)

#create data to work on
pubg <- pubg.raw

#target column created according to column "solo_WinRatio" 
playerLevel <- function(x){
  if(x<2) "beginner"
  else if(x>5.56) "expert"
  else "intermediate"
}
pubg.prepared$solo_WinRatio <- NULL

#add target column
pubg$Level <- sapply(pubg$solo_WinRatio, playerLevel)
str(pubg)

#check if there are NA's in the data
table(is.na(pubg))

#inspect the target column distribution
beginner.filter <- pubg$Level == 'beginner'
intermediate.filter <- pubg$Level == 'intermediate'
expert.filter <- pubg$Level == 'expert'

table(beginner.filter)
table(intermediate.filter)
table(expert.filter)

library(ggplot2)
ggplot(pubg, aes(Level)) + geom_bar()

pubg.prepared <- pubg

unique(pubg$player_name, incomparables = FALSE)

pubg.prepared$player_name <- NULL
pubg.prepared$tracker_id <- as.factor(pubg.prepared$tracker_id)
#pubg.prepared$solo_WinRatio <- NULL

str(pubg.prepared)

#check features
#we will mark the influence between 1-5

#does 'solo_KillDeathRatio' affect Level?
ggplot(pubg, aes(Level ,solo_KillDeathRatio)) + geom_boxplot()#1
pubg.prepared$solo_KillDeathRatio <- NULL

#does 'solo_TimeSurvived' affect Level?
ggplot(pubg, aes(Level ,solo_TimeSurvived)) + geom_boxplot()#2

#does 'solo_RoundsPlayed' affect Level?
ggplot(pubg, aes(Level ,solo_RoundsPlayed)) + geom_boxplot()#2

#does 'solo_Wins' affect Level?
ggplot(pubg, aes(Level ,solo_Wins)) + geom_boxplot()#1
pubg.prepared$solo_Wins <- NULL

#does 'solo_WinTop10Ratio' affect Level?
ggplot(pubg, aes(Level ,solo_WinTop10Ratio)) + geom_boxplot()#4

#does 'solo_Losses' affect Level?
ggplot(pubg, aes(Level ,solo_Losses)) + geom_boxplot()#2

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

#see ralation between TimeSurvived|Kills|WinTop10Ratio
ggplot(data = pubg.prepared) + 
  geom_point(mapping = aes(x = solo_TimeSurvived, y = solo_WinTop10Ratio, color = solo_Kills))

str(pubg.prepared)
#changing column names 
#install.packages("tidyverse")
library(tidyverse)

#Show correlations 
#install.packages("corrplot")
library(corrplot)

#check correlation between columns with database that only contains numbers
pubg.corr <- pubg.prepared
pubg.corr$Level <- NULL
pubg.corr$tracker_id <- NULL

#shorten the names of the columns (removing "solo_"...)
names(pubg.corr) <- substring(names(pubg.corr), 6)
cor(pubg.corr)
mat <- cor(pubg.corr)

corrplot(mat, method = 'circle', outline = T, tl.cex = 0.5,
         tl.col = 'black', order = "hclust", diag = TRUE)

#remove columns with high correlation
cor(pubg.prepared$solo_RoundsPlayed, pubg.prepared$solo_Losses)
pubg.prepared$Losses <- NULL
cor(pubg.prepared$solo_Kills, pubg.prepared$solo_DamageDealt)
pubg.prepared$DamageDealt <- NULL

#check if bining needed 
hist(pubg.prepared$solo_TimeSurvived)
hist(pubg.prepared$solo_KillDeathRatio)
hist(pubg.prepared$solo_WinRatio)####
hist(pubg.prepared$solo_TimeSurvived)
hist(pubg.prepared$solo_RoundsPlayed)
hist(pubg.prepared$solo_Wins)
hist(pubg.prepared$solo_WinTop10Ratio)
hist(pubg.prepared$solo_Top10Ratio)#####
hist(pubg.prepared$solo_Losses)
hist(pubg.prepared$solo_Rating)
hist(pubg.prepared$solo_Kills)
hist(pubg.prepared$solo_HeadshotKillRatio)
hist(pubg.prepared$solo_WeeklyKills)
hist(pubg.prepared$solo_RoundMostKills)
hist(pubg.prepared$solo_MaxKillStreaks)
hist(pubg.prepared$solo_AvgSurvivalTime)
hist(pubg.prepared$solo_WinPoints)
hist(pubg.prepared$solo_Heals)


#coercing column "solo_Top10Ratio"
table(pubg.prepared$solo_Top10Ratio)
coercex <- function(x){
  ifelse(x>60,60,x)
}
pubg.prepared$solo_Top10Ratio <- sapply(pubg.prepared$solo_Top10Ratio, coercex)
hist(pubg.prepared$solo_Top10Ratio)

hist(pubg.prepared$solo_WinTop10Ratio)
coercey <- function(x){
  ifelse(x>0.7,0.7,x)
}
pubg.prepared$solo_WinTop10Ratio <- sapply(pubg.prepared$solo_WinTop10Ratio, coercey)
hist(pubg.prepared$solo_WinTop10Ratio)

#devide to train and test





