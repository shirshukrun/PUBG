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

#check if there are NA's in winRtio
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


pubg$player_name <- NULL
pubg.prepared <- pubg
pubg.prepared$solo_WinRatio <- NULL
pubg.prepared$tracker_id <- NULL

str(pubg.prepared)
#distribution of target column
ggplot(pubg, aes(Level)) + geom_bar()

#check features
library(ggplot2)

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









