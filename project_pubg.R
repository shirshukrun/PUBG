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

#add target column
pubg$Level <- sapply(pubg$solo_WinRatio, playerLevel)
str(pubg)

pubg.prepared <- pubg

#delete the column to avoid oversampling
pubg.prepared$solo_WinRatio <- NULL

#check if there are NA's 
table(is.na(pubg.prepared))

#inspect the target column distribution
beginner.filter <- pubg$Level == 'beginner'
table(beginner.filter)

intermediate.filter <- pubg$Level == 'intermediate'
table(intermediate.filter)

expert.filter <- pubg$Level == 'expert'
table(expert.filter)

library(ggplot2)

#see distribution of the target column
ggplot(pubg, aes(Level)) + geom_bar()

#check if the column is distinct
unique(pubg$player_name, incomparables = FALSE)

pubg.prepared$player_name <- NULL
pubg.prepared$tracker_id <- as.factor(pubg.prepared$tracker_id)
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
#not deleted for further analysis

#does 'solo_HeadshotKillRatio' affect Level?
ggplot(pubg, aes(Level ,solo_HeadshotKillRatio)) + geom_boxplot()#2

#does 'solo_WeeklyKills' affect Level?
ggplot(pubg, aes(Level ,solo_WeeklyKills)) + geom_boxplot()#1
pubg.prepared$solo_WeeklyKills <- NULL

#see ralation between TimeSurvived|Kills|WinTop10Ratio
ggplot(data = pubg.prepared) + 
  geom_point(mapping = aes(x = solo_TimeSurvived, y = solo_WinTop10Ratio, color = solo_Kills))

str(pubg.prepared)

#Show correlations 
#install.packages("corrplot")
library(corrplot)

#check correlation between features

pubg.corr <- pubg.prepared
pubg.corr$Level <- NULL
pubg.corr$tracker_id <- NULL

#install.packages("tidyverse")
library(tidyverse) #(tibble|dplyr|reader|purrr)

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

str(pubg.prepared)
#check if bining needed 
hist(pubg.prepared$solo_TimeSurvived)
hist(pubg.prepared$solo_RoundsPlayed)
hist(pubg.prepared$solo_WinTop10Ratio)
hist(pubg.prepared$solo_Top10Ratio)#####
hist(pubg.prepared$solo_Losses)
hist(pubg.prepared$solo_Rating)
hist(pubg.prepared$solo_Kills)
hist(pubg.prepared$solo_HeadshotKillRatio)
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
##################################################################################

pubg.glm <- pubg.prepared
#only leavnig the wanted columns




#devide to train and test
library(caTools)
filter <- sample.split(pubg.prepared$solo_TimeSurvived, SplitRatio = 0.7)
pubg.prepared.train <- subset(pubg.prepared,filter==T)
pubg.prepared.test <- subset(pubg.prepared,filter==F)

dim(pubg.prepared.train)
dim(pubg.prepared.test)

loan.model <- glm(Level ~ ., family = binomial(link = "logit"), data = pubg.prepared.train)
str(pubg.prepared)

