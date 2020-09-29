setwd("C:/R/PUBG")
pubg.raw<-read.csv('PUBG.csv')
str(pubg.raw)
summary(pubg.raw)
pubg.raw$X <- NULL
pubg.raw$X0 <- NULL

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
#we will mark the influence between 1-4

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

#does 'solo_Top10Ratio' affect Level?
ggplot(pubg, aes(Level ,solo_Top10Ratio)) + geom_boxplot()#3

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

#does 'solo_RoundMostKills' affect Level?
ggplot(pubg, aes(Level ,solo_RoundMostKills)) + geom_boxplot()#2

#does 'solo_MaxKillStreaks' affect Level?
ggplot(pubg, aes(Level ,solo_MaxKillStreaks)) + geom_boxplot()#1
pubg.prepared$solo_MaxKillStreaks <- NULL

#does 'solo_MaxKillStreaks' affect Level?
ggplot(pubg, aes(Level ,solo_AvgSurvivalTime)) + geom_boxplot()#3

#does 'solo_WinPoints' affect Level?
ggplot(pubg, aes(Level ,solo_WinPoints)) + geom_boxplot()#3

#does 'solo_Heals' affect Level?
ggplot(pubg, aes(Level ,solo_Heals)) + geom_boxplot()#1
pubg.prepared$solo_Heals <- NULL

#does 'solo_DamageDealt' affect Level?
ggplot(pubg, aes(Level ,solo_DamageDealt)) + geom_boxplot()#1
pubg.prepared$solo_DamageDealt <- NULL

#see ralation between TimeSurvived|Kills|WinTop10Ratio
ggplot(data = pubg.prepared) + 
  geom_point(mapping = aes(x = solo_TimeSurvived, 
                           y = solo_WinTop10Ratio, 
                           color = solo_Kills))

#correlations 
#install.packages("corrplot")
library(corrplot)
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
cor(pubg.prepared$solo_RoundsPlayed, pubg.prepared$solo_TimeSurvived)
cor(pubg.prepared$solo_TimeSurvived, pubg.prepared$solo_Losses)
pubg.prepared$solo_RoundsPlayed <- NULL
pubg.prepared$solo_TimeSurvived <- NULL

str(pubg.prepared)
#check if bining needed 
hist(pubg.prepared$solo_WinTop10Ratio)
hist(pubg.prepared$solo_Top10Ratio)#####
hist(pubg.prepared$solo_Losses)
hist(pubg.prepared$solo_Rating)
hist(pubg.prepared$solo_Kills)
hist(pubg.prepared$solo_HeadshotKillRatio)
hist(pubg.prepared$solo_RoundMostKills)
hist(pubg.prepared$solo_AvgSurvivalTime)
hist(pubg.prepared$solo_WinPoints)


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

pubg.prepared$Level <- as.factor(pubg.prepared$Level)

pubg.prepared$tracker_id <- NULL
str(pubg.prepared)
cor(pubg.prepared$solo_Losses, pubg.prepared$solo_Kills)
pubg.prepared$solo_Losses <- NULL

#devide to train and test
library(caTools)
filter <- sample.split(pubg.prepared$solo_WinTop10Ratio, SplitRatio = 0.7)
pubg.prepared.train <- subset(pubg.prepared,filter==T)
pubg.prepared.test <- subset(pubg.prepared,filter==F)

dim(pubg.prepared.train)
dim(pubg.prepared.test)

########################   glm  generalized linear model  ####################
pubg.model <- glm(Level ~ ., family = binomial(link = 'logit'), data = pubg.prepared.train)
#prediction 
predict.test <- predict(pubg.model, newdata = pubg.prepared.test, type = 'response')
confusion.matrix <- table(predict.test>0.5, pubg.prepared.test$Level)

precision <- confusion.matrix[2,2] / (confusion.matrix[2,1]+confusion.matrix[2,2])
recall <- confusion.matrix[2,2] / (confusion.matrix[1,2]+confusion.matrix[2,2])

#############################################################################
treeModel <- rpart(Level~.,pubg.prepared.train) 
rpart.plot(treeModel, box.palette = "RdBu", shadow.col = "grey", nn=TRUE)

predict.prob <- predict(treeModel,pubg.prepared.test)
predict.prob.beginner <- predict.prob[,'beginner']
predict.prob.intermediate <- predict.prob[,'intermediate']
predict.prob.expert <- predict.prob[,'expert']

predicitonB <- predict.prob.beginner > 0.5
predicitonI <- predict.prob.intermediate > 0.5

actual <- bankrupt.test$class
cf <- table(prediciton,actual)
summary(bankrupt)
precision <- cf['TRUE','1']/(cf['TRUE','1'] + cf['TRUE','0'])
recall <- cf['TRUE','1']/(cf['TRUE','1'] + cf['FALSE','1'])
#recall 0.6015 percition-0.7475





########################   decision tree   ####################

#install.packages('rpart')
library(rpart)
#install.packages('rpart.plot')
library(rpart.plot)
tree <- rpart(Level ~ ., pubg.prepared.train)

prp(tree)

predictedT <- predict(tree,pubg.prepared.test)
predprob_tree<-predict(tree, pubg.prepared.test, type = 'prob')

#install.packages('pROC')
library(pROC)

rocCurve_tree<-roc(pubg.prepared.test$Level, predprob_tree[,'expert'], levels = c("expert","beginner"))
plot(rocCurve_tree,col='yellow', main = "Roc Chart")
auc(rocCurve_tree)

rocCurve_tree<-roc(pubg.prepared.test$Level, predprob_tree[,'expert'], levels = c("expert","intermediate"))
plot(rocCurve_tree,col='yellow', main = "Roc Chart")
auc(rocCurve_tree)

rocCurve_tree<-roc(pubg.prepared.test$Level, predprob_tree[,'intermediate'], levels = c("beginner","intermediate"))
plot(rocCurve_tree,col='yellow', main = "Roc Chart")
auc(rocCurve_tree)

########################   Random forest   ####################
#install.packages('randomForest')
library(randomForest)
rf.model<-randomForest(pubg.prepared.test$Level~. , data = pubg.prepared.test)
print(rf.model)
predicted<-predict(rf.model, pubg.prepared.test)
predictprob<-predict(rf.model, pubg.prepared.test, type = 'prob')
ROC<-roc(pubg.prepared.test$Level, predictprob[,'expert'],levels = c("expert","beginner") )
plot(ROC,col='yellow', main = "Roc Chart")
auc(ROC) #1

ROC<-roc(pubg.prepared.test$Level, predictprob[,'expert'],levels = c("expert","intermediate") )
plot(ROC,col='yellow', main = "Roc Chart")
auc(ROC) #1

ROC<-roc(pubg.prepared.test$Level, predictprob[,'intermediate'],levels = c("beginner","intermediate") )
plot(ROC,col='yellow', main = "Roc Chart")
auc(ROC) #1

########################################################################################

# rocCurveGLM <- roc(pubg.prepared.test$Level, predict.test, direction = "<", levels = c("expert","beginner"))
# rocCurveDT <- roc(pubg.prepared.test$Level, predictedT, direction = "<", levels = c("expert","intermediate",))
# rocCurveRF <- roc(pubg.prepared.test$Level, predicted, direction = "<", levels = c("beginner","intermediate"))
# 
# plot(rocCurveNB, col ='red', main = 'ROC chart')
# par(new=TRUE)
# 
# plot(rocCurveTR, col ='blue', main = 'ROC chart')
# 
# auc(rocCurveNB)
# auc(rocCurveTR)

