setwd("C:/R/PUBG")
pubg.raw<-read.csv('PUBG.csv')
str(pubg.raw)
summary(pubg.raw)

#create data to work on
pubg <- pubg.raw

#function to determine players level (devided by quartile statistics analysed in excel)
playerLevel <- function(x){
  if(x<2) "beginner"
  else if(x>2) "intermediate"
  else "expert"
}

pubg$Level <- sapply(pubg$solo_WinRatio, playerLevel)
str(pubg)

