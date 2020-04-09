library("TTR")
library("smoother")
library("ggplot2")
library("gcookbook")
library("tidyverse")
library("cluster")
library("MASS")

#-------Read in data-------------------------------------------------

arrests = read.csv("Arrests.csv", header=TRUE)
Football = read.csv("College_Football_2000_2018.csv", header=TRUE)
dogtags = read.csv("Dog_Tags.csv", header=TRUE)
greenTransport = read.csv("Green_Infrastructure_Transportation.csv", header=TRUE)
RoadCenterlines = read.csv("Road_Centerlines.csv", header=TRUE)
#transit2017 = read.csv("transit_2017.csv", header=TRUE)
#transit2018 = read.csv("transit_2018.csv", header=TRUE)
#trannsit2019 = read.csv("transit_2019.csv", header=TRUE)
#transit2020 = read.csv("transit_2020.csv", header=TRUE)
unemploymentYearly = read.csv("Unemployment.csv", header=TRUE)
unemploymentMonthly = read.csv("Unemployment_monthly.csv", header=TRUE)
uvaBasketball = read.csv("UVA2019_2020BasketballSeason.csv", header=TRUE)



#----Reduce columns of Basketball data-----------------

Away = (uvaBasketball$X == '@')
Won = ((uvaBasketball$W.L == 'W') | (uvaBasketball $W.L == 'W (1 OT)'))

BasketballReduced = data.frame(uvaBasketball$Date, uvaBasketball$Opp, Won, Away)
names(BasketballReduced) = c("Date", "Opponent", "Won", "Away")

charDate = as.character(BasketballReduced$Date)
BasketballReduced$Date = as.POSIXct(charDate, format= "%m/%d/%Y")


write.csv(BasketballReduced,"C:/Users/Lee/Documents/GitHub/SYS2202Final/CleanedData/Basketball.csv")



#----Reduce football dataset to just UVa-----------------

uvaHome = ((Football$Team == "Virginia") | (Football$Team == "Virginia*"))
uvaAway = ((Football$Opponent == "Virginia") | (Football$Team == "Virginia*") | (Football$Team == "No. 21 Virginia") | (Football$Team == "No. 25 Virginia*") | (Football$Team == "No. 25 Virginia"))



uvaHomeGames = Football[uvaHome,]
uvaAwayGames = Football[uvaAway,]

homeOpp = uvaHomeGames$Opponent
AwayOpp = uvaAwayGames$Team

homeWin = (substr(uvaHomeGames$Result,1,1) == 'W')
awayWin = (substr(uvaAwayGames$Result,1,1) == 'L')

Away = c(TRUE)
Home = c(FALSE)

uvaHomeReduced = data.frame(uvaHomeGames$Date, homeOpp, homeWin, Home)
uvaAwayReduced = data.frame(uvaAwayGames$Date, AwayOpp, awayWin, Away)

names(uvaHomeReduced) = c("Date", "Opponent", "Won", "Away")
names(uvaAwayReduced) = c("Date", "Opponent", "Won", "Away")

footballReduced = rbind(uvaHomeReduced,uvaAwayReduced)

charDate = as.character(footballReduced$Date)
footballReduced$Date = as.POSIXct(charDate, format= "%m/%d/%Y")

write.csv(footballReduced,"C:/Users/Lee/Documents/GitHub/SYS2202Final/CleanedData/Football.csv")
