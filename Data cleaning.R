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

BasketballReduced1920 = data.frame(uvaBasketball$Date, uvaBasketball$Opp, Won, Away)
names(BasketballReduced1920) = c("Date", "Opponent", "Won", "Away")

charDate = as.character(BasketballReduced1920$Date)
BasketballReduced1920$Date = as.POSIXct(charDate, format= "%m/%d/%Y")


#write.csv(BasketballReduced,"C:/Users/Lee/Documents/GitHub/SYS2202Final/CleanedData/Basketball1920.csv")


#2018/19 Season
uvaBasketball = read.csv("UVA2018_2019BasketballSeason.csv", header=TRUE)

Away = (uvaBasketball$X == '@')
Won = ((uvaBasketball$W.L == 'W') | (uvaBasketball $W.L == 'W (1 OT)'))

BasketballReduced1819 = data.frame(uvaBasketball$Date, uvaBasketball$Opp, Won, Away)
names(BasketballReduced1819) = c("Date", "Opponent", "Won", "Away")

charDate = as.character(BasketballReduced1819$Date)
BasketballReduced1819$Date = as.POSIXct(charDate, format= "%m/%d/%Y")

#2017/18 Season
uvaBasketball = read.csv("UVA2017_2018BasketballSeason.csv", header=TRUE)

Away = (uvaBasketball$X == '@')
Won = ((uvaBasketball$W.L == 'W') | (uvaBasketball $W.L == 'W (1 OT)'))

BasketballReduced1718 = data.frame(uvaBasketball$Date, uvaBasketball$Opp, Won, Away)
names(BasketballReduced1718) = c("Date", "Opponent", "Won", "Away")

charDate = as.character(BasketballReduced1718$Date)
BasketballReduced1718$Date = as.POSIXct(charDate, format= "%m/%d/%Y")


#2016/17 Season
uvaBasketball = read.csv("UVA2016_2017BasketballSeason.csv", header=TRUE)

Away = (uvaBasketball$X == '@')
Won = ((uvaBasketball$W.L == 'W') | (uvaBasketball $W.L == 'W (1 OT)'))

BasketballReduced1617 = data.frame(uvaBasketball$Date, uvaBasketball$Opp, Won, Away)
names(BasketballReduced1617) = c("Date", "Opponent", "Won", "Away")

charDate = as.character(BasketballReduced1617$Date)
BasketballReduced1617$Date = as.POSIXct(charDate, format= "%m/%d/%Y")

#2015/16 Season
uvaBasketball = read.csv("UVA2015_2016BasketballSeason.csv", header=TRUE)

Away = (uvaBasketball$X == '@')
Won = ((uvaBasketball$W.L == 'W') | (uvaBasketball $W.L == 'W (1 OT)'))

BasketballReduced1516 = data.frame(uvaBasketball$Date, uvaBasketball$Opp, Won, Away)
names(BasketballReduced1516) = c("Date", "Opponent", "Won", "Away")

charDate = as.character(BasketballReduced1516$Date)
BasketballReduced1516$Date = as.POSIXct(charDate, format= "%m/%d/%Y")



#Combining Basketball into one large .csv file
BasketballReduced = rbind(BasketballReduced1516,BasketballReduced1617,BasketballReduced1718,BasketballReduced1819,BasketballReduced1920)


write.csv(BasketballReduced,"C:/Users/Lee/Documents/GitHub/SYS2202Final/CleanedData/Basketball1920.csv")




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




#-------combine sports data-------------------------

IsBasketball = TRUE

BasketballReduced = data.frame(BasketballReduced,IsBasketball)

IsBasketball = FALSE

footballReduced = data.frame(footballReduced,IsBasketball)

uvaSports = rbind(BasketballReduced, footballReduced)

write.csv(uvaSports,"C:/Users/Lee/Documents/GitHub/SYS2202Final/CleanedData/uvaSports.csv")
