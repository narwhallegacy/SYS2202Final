#Cleaning Crime Data and creating integrated data set

#Import Libraries
library(tidyverse)
library(ggplot2)
library(readxl)
library(dplyr)
library(rgeos)
library(gcookbook)
library(maps)
#Read in csv files and save as variables
df<-read.csv('C:\\Users\\Ryana\\Downloads\\Crime_Data.csv')
sp<-read.csv('C:\\Users\\Ryana\\Downloads\\uvaSports.csv')
lat<-read.csv('C:\\Users\\Ryana\\Downloads\\GeocodedCoordinates.csv')
#Convert factors to strings, easier to work with  
df <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
sp <- data.frame(lapply(sp, as.character), stringsAsFactors=FALSE)
lat <- data.frame(lapply(lat, as.character), stringsAsFactors=FALSE)
#Delete "T" in middle and ending '.000Z' with an empty character string to make datetime more useable
s <-gsub( "T", " ", df$DateReported)
s <-gsub(".000Z","",s)
#Delete hour from datetime to match sports dates
s<-gsub(" .*","",df$DateReported)
df$DateReported<- s
#Rename columns to match between data sets
names(df)[names(df) == "DateReported"] <- "Date"
names(df)[names(df) == "ï..RecordID"] <- "RecordID"
names(lat)[names(lat) == "X"] <- "RecordID"

#Count NA values in each column
sapply(df, function(x) sum(is.na(x)))
#Add longitude and latitude data to crime data frame
df_lat <- left_join(df, lat, by = 'RecordID')
#Create a new data frame using only relevant columns
df_lat <- data.frame(df_lat$Offense, df_lat$Date, df_lat$HourReported, df_lat$Longitude, df_lat$Latitude)
names(df_lat)[names(df_lat) == "df_lat.Date"] <- "Date"
x <-gsub( "T", " ", df_lat$Date)
x <-gsub(".000Z","",x)
#Delete hour from datetime to match sports dates

df_lat$Date<- x
x<-gsub(" .*","",df_lat$Date)
df_lat$Date<- x

#Join crime and sports data frames
newnew <- left_join(df_lat,sp, by = 'Date')
sapply(newnew, function(x) sum(is.na(x)))
#Keep only crime data that occurred on a game day
newtotal <- newnew[which(is.na(newnew$Won) ==F), ]
newtotal_copy = newtotal
#Regular expressions to clean opponent data to remove numbers, asteriks, leading white space 
z <- gsub("^.*\\.","",newtotal$Opponent)
z <- gsub('[*]+','', z)
z <- gsub('[0-9]+','',z)
z <- gsub("'\(FCS)'",'',z)
z <- gsub("^.*\\()","",z)
trim.leading <- function (x)  sub("^\\s+", "", x)
newtotal_copy$Opponent <- trim.leading(z)

#Remove redundant/unneccessary columns
newtotal_copy$X<-NULL
newtotal_copy$RecordID<-NULL  
newtotal_copy$IncidentID<-NULL
newtotal_copy$Agency<-NULL
#Rename columns to make more legible
names(newtotal_copy)[names(newtotal_copy) == "df_lat.Offense"] <- "Offense"
names(newtotal_copy)[names(newtotal_copy) == "df_lat.HourReported"] <- "HourReported"
names(newtotal_copy)[names(newtotal_copy) == "df_lat.Latitude"] <- "Latitude"
names(newtotal_copy)[names(newtotal_copy) == "df_lat.Longitude"] <- "Longitude"
#Get list of offenses to sort into broader categories 
unique(newtotal_copy$Offense)
final_df = newtotal_copy
#Change column names for integrated data frame 
names(final_df)[names(final_df) == "Date"] <- "event_date"
names(final_df)[names(final_df) == "Sports_Type"] <- "sport"
names(final_df)[names(final_df) == "Won"] <- "win"
names(final_df)[names(final_df) == "Offense"] <- "crime_type"
names(final_df)[names(final_df) == "Away"] <- "home"

final_df<-final_df[,c(4,5,2,9,6,8,7,1,3)]
final_df$HourReported<-NULL

#Write date frames into csv files
write.csv(df, file='C:\\Users\\Ryana\\Downloads\\clean_crime_final.csv')
write.csv(final_df, file='C:\\Users\\Ryana\\Downloads\\Sports_Crime.csv')

