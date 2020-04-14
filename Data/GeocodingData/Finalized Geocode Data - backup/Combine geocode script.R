#Combine all geocoded data into 1 large geocode file

#read in .csv files
location1 = read.csv("geocoded1-2500.csv", header=TRUE)
location2501 = read.csv("geocoded2501-5000.csv", header=TRUE)
location5001 = read.csv("geocoded5001-7500.csv", header=TRUE)
location7501 = read.csv("geocoded7501-10000.csv", header=TRUE)
location10001 = read.csv("geocoded10001-12500.csv", header=TRUE)
location12501 = read.csv("geocoded12501-15000.csv", header=TRUE)
location15001 = read.csv("geocoded15001-17500.csv", header=TRUE)
location17501 = read.csv("geocoded17501-20000.csv", header=TRUE)
location20001 = read.csv("geocoded20001-22500.csv", header=TRUE)
location22501 = read.csv("geocoded22501-25000.csv", header=TRUE)
location25001 = read.csv("geocoded25001_27500.csv", header=TRUE)
location27501 = read.csv("geocoded27501-end.csv", header=TRUE)

#ensure names are the same
names(location2501) = names(location1)
names(location5001) = names(location1)
names(location7501) = names(location1)
names(location10001) = names(location1)
names(location12501) = names(location1)
names(location15001) = names(location1)
names(location17501) = names(location1)
names(location20001) = names(location1)
names(location22501) = names(location1)
names(location25001) = names(location1)
names(location27501) = names(location1)

#combine dataframes into one large dataframe and export as .csv
geocoded_location = rbind(location1,location2501,location5001,location7501,location10001,location12501,location15001,location17501,location20001,location22501,location25001,location27501)
write.csv(geocoded_location,"C:/Users/Lee/Documents/GitHub/SYS2202Final/Data/GeocodedFull.csv")

#export just coordinates as .csv
geocoded_reduced = data.frame(geocoded_location[2],geocoded_location[3])
write.csv(geocoded_reduced,"C:/Users/Lee/Documents/GitHub/SYS2202Final/Data/GeocodedCoordinates.csv")

