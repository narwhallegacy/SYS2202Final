#read in data
fullData = read.csv("crime_data_categorized.csv", header = TRUE)


#------Sport Filter----------
#Basketball
sportFilter = (fullData$sport == 'Basketball')
#Football
sportFilter = (fullData$sport == 'Football')
#No filter
sportFilter = (fullData$sport == fullData$sport)


#-----Won Filter------------
#Win
winFilter = (fullData$win == TRUE)
#Lose
winFilter = (fullData$win == FALSE)
#No Filter
winFilter = (fullData$win == fullData$win)

#-----home Game Filter ------------
#Home
homeFilter = (fullData$win == TRUE)
#loss
homeFilter = (fullData$win == FALSE)
#no filter
homeFilter = (fullData$win == fullData$win)


#------Opponent Filter-------------
sampleSelection = c('Virginia Tech', 'Duke', 'UNC')

oppFilter = fullData$Opponent %in% sampleSelection


#------Offense Filter-------------
sampleOffense = c('Theft', 'Non-criminal Report')

crimeFilter = fullData$Crime.Category %in% sampleOffense


#----------Date Filter (UNFINISHED)------------
sampleStartDate = as.Date(2018-01-01)
sampleEndDate = 2019-01-01



#Combining Filters
fullFilter = crimeFilter & oppFilter & sportFilter & wonFilter

filterData = fullData[fullFilter,]
