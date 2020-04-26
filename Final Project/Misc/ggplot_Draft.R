library(ggplot2)

TestFilter = (FINAL_DF$YEAR == '2020') & (FINAL_DF$MONTH == '02')

filteredDf = FINAL_DF[TestFilter,]

ggplot(data = filteredDf, aes(x=DAY, y = n, fill = COLOR)) + geom_bar(stat="identity")

       