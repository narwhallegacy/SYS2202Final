library(dplyr)
library(ggplot2)
library(leaflet)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)
Sports_Crime = read.csv("crime_data_categorized.csv", header = TRUE)
Total_Crime = read.csv("Crime_Data.csv", header = TRUE)


# Cleeaning and grouping the data ----
Total_Crime <- separate(Total_Crime, DateReported, c("YEAR","MONTH","DAY"), sep="-", remove = FALSE)
Total_Crime <- separate(Total_Crime, DateReported, c("DAY","etc."), sep="T", remove = FALSE)
TOTAL_COUNTS <- Total_Crime %>% group_by(DAY) %>% tally()
TOTAL_COUNTS <- separate(TOTAL_COUNTS, DAY, c("YEAR", "MONTH", "NEW_DAY"), sep="-", remove = FALSE)
names(TOTAL_COUNTS)[names(TOTAL_COUNTS) == "DAY"] <- "stringDate"
names(TOTAL_COUNTS)[names(TOTAL_COUNTS) == "NEW_DAY"] <- "DAY"
SPORTS_COUNTS <- Sports_Crime %>% group_by(event_date) %>% tally()
SPORTS_COUNTS <- separate(SPORTS_COUNTS, event_date, c("YEAR", "MONTH", "DAY"), sep="-", remove = FALSE)
TOTAL_COUNTS$Date <- with(TOTAL_COUNTS, paste0(YEAR, MONTH, DAY))
SPORTS_COUNTS$Date <- with(SPORTS_COUNTS, paste0(YEAR, MONTH, DAY))
FINAL_DF <- TOTAL_COUNTS
FINAL_DF$Game.Day <- ifelse(FINAL_DF$Date %in% SPORTS_COUNTS$Date, TRUE, FALSE)

# Plotting ----
TestFilter = (FINAL_DF$YEAR == '2020') & (FINAL_DF$MONTH == '02')
filteredDf = FINAL_DF[TestFilter,]
ggplot(data = filteredDf, aes(x=DAY, y = n, fill = Game.Day)) + geom_bar(stat="identity")




# server = function(input, output) {
#   
#   # Fill in the spot we created for a plot
#   output$phonePlot <- renderPlot({
#     
#     # Render a barplot
#     barplot(WorldPhones[,input$region]*1000, 
#             main=input$region,
#             ylab="Number of Telephones",
#             xlab="Year")
#   })
# }
# 
# ui = fluidPage(    
#   titlePanel("Telephones by region"),
#   sidebarLayout(      
#     sidebarPanel(
#       selectInput("region", "Region:", 
#                   choices=colnames(WorldPhones)),
#       hr(),
#       helpText("Data from AT&T (1961) The World's Telephones.")
#     ),
#     mainPanel(
#       plotOutput("phonePlot")  
#     )
#     
#   )
# )
# 
# shinyApp(ui = ui, server = server)