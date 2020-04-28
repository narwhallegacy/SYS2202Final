# Ryan Ahmadiyar (ra7be), George Corbin (glc5pn), Steven Wasserman (sw4kh)
# SYS 2202, Spring 2020
# RShiny Deployment of Sports-Crime Data

# Calls to necessary libraries ----
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
Sports_Crime = read.csv("crime_data_categorized.csv", header = TRUE)
Total_Crime = read.csv("Crime_Data.csv", header = TRUE)

# Cleaning and grouping the data ----
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

# User-Interface Development ----
ui = fluidPage(
  navbarPage("Sports-Crime Data", theme = shinytheme("sandstone"),
             tabPanel("Charlottesville Map of Crime", fluid = TRUE, icon = icon("globe-americas"),
              # Tab 1: Map of Reported Incidents on Game Day
              titlePanel("Map of Reported Crimes in Charlottesville on Game Days"),
              sidebarLayout( sidebarPanel( 
                fluidRow(column(6, 
                                # Selection for home game or away game
                                radioButtons( inputId = "HomeFinder", label = "Home or Away? : ",
                                              choiceNames = c( "Home", "Away", "No Filter"),
                                              choiceValues = c(TRUE, FALSE, NA),
                                              selected = NA),
                                
                                # Selection for UVA win or UVA lose
                                radioButtons( inputId = "WinFinder", label = "Game Result : ",
                                              choiceNames = c( "UVA Win", "UVA Lose", "No Filter"),
                                              choiceValues = c(TRUE,FALSE, NA),
                                              selected = NA),
                                
                                # Selection for opponent team
                                selectInput(inputId = "OpponentFinder", label = "Select Opponent School", 
                                            choices = c('N/A',as.character(Sports_Crime$Opponent)), width = "220px")
                                
                ),
                column(6, offset = 0, 
                        # Selection for crime type (concat. into more manageable groups from original data)
                         checkboxGroupInput(inputId = "CrimeFinder",
                                            label = "Select Type of Crime",
                                            choices = unique(Sports_Crime$Crime.Category), width = "220px",
                                            selected = unique(Sports_Crime$Crime.Category))
                )),
                # Selection for incident-report date range
                dateRangeInput(inputId="dateFinder", label = 'Date Range',
                               start = "2015-09-12", end = "2020-03-07",
                               min = "2015-09-12", max = "2020-03-07", format = "yyyy-mm-dd",
                               startview = "year",weekstart = 0, language = "en",
                               separator = ' to ', width = '400px')
                ),
                mainPanel( leafletOutput("mapPlot") ) ) 
          ),
          # Tab 2: Comparison of Reported Crimes to Monthly Average 
          tabPanel("Monthly Average Comparison", fluid = TRUE, icon = icon("calendar"), 
                   # Tab 1: Map of Reported Incidents on Game Day
                   titlePanel("Map of Reported Crimes in Charlottesville on Game Days"),
                   sidebarLayout( sidebarPanel( 
                     fluidRow(column(5, 
                                     # Selection for month
                                     selectInput(inputId = "monthLocator", label = "Select Month", 
                                                 choices = c(sort(as.character(FINAL_DF$MONTH))), width = "220px")
                     ),
                     column(5, offset = 0, 
                            # Selection for month
                            selectInput(inputId = "yearLocator", label = "Select Year", 
                                        choices = c(sort(as.character(FINAL_DF$YEAR))), width = "220px")
                     ))
                   ),
                   mainPanel( plotOutput("barPlot"),
                              textOutput("meanValues")) ) 
          )        
    ))

# Server Session Development ----
server = function(input, output) ({
  output$mapPlot <- renderLeaflet({
    # 'filters' data
    #Filters based on wins
    if (is.na(as.logical(input$WinFinder))) { winFilter = TRUE } 
    else { winFilter = Sports_Crime$win == as.logical(input$WinFinder) }
    #Filters based on home game/away game
    if (is.na(as.logical(input$HomeFinder))) { homeFilter = TRUE } 
    else { homeFilter = Sports_Crime$home == as.logical(input$HomeFinder) }
    #Filters based on Date Range
    dateFilter = (as.Date(Sports_Crime$event_date) >= input$dateFinder[1]) & (as.Date(Sports_Crime$event_date) <= input$dateFinder[2])
    #Filters based on Opponent
    if (input$OpponentFinder == 'N/A') { oppFilter = TRUE } 
    else { oppFilter = Sports_Crime$Opponent == input$OpponentFinder }
    #Filters based on Crime Type
    crimeFilter = Sports_Crime$Crime.Category %in% input$CrimeFinder
    #Combines all above filters
    CombinedFilter = winFilter & homeFilter & oppFilter & crimeFilter & dateFilter
    filteredData = Sports_Crime[CombinedFilter,]
    isolate({
      # Render the map plot
      leaflet(data = filteredData) %>% addTiles() %>%
        addMarkers(~Longitude,
                   ~Latitude,
                   # Creates a map plot of the data points
                   #label = ("Date of Incident: " + Date + "\nCrime Type: " + Offense))
                   label = paste("Crime Type: ", filteredData$crime_type, ' | Date: ',filteredData$event_date,
                                 " | Sport: ",filteredData$sport, sep = ""))
      }) 
    })
  output$barPlot <- renderPlot({
    
    # Plotting ----
    TestFilter = (FINAL_DF$YEAR == input$yearLocator) & (FINAL_DF$MONTH == input$monthLocator)
    # TestFilter = (FINAL_DF$YEAR == '2020') & (FINAL_DF$MONTH == '02')
    filteredDf = FINAL_DF[TestFilter,]
    isolate ({ 
      ggplot(data = filteredDf, 
             aes(x=DAY,  y = n, fill = Game.Day)) + geom_bar(stat="identity")
        })
  })
  output$meanValues <- renderText({
    TestFilter2 = (FINAL_DF$YEAR == input$yearLocator) & (FINAL_DF$MONTH == input$monthLocator)
    filteredDf2 = FINAL_DF[TestFilter2,]
    
    
    paste('Mean for non-game days:',as.character(round(mean(filteredDf2$n[filteredDf2$Game.Day == FALSE]), digits = 2)), ' | Mean for game days:', as.character(round(mean(filteredDf2$n[filteredDf2$Game.Day]), digits = 2)))
  })
  })
# Final Function Call to Generate RShiny Page----
shinyApp(ui = ui, server = server)