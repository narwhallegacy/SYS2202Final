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
                               separator = ' to ', width = '400px'),
                ),
                mainPanel( leafletOutput("mapPlot") ) ) 
          ),
          # Tab 2: Comparison of Reported Crimes to Monthly Average 
          tabPanel("Monthly Average Comparison", fluid = TRUE, icon = icon("calendar"), 
                   # Tab 1: Map of Reported Incidents on Game Day
                   titlePanel("Map of Reported Crimes in Charlottesville on Game Days"),
                   sidebarLayout( sidebarPanel( 
                     # Selection for incident-report date range
                     dateRangeInput(inputId="dateFinder", label = 'Month-Year',
                                    start = "2015-09", end = "2020-03",
                                    min = "2015-09", max = "2020-03", format = "yyyy-mm",
                                    startview = "year",weekstart = 0, language = "en",
                                    separator = ' to ', width = '400px'),
                   ),
                   mainPanel( leafletOutput("barPlot") ) ) 
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
    
    
  })
  
  
  })
# Final Function Call to Generate RShiny Page----
shinyApp(ui = ui, server = server)