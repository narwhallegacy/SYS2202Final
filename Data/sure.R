# Ryan Ahmadiyar (ra7be), George Corbin (glc5pn), Steven Wasserman (sw4kh)
# SYS 2202, Prof. Doryab
# RShiny Deployment of Sports-Crime Data

# 1 : Calls necessary libraries ----
library(dplyr)
library(ggplot2)
library(leaflet)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)

Sports_Crime = read.csv("Sports_Crime.csv", header = TRUE)


# UI ----
ui = fluidPage(
  # Give the page a title
  titlePanel("Map of Reported Crimes in Charlottesville on Game Days"),
  # Generate a row with a sidebar
  sidebarLayout( sidebarPanel( 
      titlePanel("Crime Data"),
      fluidRow(column(9, # Selection for home game or away game
                      radioButtons( inputId = "HomeFinder", label = "Home or Away? : ",
                        choices = c( "Home" = TRUE, "Away" = FALSE), selected = "Home"),
                      # Selection for UVA win or UVA lose
                      radioButtons( inputId = "WinFinder", label = "UVA win or lose? : ",
                        choices = c( "Wa-hoo-wa!" = TRUE, "Tech Sucks :( " = FALSE), selected = "Wa-hoo-wa!"),
      )),
      # Selection for UVA-game opponent school
      selectInput(inputId = "OpponentFinder", label = "Select Opponent School", 
                  choices = Sports_Crime$Opponent, width = "220px"), ),
    mainPanel( leafletOutput("mapPlot") ) ) )

# Server ----
server = function(input, output) {
  
  
  
  
  output$mapPlot <- renderLeaflet({
    
    
    isolate({
      
      # Render the map plot
      leaflet(data = Sports_Crime) %>% addTiles() %>%
        addMarkers(~Longitude,
                   ~Latitude,
                   # Creates a map plot of the data points
                   #label = ("Date of Incident: " + Date + "\nCrime Type: " + Offense))
                   label = ("Crime Type: " + Sports_Crime$crime_type))
    }) })
  
  
}


# ----
shinyApp(ui = ui, server = server)