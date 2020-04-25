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

# Creates the user interface ----
UI <- fluidPage(
  # Creates a navigation bar for different visualizations
  navbarPage("UVA Sports-Crime Data Visualization",
             theme = shinytheme("sandstone"),
             tabPanel("DataMap", 
                      fluid = TRUE,
                      icon = icon("globe-americas"),
                      
                      # Sidebar for map tab
                      sidebarLayout(
                        
                        sidebarPanel( 
                          
                          titlePanel("Map of Reported Crimes in Charlottesville on Game Days"),
                          
                          fluidRow(column(2, 
                                          
                                          # Home / Away Games ----
                                          checkboxGroupInput(
                                            inputId = "HomeFinder",
                                            label = "Home or Away? : ",
                                            choices = c(
                                              "Home" = "",
                                              "Away" = ""),
                                            selected = "Home"),
                                          
                                          # UVA Win / Lose Result ----
                                          checkboxGroupInput(
                                            inputId = "WinFinder",
                                            label = "UVA win or lose? : ",
                                            choices = c(
                                              "Wa-hoo-wa!" = "",
                                              "Tech Sucks :( " = ""),
                                            selected = "Wa-hoo-wa!"),
                                          
                          )),
                          # Select opponent school
                                                   selectInput(inputId = "OpponentFinder",
                                                              label = "Select Opponent School",
                                                             choices = Sports_Crime$Opponent,
                                                            selected = "50 Free",
                                                           width = "220px"
                                              ),
                        ),
                        mainPanel(
                          plotOutput(outputId = mapPlot)
                          
                        )
                      )
             )
  )
)

# Defines a server for the Shiny App ----
SERVER <- function(input, output, session) {
  # Implements functions to parse data
  data_finder <- reactive({
    req(input$DateFinder)     # event_date (str)
    req(input$SportTypeFinder)# sport (str)
    req(input$OpponentFinder) # opponent (str)
    req(input$HomeFinder)     # home (bool)
    req(input$WinFinder)      # win (bool)
    req(input$CrimeTypeFinder)# crime_type(str)
    filter(Sports_Crime, event_date %in% input$DateFinder)%>%
      filter(sport %in% input$SportTypeFinder) %>%
      filter(opponent %in% input$OpponentFinder) %>%
      filter(home %in% input$HomeFinder) %>%
      filter(win %in% input$WinFinder) %>%
      filter(crime_type %in% input$CrimeTypeFinder)
  })
  output$mapPlot <- renderLeaflet({
    input$DateFinder     
    input$SportTypeFinder
    input$OpponentFinder 
    input$HomeFinder     
    input$WinFinder     
    input$CrimeTypeFinder
    isolate({
      leaflet(data = Sports_Crime) %>% addTiles() %>%
        addMarkers(~longitude,
                   ~latitude,
                   
                   # Creates a map plot of the data points
                   label = ("Date of Incident: " + event_date + "\nCrime Type: " + crime_type))
    })
  })
}


# 



# Final line call ----
shinyApp(ui = UI, server = SERVER)
# 