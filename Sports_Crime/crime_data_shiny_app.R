# Steven Wasserman, sw4kh
# SYS 2202, Prof. Doryab
# RShiny Deployment of Sports-Crime Data

# Calls necessary libraries ----
library(dplyr)
library(ggplot2)
library(leaflet)
library(shiny)
library(DT)
library(ggrepel)
library(tidyr)
library(shinycssloaders)
library(shinythemes)


# temp <- Sports_Crime %>% group_by(opponent) %>% tally()
# opponents <- ordered(Sports_Crime$opponent, levels = as.string(temp$opponent))

# Defines a server for the Shiny App ----
SERVER <- function(input, output, session) {
  # Implements functions to parse data
  Data_finder <- reactive({
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
  
  # Creates a map plot of the data points
  output$mapPlot <- renderPlot({
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
                   label = as.string("Date of Incident: " + event_date + "\nCrime Type: " + crime_type))
    })
  })
}

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
                                      choices = levels(Events),
                                      selected = "50 Free",
                                      width = "220px"
                          ),
                        )
                      )
                      )
             )
)

# 



# Final line call ----
shinyApp(ui = UI, server = SERVER)
# 