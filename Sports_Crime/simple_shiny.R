# install.packages('shiny')
library(shiny)
library(leaflet)
library(datasets)
Sports_Crime = read.csv("Sports_Crime.csv", header = TRUE)

# Define a server for the Shiny app
server = function(input, output) {
  # Fill in the spot we created for a plot
  output$mapPlot <- renderLeaflet({
    # Render the map plot
    leaflet(data = Sports_Crime) %>% addTiles() %>%
      addMarkers(~Longitude,
                 ~Latitude,
                 # Creates a map plot of the data points
                 #label = ("Date of Incident: " + Date + "\nCrime Type: " + Offense))
                label = ("Crime Type: " + Sports_Crime$crime_type))
  })
}

# Use a fluid Bootstrap layout
ui = fluidPage(
  # Give the page a title
  titlePanel("Map of Reported Crimes in Charlottesville on Game Days"),
  # Generate a row with a sidebar
  sidebarLayout(
    sidebarPanel( 
      titlePanel("Crime Data"),
      fluidRow(column(9,
                      # Selection for home game or away game
                      radioButtons(
                        inputId = "HomeFinder",
                        label = "Home or Away? : ",
                        choices = c(
                          "Home" = "",
                          "Away" = "",
                          "(NO FILTER)" = ""),
                        selected = "Home"),
                      # Selection for UVA win or UVA lose
                      radioButtons(
                        inputId = "WinFinder",
                        label = "UVA win or lose? : ",
                        choices = c(
                          "Wa-hoo-wa!" = "",
                          "Tech Sucks :( " = "",
                          "(NO FILTER)" = ""),
                        selected = "Wa-hoo-wa!"),
      )),
      # Selection for UVA-game opponent school
      selectInput(inputId = "OpponentFinder",
                  label = "Select Opponent School",
                  choices = Sports_Crime$Opponent,
                  width = "220px"
      ),
    ),
    mainPanel(
      leafletOutput("mapPlot")
    )
  )
)

shinyApp(ui = ui, server = server)