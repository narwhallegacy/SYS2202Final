# install.packages('shiny')
library(shiny)

# Rely on the 'Sports_Crime' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)
Sports_Crime = read.csv("Sports_Crime.csv", header = TRUE)

# Define a server for the Shiny app
server = function(input, output) {
  
  # Fill in the spot we created for a plot
  output$mapPlot <- renderPlot({
    
    # Render a barplot
    leaflet(data = Sports_Crime) %>% addTiles() %>%
      addMarkers(~longitude,
                 ~latitude,
                 
                 # Creates a map plot of the data points
                 label = as.string("Date of Incident: " + event_date + "\nCrime Type: " + crime_type))
  })
}

# Use a fluid Bootstrap layout
ui = fluidPage(    
  
  # Give the page a title
  titlePanel("TEST"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("oppoent", "Opponent:", 
                  choices = Sports_Crime$Opponent),
      hr(),
      helpText("Data from CVille City Data")
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotOutput("phonePlot")  
    )
    
  )
)

shinyApp(ui = ui, server = server)