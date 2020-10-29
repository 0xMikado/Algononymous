


# Load R packages
library(shiny)
library(shinythemes)


  # Define UI
  ui <- fluidPage(theme = shinytheme("united"),
    
  ) 

  
  # Define server function  
  server <- function(input, output) {
    
    
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
