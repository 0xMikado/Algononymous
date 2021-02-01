#-------------------------------------
# Algononymous shiny app
# Author: Nicolas Martinod
#-------------------------------------

rm(list=ls())
# Load R packages for shiny app
library(shiny)
library(shinythemes)
library(shinydashboard)
#Load SDC package
library(sdcMicro)
#Load a user-friendly table viewer
library(reactable)

#Call the Function.R script within the app
source("Functions.R")

#--------------------------------------------------------------------------------------------
# Define UI
#     In the Dashboard template of Shiny the UI is composed of: 
#         - a header, 
#         - a sidebar and,
#         - a body
#--------------------------------------------------------------------------------------------

  header <- dashboardHeader(title = 'Algononymous')

#-----------------------------------------------------------
# Sidebar: here we list all the tab we want in our Dashboard
#   We give them a name, an identifier (tabName) and an icon taken from fontawesome.com
#-----------------------------------------------------------
  
  sidebar <- dashboardSidebar(
            sidebarMenu(
              menuItem("Information", tabName = "information", icon = icon("info-circle")),
              menuItem("Upload file", tabName = "uploadfile", icon = icon("upload")),
              menuItem("Product", tabName = "product", icon = icon("database")),
              menuItem("Anonymization", tabName = "product", icon = icon("cog")),
              menuItem("Result", tabName = "product", icon = icon("poll")),
              menuItem("Export", tabName = "product", icon = icon("file-export"))
            )
          )
       
  body <- dashboardBody(
         tabItems(
           #-------------------------------------------------
           # First tab content
           #-------------------------------------------------
        
           tabItem(tabName = "information",
                   h2("What is Algononymous"),
                   br(),
                   "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor 
                    incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis 
                    nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. 
                    Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. 
                    Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt 
                    mollit anim id est laborum."
                   ),
          
           #-------------------------------------------------
           # Second tab content
           #-------------------------------------------------
          
           #Use the tabName identifier defined in Sidebar
          tabItem(tabName = "uploadfile",  
                  
                  #The type of Shniy UI we want to use in this tab
                  fluidPage(
                    
                    # Main tab title
                    titlePanel("Uploading Files"),
                    
                    # Sidebar layout with input and output definitions
                    sidebarLayout(
                      
                      # Sidebar panel for inputs
                      sidebarPanel(
                        
                        # Input: Select a file
                        fileInput("file1", "Choose CSV File",
                                  multiple = TRUE,
                                  accept = c("text/csv",
                                             "text/comma-separated-values,text/plain",
                                             ".csv")),
                        
                        # Horizontal line 
                        tags$hr(),
                        
                        # Input: Checkbox if file has header
                        checkboxInput("header", "Header", TRUE),
                        
                        # Input: Select separator
                        radioButtons("sep", "Separator",
                                     choices = c(Comma = ",",
                                                 Semicolon = ";",
                                                 Tab = "\t"),
                                     selected = ","),
                        
                        # Input: Select quotes
                        radioButtons("quote", "Quote",
                                     choices = c(None = "",
                                                 "Double Quote" = '"',
                                                 "Single Quote" = "'"),
                                     selected = '"'),

                      ),
                      
                      # Main panel for displaying outputs ----
                      mainPanel(
                        
                        # Output: Data file ----
                        reactableOutput("uploaded_file")
                      )
                      
                    )
                  )
                  ),
          
          #-------------------------------------------------
          # Third tab content
          #-------------------------------------------------
          
          #Tab tittle
          tabItem(tabName = "product",
                  
                  #Type of UI to use
                  fluidPage(
                    
                    #Separete the page in rows
                   fluidRow(
                   #-------------------------------------------------
                   # Box dataset selection
                   #-------------------------------------------------
                            column(4,
                              box( title = "Dataset selection", status = "primary", solidHeader = TRUE, width = '100%',
                                selectInput("select", label = h4("Select the IDDO dataset you want to access"), 
                                      choices = list("Ebola Demographic", "Death Detail", ""), 
                                      selected = NULL) 
                              )
                            ),
                   #-------------------------------------------------
                   # Box dataset characteristics
                   #-------------------------------------------------
                            column(8,
                              box(
                                title = "Dataset characteristics", status = "primary", solidHeader = TRUE,
                                width = '100%', 
                                
                          #-------------------------------------------------
                          # In this box, display a panel with 3 tabs
                          #-------------------------------------------------
                                tabsetPanel(
                                  tabPanel("Overview", 
                                           br(),                                                           #break line
                                           h4(strong(textOutput("dataset"))),
                                           h4(textOutput("abstract"), style = "font-size: 16px"),
                                           br(),
                                           fluidRow(
                                             valueBoxOutput("patient_number"),
                                             valueBoxOutput("sensible_variable"),
                                             valueBoxOutput("risk")
                                           )
                                  ), 
                                  tabPanel("Variables",
                                           tableOutput('variable')
                                  ), 
                                  
                                  tabPanel("Table", 
                                           reactableOutput("table")
                                  )
                                )
                              )
                            )
                  ),
                  
             
                 fluidRow( 
                 #-------------------------------------------------
                 #  Box Minimal clinically relevant dataset
                 #-------------------------------------------------
                   column(4,
                      box(
                        title = "Minimal clinically relevant dataset", status = "primary", solidHeader = TRUE,
                        width = '100%', 
                        # Copy the chunk below to make a group of checkboxes
                        checkboxGroupInput("checkGroup", label = h3("Select required variables"), 
                                           choices = list("RFSTDTC", "DTHFL", "SITEID", "AGE", "AGEU", "SEX", "COUNTRY"),
                                           selected = 1)
                      )
                   ),
                 #-------------------------------------------------
                 # Box privacy risk associated
                 #-------------------------------------------------
                  column(8, 
                         box(
                           title = "Privacy risk associated", status = "primary", solidHeader = TRUE,
                           width = '100%',
                           reactableOutput("risk_measure") #Output associeted to this box 
                            )
                  )
                 
                ) #end of fluidrow
        ) #end of fluidpage
      ) #end of TabItem
   ) #end of TabItem 
  ) #end of body dashboard

#-------------------------------------------------
# Add the 3 composents defined above inside the ui
#-------------------------------------------------
  ui <- dashboardPage(header,sidebar,body)
  
  
#-------------------------------------------------
# Define the server (back-end)
#-------------------------------------------------  
  server <- function(input, output) {
    
  #-------------------------------------
  # First tab back-end
  #-------------------------------------
  
    #No back-end for the first tab
  
  #-------------------------------------
  # Second tab back-end
  #-------------------------------------
   
     upload <- reactive({
      # input$file1 will be NULL initially. After the user selects
      # and uploads a file, head of that data file by default,
      # or all rows if selected, will be shown.
      
      req(input$file1)
      
      df <- read.csv(input$file1$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
    })
    
    output$uploaded_file <- renderReactable({
      
      reactable(upload(), showPageSizeOptions = TRUE, pageSizeOptions = c(10, 50, 100), defaultPageSize = 10)
      
    })
  #-------------------------------------
  # Third tab back-end
  #-------------------------------------


    #-------------------------------------
    # Box dataset selection
    #-------------------------------------
    Dataset <- reactive({
      #Calls import_dataset from Functions.R
      input_dataset <- import_dataset(input$select)
    })
     
     #-------------------------------------
     # Box dataset characteristics, Overview tab
     #-------------------------------------
     output$abstract <- renderText({
       #Calls dataset_abstracts from Functions.R
       abstract <- dataset_abstract(input$select)
     })
     
     output$dataset <- renderText({dataset <- input$select})
     
     output$patient_number <- renderValueBox({
       valueBox(
         value = tags$p("9472", style = "font-size: 90%;"),
         subtitle = tags$p("Number of patient", style = "font-size: 90%;"), icon = tags$i(class = "fas fa-hospital-user", style="font-size: 50px; color:white"),
         color = "blue"
       )
     })
     
     output$sensible_variable <- renderValueBox({
       valueBox(
         value = tags$p("7", style = "font-size: 90%;"),
         subtitle = tags$p("Sensible variables", style = "font-size: 90%;"), icon = tags$i(class = "fas fa-exclamation-triangle", style="font-size: 50px; color:white"),
         color = "yellow"
       )
     })
     output$risk <- renderValueBox({
       valueBox(
         value = tags$p("37%", style = "font-size: 90%;"),
         subtitle = tags$p("Privacy risk", style = "font-size: 90%;"), icon = tags$i(class = "fas fa-user-secret", style="font-size: 50px; color:white"),
         color = "red"
       )
     })
     
     #-------------------------------------
     # Box dataset characteristics, Variable tab
     #-------------------------------------
        #Call the variable_overview function from Functions.R
     output$variable <- renderTable( variable <- variable_overview(), bordered = TRUE )
     
     #-------------------------------------
     # Box dataset characteristics, Table tab
     #-------------------------------------
     output$table<- renderReactable({
       reactable(Dataset(), showPageSizeOptions = TRUE, pageSizeOptions = c(10, 50, 100), defaultPageSize = 10)
     })
     
     #-------------------------------------
     # Box Minimal clinically relevant dataset
     #-------------------------------------
     output$value <- renderPrint({ input$checkGroup })
    
     #-------------------------------------
     # Box Privacy isk associated
     #-------------------------------------
     SDCObject <- reactive({
     })
     
     output$risk_measure <- renderReactable({
       reactable
     })
     
  } # server
  

  # Create Shiny object
  shinyApp(ui = ui, server = server)
