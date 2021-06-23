library(shiny)

library(shinyWidgets)

library(shinythemes)



# Define UI for application that draws a histogram

shinyUI(fluidPage(
  
  theme  = shinytheme("cerulean"),
  
  # Application title
  
  tags$h1("Automated  Model Builder"),
  
  br(),
  
  br(),
  
  sidebarLayout(
    
    sidebarPanel(
      
      fileInput("file1", "Choose CSV File To Load",
                
                multiple = FALSE,
                
                ".csv"),
      
      # Horizontal line ----
      
      tags$hr(),
      
      radioButtons("Choice", NULL, c("Predicition" =0, "Classification"=1), 0)
      
    ),
    
    
    
    mainPanel(
      
      tabsetPanel(type = "tabs",
                  
                  tabPanel("Table",
                           
                           h3("Preview Data Frame", align = "center"),
                           
                           # Output: Data file ----
                           
                           dataTableOutput("contents")),
                  
                  
                  
                  tabPanel("Models", dataTableOutput("rModelTable"),
                           
                           tags$script("$(document).on('click','#rModelTable button', function () {

                                                            Shiny.onInputChange('lastClickId', this.id);

                                                            Shiny.onInputChange('lastClick', Math.random())}); ")),
                  
                  tabPanel("Model Summary",
                           
                           verbatimTextOutput("preview")),
                  
                  
                  
                  tabPanel("Diagnostic Model Plots",
                           
                           verticalLayout(splitLayout(plotOutput("diagnostic1"), plotOutput("diagnostic2")), splitLayout(plotOutput("diagnostic3"), plotOutput("diagnostic4"))))
                  
      )
      
    )
    
  )))





