rm(list = ls())
library(tidymodels)
library(beans)
library(bundle)
library(shiny)
library(rsconnect)
library(tidyverse)
library(ranger)

# Read data
set.seed(250)
bean_split = initial_split(beans, prop = 0.8, strata = class)
bean_training <- training(bean_split)
bean_testing <- testing(bean_split)

rf.data.file.name <- "RF_predict.RData"
load(rf.data.file.name)

set.seed(runif(890))
sample_test <- sample_n(bean_testing, 2)

model_object <- unbundle(rf_bundle)
table(as.character(predict(model_object, bean_testing)[[1]]), as.character(bean_testing$class))


# Define your Shiny UI here
ui <- fluidPage(
  
  # App title 
  titlePanel("An interactive dashboard for machine learning models for bean classification - TEAM BEAN"),
  
  navbarPage("",
             
             tabPanel(
               "Interactively explore model",
               
               sidebarLayout(
                 
                 # Sidebar panel for inputs ----
                 sidebarPanel(
                   
                   # Input: Selector for choosing outcome ----
                   selectInput(inputId = "outcome",
                               label = "Choose an outcome:",
                               choices = c("Diarrhea under 5s")),
                   
                 ),
              
                 
                 # Input: Selector for choosing input ----
                 selectInput(inputId = "input",
                             label = "Choose a model:",
                             choices = c("Random Forest",
                                         "XGBoost"))),
                 
                 mainPanel(
                   textOutput("outcome_title"),
                   verbatimTextOutput("outcome_summary"),
                   textOutput("input_title"),
                   verbatimTextOutput("input_summary"),
                   plotOutput("plot")
                   
                 )
               ),
             
             tabPanel("Model Training"), 
             
             tabPanel("Interactive Component"), 
             
             tabPanel("Classify multiple observations")))


# Define your Shiny server logic here
server <- function(input, output, session){
  
  # Return the requested dataset ----
  datasetOutcome <- reactive({
    switch(input$outcome, 
           "Diarrhea under 5s" = "diarrhea_under_5")
  })
  
}

# Create and launch the Shiny app
shinyApp(ui, server)



