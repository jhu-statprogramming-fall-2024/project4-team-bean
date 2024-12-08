rm(list = ls())
library(tidymodels)
library(beans)
library(bundle)
library(shiny)
library(rsconnect)
library(tidyverse)
library(ranger)

# Read data

## get Range of bean variables for slider inputs
data <- beans
names(data)
beans_summary_stats <- data.frame(cbind(t(beans %>% select(-"class") %>% summarise_all(min)), 
                             t(beans %>% select(-"class") %>% summarise_all(max)),
                             t(beans %>% select(-"class") %>% summarise_all(.funs = function(x) min(x)/2 + max(x)/2))))

names(beans_summary_stats) <- c("min", "max", "mid")
beans_summary_stats <- cbind(data.frame("var" = rownames(beans_summary_stats)),
                             beans_summary_stats)
rownames(beans_summary_stats) <- NULL

vars <- beans_summary_stats$var

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
             
             tabPanel("Explore dataset", 
                      
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          
                          
                          # Input: Selector for model
                          selectInput(inputId = "data_plot.x",
                                      label = "Choose a variable to plot on x axis:",
                                      choices = vars),
                          
                          selectInput(inputId = "data_plot.y",
                                      label = "Choose a variable to plot on y axis:",
                                      choices = vars)),
                        
                        
                        mainPanel(
                          
                          plotOutput("scatter_plot")
                        )
                      )),
             
             tabPanel(
               "Interactively explore model",
               
               sidebarLayout(
                 
                 # Sidebar panel for inputs ----
                 sidebarPanel(
                   
                   # Input: Selector for choosing visualisation
                   selectInput(inputId = "outcome",
                               label = "Choose an outcome:",
                               choices = c("Diarrhea under 5s")),
                   
                    
                     
                     # Input: Selector for model
                     selectInput(inputId = "model_type",
                                 label = "Choose a model:",
                                 choices = c("Random Forest",
                                             "XGBoost")),
                 
                 
                 # Sliders
                 "Sliders to choose variable values:",
                 
                 sliderInput(inputId = "area", 
                             label = "area:",
                             min = beans_summary_stats$min[which(beans_summary_stats$var == "area")], 
                             max = beans_summary_stats$max[which(beans_summary_stats$var == "area")],
                             value = beans_summary_stats$mid[which(beans_summary_stats$var == "area")])
                 
                 ),
                   
                 
                 mainPanel(
                   "Main panel"
                   
                 )
               )),
             
             tabPanel("Model Training"), 
             
             tabPanel("Interactive Component"), 
             
             tabPanel("Classify multiple observations")))


# Define your Shiny server logic here
server <- function(input, output, session){
  
  output$scatter_plot <- renderPlot({
    
    plot_data <- data
    
    plot_data$xvar <- unlist(data[,input$data_plot.x])
    
    plot_data$yvar <- unlist(data[,input$data_plot.y])
    
    colnames(plot_data)[c(ncol(plot_data)-1, ncol(plot_data))] <- c("xvar", "yvar")
    
    ggplot(plot_data, aes(x = xvar, y = yvar, colour = class)) +
      geom_point() +
      theme_minimal()
    
    
  })
  
  
}




# Create and launch the Shiny app
shinyApp(ui, server)



