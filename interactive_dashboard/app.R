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

# Read in random forest
rf.data.file.name <- "RF_predict.RData"
load(rf.data.file.name)

set.seed(runif(890))
sample_test <- sample_n(bean_testing, 2)

model_object <- unbundle(rf_bundle)
table(as.character(predict(model_object, bean_testing)[[1]]), as.character(bean_testing$class))

models <- list()
models[["Random Forest"]] <- model_object

# Read in Random Forest





# Define your Shiny UI here
ui <- fluidPage(
  
  # App title 
  titlePanel("Machine learning models for bean classification - TEAM BEAN"),
  
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
                   
                     
                     # Input: Selector for model
                     selectInput(inputId = "model_type",
                                 label = "Choose a model:",
                                 choices = c("Random Forest",
                                             "XGBoost")),
                 
                 
                 # Sliders
                 "Choose variable values:",
                 
                 sliderInput(inputId = "area", 
                             label = "area:",
                             min = beans_summary_stats$min[which(beans_summary_stats$var == "area")], 
                             max = beans_summary_stats$max[which(beans_summary_stats$var == "area")],
                             value = beans_summary_stats$mid[which(beans_summary_stats$var == "area")]),


                 sliderInput(inputId = "perimeter",
                             label = "perimeter:",
                             min = beans_summary_stats$min[which(beans_summary_stats$var == "perimeter")],
                             max = beans_summary_stats$max[which(beans_summary_stats$var == "perimeter")],
                             value = beans_summary_stats$mid[which(beans_summary_stats$var == "perimeter")]),


               sliderInput(inputId = "major_axis_length",
                           label = "major_axis_length:",
                           min = beans_summary_stats$min[which(beans_summary_stats$var == "major_axis_length")],
                           max = beans_summary_stats$max[which(beans_summary_stats$var == "major_axis_length")],
                           value = beans_summary_stats$mid[which(beans_summary_stats$var == "major_axis_length")]),

               sliderInput(inputId = "minor_axis_length",
                           label = "minor_axis_length:",
                           min = beans_summary_stats$min[which(beans_summary_stats$var == "minor_axis_length")],
                           max = beans_summary_stats$max[which(beans_summary_stats$var == "minor_axis_length")],
                           value = beans_summary_stats$mid[which(beans_summary_stats$var == "minor_axis_length")]),

               sliderInput(inputId = "aspect_ratio",
                           label = "aspect_ratio:",
                           min = beans_summary_stats$min[which(beans_summary_stats$var == "aspect_ratio")],
                           max = beans_summary_stats$max[which(beans_summary_stats$var == "aspect_ratio")],
                           value = beans_summary_stats$mid[which(beans_summary_stats$var == "aspect_ratio")]),

               sliderInput(inputId = "eccentricity",
                           label = "eccentricity:",
                           min = beans_summary_stats$min[which(beans_summary_stats$var == "eccentricity")],
                           max = beans_summary_stats$max[which(beans_summary_stats$var == "eccentricity")],
                           value = beans_summary_stats$mid[which(beans_summary_stats$var == "eccentricity")]),

               sliderInput(inputId = "convex_area",
                           label = "convex_area:",
                           min = beans_summary_stats$min[which(beans_summary_stats$var == "convex_area")],
                           max = beans_summary_stats$max[which(beans_summary_stats$var == "convex_area")],
                           value = beans_summary_stats$mid[which(beans_summary_stats$var == "convex_area")]),

               sliderInput(inputId = "equiv_diameter",
                           label = "equiv_diameter:",
                           min = beans_summary_stats$min[which(beans_summary_stats$var == "equiv_diameter")],
                           max = beans_summary_stats$max[which(beans_summary_stats$var == "equiv_diameter")],
                           value = beans_summary_stats$mid[which(beans_summary_stats$var == "equiv_diameter")]),

               sliderInput(inputId = "extent",
                           label = "extent:",
                           min = beans_summary_stats$min[which(beans_summary_stats$var == "extent")],
                           max = beans_summary_stats$max[which(beans_summary_stats$var == "extent")],
                           value = beans_summary_stats$mid[which(beans_summary_stats$var == "extent")]),

               sliderInput(inputId = "solidity",
                           label = "solidity:",
                           min = beans_summary_stats$min[which(beans_summary_stats$var == "solidity")],
                           max = beans_summary_stats$max[which(beans_summary_stats$var == "solidity")],
                           value = beans_summary_stats$mid[which(beans_summary_stats$var == "solidity")]),

               sliderInput(inputId = "roundness",
                           label = "roundness:",
                           min = beans_summary_stats$min[which(beans_summary_stats$var == "roundness")],
                           max = beans_summary_stats$max[which(beans_summary_stats$var == "roundness")],
                           value = beans_summary_stats$mid[which(beans_summary_stats$var == "roundness")]),

               sliderInput(inputId = "compactness",
                           label = "compactness:",
                           min = beans_summary_stats$min[which(beans_summary_stats$var == "compactness")],
                           max = beans_summary_stats$max[which(beans_summary_stats$var == "compactness")],
                           value = beans_summary_stats$mid[which(beans_summary_stats$var == "compactness")]),

               sliderInput(inputId = "shape_factor_1",
                           label = "shape_factor_1:",
                           min = beans_summary_stats$min[which(beans_summary_stats$var == "shape_factor_1")],
                           max = beans_summary_stats$max[which(beans_summary_stats$var == "shape_factor_1")],
                           value = beans_summary_stats$mid[which(beans_summary_stats$var == "shape_factor_1")]),

               sliderInput(inputId = "shape_factor_2",
                           label = "shape_factor_2:",
                           min = beans_summary_stats$min[which(beans_summary_stats$var == "shape_factor_2")],
                           max = beans_summary_stats$max[which(beans_summary_stats$var == "shape_factor_2")],
                           value = beans_summary_stats$mid[which(beans_summary_stats$var == "shape_factor_2")]),

               sliderInput(inputId = "shape_factor_3",
                           label = "shape_factor_3:",
                           min = beans_summary_stats$min[which(beans_summary_stats$var == "shape_factor_3")],
                           max = beans_summary_stats$max[which(beans_summary_stats$var == "shape_factor_3")],
                           value = beans_summary_stats$mid[which(beans_summary_stats$var == "shape_factor_3")]),


               sliderInput(inputId = "shape_factor_4",
                           label = "shape_factor_4:",
                           min = beans_summary_stats$min[which(beans_summary_stats$var == "shape_factor_4")],
                           max = beans_summary_stats$max[which(beans_summary_stats$var == "shape_factor_4")],
                           value = beans_summary_stats$mid[which(beans_summary_stats$var == "shape_factor_4")])


             
             ),
                   
                 
                 mainPanel(
                   "Main panel",
                   
                   tableOutput("model_prediction")
                   
                 )
               )),
             
             tabPanel("Model Training"), 
             
             tabPanel("Interactive Component"), 
             
             tabPanel("Classify multiple observations")))


# Define your Shiny server logic here
server <- function(input, output, session){
  
  # Reactive expression to create data frame of all input values ----
  sliderValues <- reactive({
    
    data.frame(
      "Name" = c("area",
               "perimeter",
               "major_axis_length",
                "minor_axis_length",
                "aspect_ratio",
                "eccentricity",
                "convex_area",
                "equiv_diameter",
                "extent",
                "solidity",
                "roundness",
                "compactness",
                "shape_factor_1",
                "shape_factor_2",
                "shape_factor_3",
                "shape_factor_4"),
      
      "Value" = c(input$area,
                 input$perimeter,
                 input$major_axis_length,
                 input$minor_axis_length,
                 input$aspect_ratio,
                 input$eccentricity,
                 input$convex_area,
                 input$equiv_diameter,
                 input$extent,
                 input$solidity,
                 input$roundness,
                 input$compactness,
                 input$shape_factor_1,
                 input$shape_factor_2,
                 input$shape_factor_3,
                 input$shape_factor_4))
    
  })
  
  
  output$scatter_plot <- renderPlot({
    
    plot_data <- data
    
    plot_data$xvar <- unlist(data[,input$data_plot.x])
    
    plot_data$yvar <- unlist(data[,input$data_plot.y])
    
    colnames(plot_data)[c(ncol(plot_data)-1, ncol(plot_data))] <- c("xvar", "yvar")
    
    ggplot(plot_data, aes(x = xvar, y = yvar, colour = class)) +
      geom_point() +
      theme_minimal()
    
    
  })
  
  output$model_prediction <- renderTable({

    inputs <- sliderValues()
    inputs <- data.frame(inputs)
    
    inputs <- inputs %>% pivot_wider(id_cols = NULL, names_from = Name, values_from = Value)
    
    inputs
    
    model_name <- input$model_type
    model <- models[[model_name]]
    
    prediction <- predict(models[["Random Forest"]], inputs)
    paste0("Here is a prediction: ", prediction)
    
    
  })
  
  

  
}




# Create and launch the Shiny app
shinyApp(ui, server)



