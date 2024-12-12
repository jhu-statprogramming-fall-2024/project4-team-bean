rm(list = ls())
library(tidymodels)
library(beans)
library(bundle)
library(shiny)
library(rsconnect)
library(tidyverse)
library(ranger)
library(stringr)
library(cowplot)

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

get_var_longname <- function(var){
  switch(var, 
         "area" = "Area (square pixels)",
         "perimeter" = "Perimeter (pixels)",
         "major_axis_length" = "Major axis length (pixels)",
         "minor_axis_length"  = "Minor axis length (pixels)",
         "aspect_ratio" = "Aspect ratio",
         "eccentricity" = "Eccentricity",
         "convex_area" = "Convex area (pixels)",
         "equiv_diameter"  = "Equivalent diameter (pixels)",
         "extent" = "Extent",
         "solidity" = "Solidity",
         "roundness" = "Roundness",
         "compactness" = "Compactness",
         "shape_factor_1" = "Shape Factor 1",
         "shape_factor_2" = "Shape Factor 2",
         "shape_factor_3" = "Shape Factor 3",
         "shape_factor_4" = "Shape Factor 4")}

vars_pretty <- sapply(X = vars, FUN = function(X) get_var_longname(X))
names(vars) <- vars_pretty

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


# Define your Shiny UI here
ui <- fluidPage(
  
  # App title 
  titlePanel("Machine learning models for bean classification - TEAM BEAN"),
  
    
  
  navbarPage("",
             
             tabPanel("About", 
                      
                      mainPanel(
                        "
This interactive dashboard offers users a chance to explore the behaviour of different machine learning models trained on the dry bean dataset, a popular dataset for machine learning model benchmarking and testing. The dataset can be accessed on the UCI machine learning repository [1].

More details on bean classes, and the creation of the dataset, including how the features in the dataset were extracted from photographs of beans using machine imaging techniques, can be found in the paper [2].

[1] https://www.archive.ics.uci.edu/dataset/602/dry+bean+dataset, last accessed 8 December 2024.

[2] Koklu, Murat, and Ilker Ali Ozkan. 'Multiclass classification of dry beans using computer vision and machine learning techniques.' Computers and Electronics in Agriculture 174 (2020): 105507."
                      )
                      
             ),
             
             tabPanel("Explore dataset", 
                      
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          selectInput(inputId = "model_type",
                                      label = "Choose a model:",
                                      choices = c("Random Forest",
                                                  "XGBoost")),
                          
                          # Input: Selector for model
                          selectInput(inputId = "data_plot.x",
                                      label = "Choose a variable to plot on x axis:",
                                      choices = vars),
                          
                          selectInput(inputId = "data_plot.y",
                                      label = "Choose a variable to plot on y axis:",
                                      selected = "perimeter",
                                      choices = vars),
                        # Sliders
                       "Choose variable values:",
                       " ",
                       
                       sliderInput(inputId = "area", 
                                   label = paste0(get_var_longname("area"), ":"),
                                   min = floor(beans_summary_stats$min[which(beans_summary_stats$var == "area")]/10^3)*10^3, 
                                   max = ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "area")]/10^3)*10^3,
                                   value = floor(beans_summary_stats$min[which(beans_summary_stats$var == "area")]/10^3)*10^3/2 +ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "area")]/10^3)*10^3/2 ,
                                   round = 1),
      
      
                       sliderInput(inputId = "perimeter",
                                   label = paste0(get_var_longname("perimeter"), ":"),
                                   min = floor(beans_summary_stats$min[which(beans_summary_stats$var == "perimeter")]/10^2)*10^2,
                                   max = ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "perimeter")]/10^2)*10^2,
                                   value = floor(beans_summary_stats$min[which(beans_summary_stats$var == "perimeter")]/10^2)*10^2/2 + ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "perimeter")]/10^2)*10^2/2,
                                   round = 1),
      
      
                     sliderInput(inputId = "major_axis_length",
                                 label = paste0(get_var_longname("major_axis_length"), ":"),
                                 min = floor(beans_summary_stats$min[which(beans_summary_stats$var == "major_axis_length")]/10)*10,
                                 max = ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "major_axis_length")]/10)*10,
                                 value = floor(beans_summary_stats$min[which(beans_summary_stats$var == "major_axis_length")]/10)*10/2 + ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "major_axis_length")]/10)*10/2,
                                 round = 1),
      
                     sliderInput(inputId = "minor_axis_length",
                                 label = paste0(get_var_longname("minor_axis_length"), ":"),
                                 min = floor(beans_summary_stats$min[which(beans_summary_stats$var == "minor_axis_length")]/10)*10,
                                 max = ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "minor_axis_length")]/10)*10,
                                 value = floor(beans_summary_stats$min[which(beans_summary_stats$var == "minor_axis_length")]/10)*10/2 + ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "minor_axis_length")]/10)*10/2, 
                                 round = 1),
      
                     sliderInput(inputId = "aspect_ratio",
                                 label = paste0(get_var_longname("aspect_ratio"), ":"),
                                 min = floor(beans_summary_stats$min[which(beans_summary_stats$var == "aspect_ratio")]/.01)*.01,
                                 max = ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "aspect_ratio")]/.01)*.01,
                                 value = floor(beans_summary_stats$min[which(beans_summary_stats$var == "aspect_ratio")]/.01)*.01/2 + ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "aspect_ratio")]/.01)*.01/2,
                                 round = -2),
      
                     sliderInput(inputId = "eccentricity",
                                 label = paste0(get_var_longname("eccentricity"), ":"),
                                 min = floor(beans_summary_stats$min[which(beans_summary_stats$var == "eccentricity")]/.01)*.01,
                                 max = ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "eccentricity")]/.01)*.01,
                                 value = floor(beans_summary_stats$min[which(beans_summary_stats$var == "eccentricity")]/.01)*.01/2 + ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "eccentricity")]/.01)*.01/2, 
                                 round = -.2),
      
                     sliderInput(inputId = "convex_area",
                                 label = paste0(get_var_longname("convex_area"), ":"),
                                 min = floor(beans_summary_stats$min[which(beans_summary_stats$var == "convex_area")]/10^3)*10^3,
                                 max = ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "convex_area")]/10^3)*10^3,
                                 value = floor(beans_summary_stats$min[which(beans_summary_stats$var == "convex_area")]/10^3)*10^3/2 + ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "convex_area")]/10^3)*10^3/2,
                                 round = 1),
      
                     sliderInput(inputId = "equiv_diameter",
                                 label = paste0(get_var_longname("equiv_diameter"), ":"),
                                 min = floor(beans_summary_stats$min[which(beans_summary_stats$var == "equiv_diameter")]/100)*100,
                                 max = ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "equiv_diameter")]/100)*100,
                                 value = floor(beans_summary_stats$min[which(beans_summary_stats$var == "equiv_diameter")]/100)*100/2 + ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "equiv_diameter")]/100)*100/2,
                                 round =1),
      
                     sliderInput(inputId = "extent",
                                 label = paste0(get_var_longname("extent"), ":"),
                                 min = floor(beans_summary_stats$min[which(beans_summary_stats$var == "extent")]/0.01)*0.01,
                                 max = ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "extent")]/0.01)*0.01,
                                 value = floor(beans_summary_stats$min[which(beans_summary_stats$var == "extent")]/0.01)*0.01/2 + ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "extent")]/0.01)*0.01/2,
                                 round = -2),
      
                     sliderInput(inputId = "solidity",
                                 label = paste0(get_var_longname("solidity"), ":"),
                                 min = floor(beans_summary_stats$min[which(beans_summary_stats$var == "solidity")]/0.01)*0.01,
                                 max = ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "solidity")]/0.01)*0.01,
                                 value = floor(beans_summary_stats$min[which(beans_summary_stats$var == "solidity")]/0.01)*0.01/2 + ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "solidity")]/0.01)*0.01/2,
                                 round = -2),
      
                     sliderInput(inputId = "roundness",
                                 label = paste0(get_var_longname("roundness"), ":"),
                                 min = floor(beans_summary_stats$min[which(beans_summary_stats$var == "roundness")]/0.01)*0.01,
                                 max = ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "roundness")]/0.01)*0.01,
                                 value = floor(beans_summary_stats$min[which(beans_summary_stats$var == "roundness")]/0.01)*0.01/2 + ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "roundness")]/0.01)*0.01/2, 
                                 round = -2),
      
                     sliderInput(inputId = "compactness",
                                 label = paste0(get_var_longname("compactness"), ":"),
                                 min = floor(beans_summary_stats$min[which(beans_summary_stats$var == "compactness")]/0.01)*0.01,
                                 max = ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "compactness")]/0.01)*0.01,
                                 value = floor(beans_summary_stats$min[which(beans_summary_stats$var == "compactness")]/0.01)*0.01/2 + ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "compactness")]/0.01)*0.01/2,
                                 round = -2),
      
                     sliderInput(inputId = "shape_factor_1",
                                 label = paste0(get_var_longname("shape_factor_1"), ":"),
                                 min = floor(beans_summary_stats$min[which(beans_summary_stats$var == "shape_factor_1")]/10^-4)*10^-4,
                                 max = ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "shape_factor_1")]/10^-4)*10^-4,
                                 value = floor(beans_summary_stats$min[which(beans_summary_stats$var == "shape_factor_1")]/10^-4)*10^-4/2 + ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "shape_factor_1")]/10^-4)*10^-4/2,
                                 round = -4),
      
                     sliderInput(inputId = "shape_factor_2",
                                 label = paste0(get_var_longname("shape_factor_2"), ":"),
                                 min = floor(beans_summary_stats$min[which(beans_summary_stats$var == "shape_factor_2")]/10^-4)*10^-4,
                                 max = ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "shape_factor_2")]/10^-4)*10^-4,
                                 value = floor(beans_summary_stats$min[which(beans_summary_stats$var == "shape_factor_2")]/10^-4)*10^-4/2 + ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "shape_factor_2")]/10^-4)*10^-4/2,
                                 round = -4),
      
                     sliderInput(inputId = "shape_factor_3",
                                 label = paste0(get_var_longname("shape_factor_3"), ":"),
                                 min = floor(beans_summary_stats$min[which(beans_summary_stats$var == "shape_factor_3")]/10^-4)*10^-4,
                                 max = ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "shape_factor_3")]/10^-4)*10^-4,
                                 value = floor(beans_summary_stats$min[which(beans_summary_stats$var == "shape_factor_3")]/10^-4)*10^-4/2 + ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "shape_factor_3")]/10^-4)*10^-4/2,
                                 round = -4),
      
      
                     sliderInput(inputId = "shape_factor_4",
                                 label = paste0(get_var_longname("shape_factor_4"), ":"),
                                 min = floor(beans_summary_stats$min[which(beans_summary_stats$var == "shape_factor_4")]/10^-4)*10^-4,
                                 max = ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "shape_factor_4")]/10^-4)*10^-4,
                                 value = floor(beans_summary_stats$min[which(beans_summary_stats$var == "shape_factor_4")]/10^-4)*10^-4/2 + ceiling(beans_summary_stats$max[which(beans_summary_stats$var == "shape_factor_4")]/10^-4)*10^-4/2,
                                 round = -4)),
                        
                        mainPanel(
                          
                          strong(textOutput("plot_title")),
                          fluidRow(
                            column(6, plotOutput(outputId = "beans_legend", width  = "400px",height = "400px")),
                            column(6, plotOutput(outputId = "histogram_x", width  = "400px",height = "400px"))
                          ),
                          
                          fluidRow(
                            column(6, plotOutput(outputId = "histogram_y", width  = "400px",height = "400px")),
                            column(6, plotOutput(outputId = "scatter_plot", width  = "400px",height = "400px"))
                            
                          ),
                            
                            
                          strong("Model agreement and disagreement..."),
                          tableOutput("model_prediction")
                        )
                      ))),
             
             
             tabPanel("Model Training"), 
             
             tabPanel("Interactive Component"), 
             
             tabPanel("Classify multiple observations"))


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
  
  output$plot_title <- renderText({
    str_to_sentence(paste0(get_var_longname(input$data_plot.y), " against ", get_var_longname(input$data_plot.x), ", by bean class"))
    })
  
  output$scatter_plot <- renderPlot({
    
    plot_data <- bean_training
    
    plot_data$xvar <- unlist(bean_training[,input$data_plot.x])
    
    plot_data$yvar <- unlist(bean_training[,input$data_plot.y])
    
    colnames(plot_data)[c(ncol(plot_data)-1, ncol(plot_data))] <- c("xvar", "yvar")
    
    inputs    <- sliderValues()
    user_xval <- inputs$Value[which(inputs$Name == input$data_plot.x)]
    user_yval <- inputs$Value[which(inputs$Name == input$data_plot.y)]
    
    ggplot(plot_data, aes(x = xvar, y = yvar, colour = class)) +
      geom_point(alpha = 0.4) +
      theme_classic() +
      geom_point(inherit.aes = F, colour = "red", x = user_xval, y = user_yval, shape = 4, size = 6) +
      labs(x = get_var_longname(input$data_plot.x), 
           y = get_var_longname(input$data_plot.y)) +
      theme(legend.position = "none")
    
  })
  
  output$histogram_x <- renderPlot({
    
    plot_data <- bean_training
    
    plot_data$xvar <- unlist(bean_training[,input$data_plot.x])
    
    plot_data$yvar <- unlist(bean_training[,input$data_plot.y])
    
    colnames(plot_data)[c(ncol(plot_data)-1, ncol(plot_data))] <- c("xvar", "yvar")
    
    inputs    <- sliderValues()
    user_xval <- inputs$Value[which(inputs$Name == input$data_plot.x)]
    user_yval <- inputs$Value[which(inputs$Name == input$data_plot.y)]
    
    ggplot(plot_data, aes(x = xvar, group = class, fill = class)) +
      geom_density(linewidth = 0.5, colour = "white", alpha = 0.5) +
      theme_classic() +
      geom_vline(inherit.aes = F, xintercept = user_xval, colour = "red") +
      labs(x = get_var_longname(input$data_plot.x),
           y = "Count") +
      theme(axis.text.y = element_blank(),
            axis.ticks = element_blank(), 
            axis.line.y = element_blank(), 
            axis.text.x = element_blank(), 
            axis.ticks.x = element_blank()) +
      scale_y_continuous(expand = expansion(0)) +
      theme(legend.position = "none")
    
  })
  
  output$histogram_y <- renderPlot({
    
    plot_data <- bean_training
    
    plot_data$xvar <- unlist(bean_training[,input$data_plot.x])
    
    plot_data$yvar <- unlist(bean_training[,input$data_plot.y])
    
    colnames(plot_data)[c(ncol(plot_data)-1, ncol(plot_data))] <- c("xvar", "yvar")
    
    inputs    <- sliderValues()
    user_xval <- inputs$Value[which(inputs$Name == input$data_plot.x)]
    user_yval <- inputs$Value[which(inputs$Name == input$data_plot.y)]
    
    ggplot(plot_data, aes(x = yvar, group = class, fill = class)) +
      geom_density(linewidth = 0.5, colour = "white", alpha = 0.5) +
      theme_classic() +
      geom_vline(inherit.aes = F, xintercept = user_yval, colour = "red") +
      labs(x = get_var_longname(input$data_plot.y),
           y = "Count") + 
      theme(axis.text.y = element_blank(),
            axis.ticks = element_blank(), 
            axis.line.x = element_blank(), 
            axis.text.x = element_blank(), 
            axis.ticks.x = element_blank()) +
      coord_flip()  +
      scale_y_reverse(expand = expansion(0)) +
      scale_x_continuous(expand = expansion(0), position = "top") +
      theme(legend.position = "none")
    
  })
  
  output$beans_legend <- renderPlot({
    
    plot_data <- bean_training
    
    plot_data$xvar <- unlist(bean_training[,input$data_plot.x])
    
    plot_data$yvar <- unlist(bean_training[,input$data_plot.y])
    
    colnames(plot_data)[c(ncol(plot_data)-1, ncol(plot_data))] <- c("xvar", "yvar")
    
    inputs    <- sliderValues()
    user_xval <- inputs$Value[which(inputs$Name == input$data_plot.x)]
    user_yval <- inputs$Value[which(inputs$Name == input$data_plot.y)]
    
    p <- ggplot(plot_data, aes(x = yvar, group = class, fill = class)) +
      geom_density(linewidth = 0.5, colour = "white", alpha = 0.5) +
      theme_classic() +
      geom_vline(inherit.aes = F, xintercept = user_yval, colour = "red") +
      labs(title = paste0("Marginal distribution of ",get_var_longname(input$data_plot.x), ", by bean class"), 
           x = get_var_longname(input$data_plot.y),
           y = "Count") +
      coord_flip()
    
    legend <- get_legend(p)
    
    plot(legend)
    
  })
  
  
  output$model_prediction <- renderTable({

    inputs <- sliderValues()
    inputs <- data.frame(inputs)
    
    inputs <- inputs %>% pivot_wider(id_cols = NULL, names_from = Name, values_from = Value)
    
    model_name <- input$model_type
    model <- models[[model_name]]
    
    prediction <- predict(models[["Random Forest"]], inputs)
    
    paste0("The ", tolower(model_name), " model predicts that a bean with the supplied feature values is a ", as.character(prediction[[1]]), " bean")
    
  })
  
}


# Create and launch the Shiny app
shinyApp(ui, server)



