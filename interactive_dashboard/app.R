############# Initialization and background code ###############################

# Load required packages
rm(list = ls())
library(tidymodels)
library(beans)
library(bundle)
library(shiny)
library(tidyverse)
library(ranger)
library(stringr)
library(cowplot)
library(xgboost)
library(kernlab)
library(grid)
library(shinythemes)

# Define utility function for application
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


# Read in machine learning models and save in list 
model_names <- c("Random Forest" = "RF",
                 "Lasso Regression" = "Lasso",
                 "XGBoost" = "XG",
                 "Lasso on principle components" = "Lasso_PCA",
                 "Support Vector Machine" = "SVM",
                 "Ensemble prediction using all models" = "RF_XG_Lasso_LassoPCA_SVM")

models <- list()

for (model in model_names){
  
  load(paste0(model, "_predict.RData"))
  
  # switch names as needed (there are inconsistencies between file names and bundle names)
  model <- ifelse(model == "RF", "rf",
                  ifelse(model == "Lasso","lasso", 
                         ifelse(model == "RF_XG_Lasso_LassoPCA_SVM", "ensemble_object_5_2", 
                                ifelse(model == "SVM", "svm", 
                                       ifelse(model == "XG", "xg", 
                                              ifelse(model == "Lasso_PCA", "lasso_PCA", model))))))
  
  if (model != "ensemble_object_5_2") {
    model_object <- unbundle(get(paste0(model, "_bundle")))
  } else {
    
  }
  
  
  ensemble_object_5_2 <-  
  
  # switch names back as needed (there are inconsistencies between file names and bundle names)
  model <- ifelse(model == "rf", "RF", 
                  ifelse(model == "lasso","Lasso", 
                         ifelse(model == "ensemble_object_5_2","RF_XG_Lasso_LassoPCA_SVM", 
                                ifelse(model == "svm", "SVM", 
                                       ifelse(model == "xg", "XG", 
                                              ifelse(model == "lasso_PCA", "Lasso_PCA", model))))))
  
  # Save unbundled model objesct into list
  models[[names(model_names)[which(model_names == model)]]] <- model_object
}

# Get ranges of features for slider inputs in dashboard
data <- beans
names(data)

beans_summary_stats <- data.frame(cbind(t(beans %>% select(-"class") %>% summarise_all(min)), 
                             t(beans %>% select(-"class") %>% summarise_all(max)),
                             t(beans %>% select(-"class") %>% summarise_all(.funs = function(x) min(x)/2 + max(x)/2))))

names(beans_summary_stats) <- c("min", "max", "mid")
beans_summary_stats <- cbind(data.frame("var" = rownames(beans_summary_stats)),
                             beans_summary_stats)

rownames(beans_summary_stats) <- NULL

bean_names <- sort(unique(data$class))
names(bean_names) <- c("Barbunya","Bombay","Cali","Dermason","Horoz","Seker","Sira")
  
col_scale <- scale_color_manual(values = c("barbunya" = "#1B9E77",
                                          "bombay" =  "#D95F02",
                                          "cali" = "#7570B3",
                                          "dermason" = "#E7298A",
                                          "horoz" = "#66A61E",
                                          "seker" = "#E6AB02",
                                          "sira" = "#A6761D"),
                                
                                labels = names(bean_names))

fill_scale <- scale_fill_manual(values = c("barbunya" = "#1B9E77",
                                            "bombay" =  "#D95F02",
                                            "cali" = "#7570B3",
                                            "dermason" = "#E7298A",
                                            "horoz" = "#66A61E",
                                            "seker" = "#E6AB02",
                                            "sira" = "#A6761D"),
                                
                                labels = names(bean_names))

# Save summaries of features 
vars <- beans_summary_stats$var
vars_pretty <- sapply(X = vars, FUN = function(X) get_var_longname(X))
names(vars) <- vars_pretty

# Save training and test dataset 
set.seed(250)
bean_split = initial_split(beans, prop = 0.8, strata = class)
bean_training <- training(bean_split)
bean_testing <- testing(bean_split)


################################### APP ########################################

# Define Shiny UI here
ui <- fluidPage(theme = shinytheme("paper"),
  
  # App title 
  titlePanel("Machine learning models for bean classification"),
  
  navbarPage("",
             
             tabPanel("About", 
                      
                      mainPanel(
                        includeHTML("about.html")
                      )),
                      
             
             tabPanel("Explore dataset and model behaviour", 
                      
                      sidebarLayout(
                        
                        # Sidebar panel for inputs ----
                        sidebarPanel(
                          
                          # Input: Selector for model
                          selectInput(inputId = "data_plot.x",
                                      label = "Choose a variable to plot on x axis:",
                                      choices = vars),
                          
                          selectInput(inputId = "data_plot.y",
                                      label = "Choose a variable to plot on y axis:",
                                      selected = "perimeter",
                                      choices = vars),
                          
                        # Input: sliders for variable value choices
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
                          h1(strong("Data visualisation"), style = "font-size:20px;"),
                          strong(textOutput("plot_title")),
                          "Click on the scatter plot or histograms to set the feature values!",
                          fluidRow(
                            column(6, plotOutput(outputId = "beans_legend", width  = "350px",height = "350px")),
                            column(6, plotOutput(outputId = "histogram_x", width  = "350px",height = "350px",click = "histx_click"))
                          ),
                          
                          fluidRow(
                            column(6, plotOutput(outputId = "histogram_y", width  = "350px",height = "350px", click = "histy_click")),
                            column(6, plotOutput(outputId = "scatter_plot", width  = "350px",height = "350px", click = "plot_click"))
                            
                          ),
                          headerPanel(""),
                          "Change the x variable and the y variable using the drop-down menus on the left hand panel to explore the pairwise relationships between features!",
                          h1(strong("Machine learning model predictions"), style = "font-size:20px;"),
                          "The below table shows the agreement and disagreement between different models' predictions, for a bean with variable values as selected by the user.",
                          
                          plotOutput("model_prediction"))
                        )
                      ), 


            tabPanel("Predict for multiple beans",
                "This page allows a user to upload a file containing covariate values for multiple beans, and to download the dataset containing the model predictions.",
                "NB: the file can be at most 5 MB.",
                fileInput("upload", strong("Upload a file containing bean values"),buttonLabel = "Upload...", multiple = TRUE, accept = c(".csv", ".tsv")),
                strong("Select which machine learning models you want to apply to this dataset to predict bean class:"),
                checkboxInput("RF_checkbox", "Random Forest", FALSE),
                checkboxInput("Lasso_checkbox", "Lasso Regression", FALSE),
                checkboxInput("Lasso_PCA_checkbox","Lasso on principle components",  FALSE),
                checkboxInput("XGBoost_checkbox", "XGBoost", FALSE),
                checkboxInput("SVM_checkbox", "Support Vector Machine", FALSE),
                checkboxInput("ensemble_checkbox", "Ensemble using all models", FALSE),
                
                radioButtons("render_head_table", "Render first ten elements of dataframe and classifications?",
                             c("Render" = "y",
                               "Hide" = "n"),
                             selected = "n"),
                
                tableOutput("head"), 
                strong("Download predictions:"),
                downloadButton("download", "Download .csv")

)))



# Define Shiny server logic
server <- function(input, output, session){
  
  # Expression to create data frame of all input values
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
  

  checkboxValues <- reactive({
    
    data.frame(
      "Name" = c("Random Forest",
                 "Lasso Regression",
                 "XGBoost",
                 "Lasso on principle components",
                 "Support Vector Machine",
                 "Ensemble prediction using all models"),
      
      "Value" = c(input$RF_checkbox,
                  input$Lasso_checkbox,
                  input$Lasso_PCA_checkbox,
                  input$XGBoost_checkbox,
                  input$SVM_checkbox,
                  input$ensemble_checkbox))
    
  })
  
  # Create plot title
  output$plot_title <- renderText({
    str_to_sentence(paste0(get_var_longname(input$data_plot.y), " against ", get_var_longname(input$data_plot.x), ", by bean class"))
    })
  
  # Create scatter plot
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
      col_scale +
      theme_classic() +
      geom_point(inherit.aes = F, colour = "red", x = user_xval, y = user_yval, shape = 4, size = 6) +
      labs(x = get_var_longname(input$data_plot.x), 
           y = get_var_longname(input$data_plot.y)) +
      theme(legend.position = "none")
    
  })
  
  # Create histogram of feature on x axis of scatter plot
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
      fill_scale +
      geom_vline(xintercept = user_xval, colour = "red") +
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
  
  # Create histogram of feature on y axis of scatter plot
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
      fill_scale +
      geom_vline(xintercept = user_yval, colour = "red") +
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
  
  # Create legend for plots
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
      fill_scale +
      geom_vline(xintercept = user_yval, colour = "red") +
      labs(title = paste0("Marginal distribution of ",get_var_longname(input$data_plot.x), ", by bean class"), 
           x = get_var_longname(input$data_plot.y),
           y = "Count", 
           fill = "Bean class") +
      coord_flip() +
      theme(legend.text = element_text(size = 14), 
            legend.title = element_text(size = 18))
      
    
    a <- get_legend(p)
    
    grid.draw(a)
    
  })
  
  # Plot model predictions as heatmap
  output$model_prediction <- renderPlot({
    
    inputs <- sliderValues() 
    
    inputs <- inputs %>% pivot_wider(id_cols = NULL, names_from = Name, values_from = Value)
    
    predictions <- c() 
    
    for (model in names(model_names)){
      
      predictions <- c(predictions, as.character(predict(models[[model]], inputs)[[1]]))
      
    }
    
    predictions_df <- data.frame("Model" = names(model_names),
                                "Classification" = predictions) 
    
    all_predictions_df <- data.frame("Model" = rep(names(model_names), length(bean_names)),
                                 "Classification" = rep(bean_names, each = length(model_names)),
                                 "pred" = rep(NA, length(bean_names) * length(model_names)))
    
    predictions_df$Classification <- factor(predictions_df$Classification, levels = bean_names)
    
    ggplot(all_predictions_df, aes(y = Model, x = Classification)) + 
      geom_tile(linewidth=0.5, colour ="black", fill = "white") + 
      geom_tile(data = predictions_df, aes(fill = Classification), line_width=0.5, colour ="black") + 
      fill_scale + 
      theme_classic() + 
      scale_x_discrete(labels = names(bean_names),position = "top") +
      labs(title = "Bean class prediction by model") +
      theme(legend.position = "none",
            axis.ticks = element_blank(),
            axis.text.y = element_text(size = 12),
            axis.text.x = element_text(size = 12),
            axis.title = element_blank(),
            title = element_text(size = 14)) + 
      
      theme(axis.line = element_blank())
    
  })
  
  # Update slider input based on plot click

  observeEvent(input$plot_click$x, {

    inputs <- sliderValues()
    updateSliderInput(session, input$data_plot.x, value = input$plot_click$x)
    
  })

  observeEvent(input$plot_click$y, {
    
    inputs <- sliderValues()
    updateSliderInput(session, input$data_plot.y, value = input$plot_click$y)
    
  })
  
  observeEvent(input$histx_click$x, {
    
    inputs <- sliderValues()
    updateSliderInput(session, input$data_plot.x, value = input$histx_click$x)
    
  })
  
  observeEvent(input$histy_click$y, {
    
    inputs <- sliderValues()
    updateSliderInput(session, input$data_plot.y, value = input$histy_click$y)
    
  })
  
  # get data 
  
  data <- reactive({
    req(input$upload)
    
    ext <- tools::file_ext(input$upload$name)
    
    switch(ext,
           csv = vroom::vroom(input$upload$datapath, delim = ","),
           tsv = vroom::vroom(input$upload$datapath, delim = "\t"),
           validate("Invalid file; Please upload a .csv or .tsv file")
    )
    
  })
  
  data_predicted <- reactive({
    
    models_pred_index <- checkboxValues()
    
    data_predicted <- data()
    
    for (model in models_pred_index$Name[which(models_pred_index$Value == T)]){
      data_predicted[,model] <- as.character(predict(models[[model]], data_predicted)[[1]])
    }
    
    data_predicted
    
  })
  
  
  output$head <- renderTable({
    
    if (input$render_head_table == "y"){
      
      head(data_predicted(), 10)
    }
    
  })
  
  output$download <- downloadHandler(
    
    filename = function() {
      paste0(input$dataset, "predictions.csv")
    },
    
    content = function(file) {
      write.csv(data(), file)
    }
  )
  
}

# Create and launch the Shiny app 
shinyApp(ui, server)



