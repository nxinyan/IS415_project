#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(sf)
library(tmap)
library(tidyverse)
library(readr)
library(olsrr)
library(GWmodel)
library(corrplot)
library(shinythemes)
options(shiny.maxRequestSize = 30*1024^2)

# Define UI for application that draws a histogram
ui <- navbarPage("Regressors",
                 theme = shinytheme('spacelab'),
                 tabPanel("About Us",
                            fluidPage(
                                sidebarLayout(
                                    sidebarPanel(
                                        img(src='logo.jpg', align = "center"),
                                        h2(strong("GWR by Team 5")),
                                        tags$ul(
                                            tags$li("Toh Jun Long", style = "font-size:150%"),
                                            tags$li("Kwek Yi Chen", style = "font-size:150%"),
                                            tags$li("Ngah Xin Yan", style = "font-size:150%")
                                        ),
                                        h2(strong("Guided by:")),
                                        tags$li("Professor Kam Tin Seong (SMU IS415)", style = "font-size:150%"),
                                        width = 3
                                    ),
                                    mainPanel(
                                        h2(strong("Project Motivation")),
                                        hr(),
                                        p("COVID-19 has become an indisputable part of our daily life ever since the virus spread to the majority of the world. Some countries have been able to keep the situation under control, however, there are some that suffered more devastating effects from it. India and Indonesia are 2 countries in Asia that have the highest COVID related mortality and positive rates for COVID-19 cases, especially so in the main capital of the respective countries, likely due to certain underlying common factors within the countries.

Researchers claims that Indonesia’s capital, Jakarta, could have as many as 4.7 million people who are possibly infected by the virus in March 2021. This is alarming as this number constitutes to “nearly half” of Jakarta’s population.
                                          
We realize the importance of using Geographically Weighted Regression (GWR) to investigate the impact independent variables has on  a dependent variable.", style = "font-size:150%"),
                                        h2(strong("Objective")),
                                        hr(),
                                        p("This application aims to help users to flexibly import their own data and identify the relationship between their selected independent variables, such as proximity to Healthcare facilities and proximity to attraction, and dependent variable, such as Number of positive COVID cases.", style = "font-size:150%"),
                                        h2(strong("App Functions")),
                                        hr(),
                                        tags$ol(
                                            tags$li(strong("Exploratory Data Analysis (EDA):"), "Visualizing the different variables on Choropleth map and Histogram.", style = "font-size:150%"),
                                            tags$li(strong("GWR: "),"Build a GWR model based on selected dependent and independent variables, provides analysis on their relationship, thus allowing users to select the best parameters for the GWR base model. Additionally, users are able to visualize the accuracy of the model geographically.", style = "font-size:150%"),
                                            tags$li(strong("GWR Prediction:"),"A GWR prediction model is built based on selected dependent and independent variables with the dataset provided. The output is the predicted values which will be analyzed and visualized geographically on the interactive map.", style = "font-size:150%")
                                        )
                                    )
                                )
                                
                            )),
                 tabPanel("EDA",
                            fluidPage(
                              sidebarLayout(
                                  sidebarPanel( 
                                      # Upload a file
                                      fileInput("file1Gwr", "Choose a CSV file",
                                              multiple = FALSE,
                                             accept = c(".csv")),
                                      textInput(
                                         inputId = "crsprojection",
                                         label = "CRS:",
                                          placeholder = "Enter CRS"),
                                      submitButton("Update CRS"),
                                      h5(),
                                      selectInput(inputId = "allvariables",
                                                  label = "Variable:",
                                                  choices = ""),
                                      h3("Choropleth Map"),
                                      selectInput(inputId = "classmethod",
                                                  label = "Classification Method:",
                                                  choices = list("Bclust" = "bclust",
                                                                 "Fisher" = "fisher",
                                                                 "Equal" = "equal",
                                                                 "Fixed" = "fixed",
                                                                 "Hclust" = "hclust",
                                                                 "Jenks" = "jenks",
                                                                 "Kmeans" = "kmeans",
                                                                 "Pretty" = "pretty",
                                                                 "Quantile" = "quantile",
                                                                 "sd" = "sd"),
                                                  selected = "jenks"),
                                      h3("Histogram"),
                                      sliderInput(inputId = "bins",
                                                  label = "Number of Bins:",
                                                  min = 5,
                                                  max = 20,
                                                  value = 10),
                                      selectInput(inputId = "fill",
                                                  label = "Histogram Fill:",
                                                  choices = list("Blue" = "light blue",
                                                                 "Red" = "red",
                                                                 "Green" = "green",
                                                                 "Yellow" = "yellow",
                                                                 "Purple" = "purple"),
                                                  selected = "Blue"),
                                      submitButton("Run"),
                                      checkboxInput(inputId = "showData",
                                                    label = "Show data table",
                                                    value = TRUE)
                                  ),
                                  mainPanel(
                                      tabsetPanel(
                                          tabPanel("Choropleth Map", tmapOutput("choromap")),
                                          tabPanel("Histogram", plotOutput("histogram")),
                                          tabPanel("Data Table", DT::dataTableOutput(outputId = "aTableEDA"))
                                      )
                                  )
                               )
                            )  
                          ),
                 tabPanel("GWR",
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      
                                      # Upload a file
                                      # fileInput("file1Gwr", "Choose a CSV file",
                                        #        multiple = FALSE,
                                         #       accept = c(".csv")),
                                      #textInput(
                                       #   inputId = "crsprojection",
                                        #  label = "CRS:",
                                         # placeholder = "Enter CRS"),
                                      #submitButton("Update CRS"),
                                      selectInput(inputId = "dependentGwr",
                                                  label = "Dependent Variable:",
                                                  choices = ""),
                                      checkboxGroupInput(inputId = "independentGwr",
                                                         label = "Independent Variables (Select 2 or more):",
                                                         choices = ""),
                                      h3("Correlation Plot"),
                                      selectInput(inputId = "corrorderGwr",
                                                  label = "Order:",
                                                  choices = list("AOE" = "AOE",
                                                                 "FPC" = "FPC",
                                                                 "hclust" = "hclust",
                                                                 "alphabet" = "alphabet"),
                                                  selected = "AOE"),
                                      selectInput(inputId = "corrmethodGwr",
                                                  label = "Method:",
                                                  choices = list("circle" = "circle",
                                                                 "square" = "square",
                                                                 "ellipse" = "ellipse",
                                                                 "number" = "number",
                                                                 "shade" = "shade",
                                                                 "color" = "color",
                                                                 "pie" = "pie"),
                                                  selected = "circle"),
                                      selectInput(inputId = "corrtypeGwr",
                                                  label = "Type:",
                                                  choices = list("full" = "full",
                                                                 "upper" = "upper",
                                                                 "lower" = "lower"),
                                                  selected = "full"),
                                      h3("Model"),
                                      selectInput(inputId = "bandwidthtfGwr",
                                                  label = "Bandwidth:",
                                                  choices = list("Fixed" = "Fixed",
                                                                 "Adaptive" = "Adaptive"),
                                                  selected = "Fixed"),
                                      selectInput(inputId = "approachGwr",
                                                  label = "Approach:",
                                                  choices = list("Cross Validation (CV)" = "CV",
                                                                 "Akaike Information Criterion (AIC)" = "AIC"),
                                                  selected = "CV"),
                                      selectInput(inputId = "kernelGwr",
                                                  label = "Kernel:",
                                                  choices = list("Gaussian" = "gaussian",
                                                                 "Exponential" = "exponential",
                                                                 "Bisquare" = "bisquare",
                                                                 "Tricube" = "tricube",
                                                                 "Boxcar" = "boxcar"),
                                                  selected = "gaussian"),
                                      selectInput(inputId = "longlattfGwr",
                                                  label = "Distance metric:",
                                                  choices = list("Euclidean" = "Euclidean",
                                                                 "Great Circle" = "Great Circle"),
                                                  selected = "Euclidean"),
                                      submitButton("Run"),
                                      checkboxInput(inputId = "showDataGwr",
                                                    label = "Show data table",
                                                    value = TRUE)
                                  ),
                                  mainPanel(
                                      tabsetPanel(
                                          tabPanel("Formula", verbatimTextOutput("formulaGwr")),
                                          tabPanel("Correlation Plot", plotOutput("corrGwr")),
                                          tabPanel("Summary", verbatimTextOutput("mlrsummaryGwr")),
                                          tabPanel("Multicollinearity", verbatimTextOutput("olsviftolGwr")),
                                          tabPanel("Linearity", plotOutput("olsplotresidfitGwr")),
                                          tabPanel("Normality", plotOutput("olsplotresidhistGwr")),
                                          tabPanel("Base Model's Performance", verbatimTextOutput("bwGwr")),
                                          tabPanel("Visualization", tmapOutput("visualizationGwr")),
                                          tabPanel("Data Table", DT::dataTableOutput("aTableGwr"))
                                      )
                                  )
                              )
                          )
                 ),
                 tabPanel("GWR Prediction",
                          fluidPage(
                              sidebarLayout(
                                  sidebarPanel(
                                      
                                      # # Upload a file
                                      # fileInput("file1", "Choose a CSV file",
                                      #           multiple = FALSE,
                                      #           accept = c(".csv")),
                                      # textInput(
                                      #     inputId = "crsprojection",
                                      #     label = "CRS:",
                                      #     placeholder = "Enter CRS"),
                                      # submitButton("Update CRS"),
                                      selectInput(inputId = "dependent",
                                                  label = "Dependent Variable:",
                                                  choices = ""),
                                      checkboxGroupInput(inputId = "independent",
                                                         label = "Independent Variables (Select 2 or more):",
                                                         choices = ""),
                                      h3("Model"),
                                      selectInput(inputId = "bandwidthtf",
                                                  label = "Bandwidth:",
                                                  choices = list("Fixed" = "Fixed",
                                                                 "Adaptive" = "Adaptive"),
                                                  selected = "Fixed"),
                                      selectInput(inputId = "approach",
                                                  label = "Approach:",
                                                  choices = list("Cross Validation (CV)" = "CV",
                                                                 "Akaike Information Criterion (AIC)" = "AIC"),
                                                  selected = "CV"),
                                      selectInput(inputId = "kernel",
                                                  label = "Kernel:",
                                                  choices = list("Gaussian" = "gaussian",
                                                                 "Exponential" = "exponential",
                                                                 "Bisquare" = "bisquare",
                                                                 "Tricube" = "tricube",
                                                                 "Boxcar" = "boxcar"),
                                                  selected = "gaussian"),
                                      selectInput(inputId = "longlattf",
                                                  label = "Distance metric:",
                                                  choices = list("Euclidean" = "Euclidean",
                                                                 "Great Circle" = "Great Circle"),
                                                  selected = "Euclidean"),
                                      submitButton("Run"),
                                      checkboxInput(inputId = "showData",
                                                    label = "Show data table",
                                                    value = TRUE)
                                  ),
                                  mainPanel(
                                      tabsetPanel(
                                          tabPanel("Formula", verbatimTextOutput("formula"),
                                                   HTML("Tips:
                                                        <br/> This formula is based on the selected dependent and independent variables.")),
                                          tabPanel("Summary", verbatimTextOutput("mlrsummary"), 
                                                   HTML("Tips: 
                                                        <br/> Remove independent variables without asterisk because they are not statistically significant.
                                                        <br/> R-squared shows how much the model is able to explain the dependent variable.
                                                        <br/> Example: R-squared of 0.5 means the model is able to explain 50% of the dependent variable.
                                                        <br/> P-value > significance level = mean is good estimator of the dependent variable.
                                                        <br/> P-value < significance level = model is good estimator of the dependent variable.")),
                                          tabPanel("Prediction Model's Performance", verbatimTextOutput("bw"),
                                                   HTML("Tips: 
                                                        <br/> This shows the min, median and max of the predicted value of the dependent variable.")),
                                          tabPanel("Visualization", tmapOutput("visualization"),
                                                   HTML("Tips: 
                                                        <br/> This shows the predicted value of the dependent variable geographically.
                                                        <br/> Dark red means a higher predicted value of the dependent variable.
                                                        <br/> Light yellow means a lower predicted value of the dependent variable.")),
                                          tabPanel("Data Table", DT::dataTableOutput("aTable"),
                                                   HTML("Tips: 
                                                        <br/> This shows data of the selected dependent and independent variables."))
                                      )
                                  )
                              )
                          )
                 )
)

server <- function(input, output, session) {
    
    initialdf <- data.frame(names=c("Upload","a","File"))
    
    uploaded_data <- reactive({
        # if(!(is.null(req(input$file1Gwr)))) {
        #     req(input$file1Gwr)
        #     inFile <- input$file1Gwr
        # } else if (!(is.null(req(input$file1)))) {
        #     req(input$file1)
        #     inFile <- input$file1
        # }
        # if(!(is.null(req(input$crsprojection)))) {
        #     crsValue = as.numeric(req(input$crsprojection))
        # } else if (!(is.null(req(input$crsprojectionGwr)))) {
        #     crsValue = as.numeric(req(input$crsprojectionGwr))
        #     print(crsValue)
        # }
        req(input$file1Gwr)
        inFile <- input$file1Gwr
        if (is.null(inFile)) {
            uploaded_data <- initialdf
        } else {
            dkijkt_covid <- read_csv(inFile$datapath)
            if ("X" %in% colnames(dkijkt_covid)) {
                dkijkt_covid_sf <- st_as_sf(dkijkt_covid,
                                            coords = c("X", "Y"),
                                            crs=as.numeric(req(input$crsprojection)))
            } else if ("LAT" %in% colnames(dkijkt_covid)) {
                dkijkt_covid_sf <- st_as_sf(dkijkt_covid,
                                            coords = c("LONG", "LAT"),
                                            crs=4326) %>% 
                    st_transform(crs=as.numeric(req(input$crsprojection)))
            }
        }
    })
    
    uploaded_data_original <- reactive({
        req(input$file1Gwr)
        inFile <- input$file1Gwr
        if (is.null(inFile)) {
            uploaded_data <- initialdf
        } else {
            dkijkt_covid <- read_csv(inFile$datapath)
        }
    })
    
    output$aTableEDA <- DT::renderDataTable({
        input$submitButton
        
        req(input$allvariables)
        
        if(input$showData){
            req(input$allvariables)
            DT::datatable(data = uploaded_data() %>%
                              dplyr::select(input$allvariables),
                          options= list(pageLength = 5),
                          rownames = FALSE)
        }
    })
    
    output$choromap <- renderTmap({
        req(input$allvariables)
        
        input$submitButton
        covid_sf <- uploaded_data()
        tmap_mode("view")
        tm_shape(covid_sf) +  
            tm_dots(col = input$allvariables,
                    alpha = 0.6,
                    style=input$classmethod) +
            tm_view(set.zoom.limits = c(10,15))+
            tm_basemap("OpenStreetMap")
    })
    
    output$histogram <- renderPlot({
        req(input$allvariables)
        req(input$bins)
        req(input$fill)
        
        columndata <- as.numeric(uploaded_data()[[input$allvariables]])
        column_name <- as.character(input$allvariables)
        
        ggplot(uploaded_data(), aes(x = columndata)) + 
            geom_histogram(bins = input$bins, color="black", fill=input$fill) +
            labs(title = paste("Histogram of", column_name), x=column_name)
    })
    
    observe({
        updateSelectInput(session, "allvariables", label = "Variable:", choices = names(uploaded_data()))
    })
    
    output$aTableGwr <- DT::renderDataTable({
        if(input$showDataGwr){
            req(input$dependentGwr)
            req(input$independentGwr)
            
            DT::datatable(data = uploaded_data() %>%
                              dplyr::select(input$dependentGwr, input$independentGwr),
                          options= list(pageLength = 5),
                          rownames = FALSE)
        }
    })
    
    output$formulaGwr <- renderText({
        req(input$dependentGwr)
        req(input$independentGwr)
        
        formula = paste(input$dependentGwr, "~", paste(input$independentGwr, collapse= " + "))
        paste("Formula: ", formula)
    })
    
    output$corrGwr <- renderPlot({
        req(input$independentGwr)
        req(input$corrorderGwr)
        req(input$corrmethodGwr)
        req(input$corrtypeGwr)
        
        
        col <- input$independentGwr
        
        selecteddata <- uploaded_data_original() %>% dplyr::select(input$independentGwr)
        selecteddata[col] <- sapply(selecteddata[col], as.numeric)
        
        corrplot(cor(selecteddata), diag = TRUE, order = input$corrorderGwr,
                  tl.cex = 0.8, method = input$corrmethodGwr, type = input$corrtypeGwr)
    })
    
    mlrsummaryGwr <- reactive({
        req(input$dependentGwr)
        req(input$independentGwr)
        
        formula_reactive = as.formula(paste(input$dependentGwr, " ~ ", paste(input$independentGwr, collapse= "+")))
        summary(stats::lm(formula=formula_reactive, data=uploaded_data()))
    })
    
    output$mlrsummaryGwr <- renderPrint({
        mlrsummaryGwr()
    })
    
    olsviftolGwr <- reactive({
        req(input$dependentGwr)
        req(input$independentGwr)
        
        formula_reactive = as.formula(paste(input$dependentGwr, " ~ ", paste(input$independentGwr, collapse= "+")))
        ols_vif_tol(stats::lm(formula=formula_reactive, data=uploaded_data()))
    })
    
    output$olsviftolGwr <- renderPrint({
        olsviftolGwr()
    })
    
    olsplotresidfitGwr <- reactive({
        req(input$dependentGwr)
        req(input$independentGwr)
        
        formula_reactive = as.formula(paste(input$dependentGwr, " ~ ", paste(input$independentGwr, collapse= "+")))
        ols_plot_resid_fit(stats::lm(formula=formula_reactive, data=uploaded_data()))
    })
    
    output$olsplotresidfitGwr <- renderPlot({
        olsplotresidfitGwr()
    })
    
    olsplotresidhistGwr <- reactive({
        req(input$dependentGwr)
        req(input$independentGwr)
        
        formula_reactive = as.formula(paste(input$dependentGwr, " ~ ", paste(input$independentGwr, collapse= "+")))
        ols_plot_resid_hist(stats::lm(formula=formula_reactive, data=uploaded_data()))
    })
    
    output$olsplotresidhistGwr <- renderPlot({
        olsplotresidhistGwr()
    })
    
    bwGwr <- reactive ({
        input$submitbutton
        
        req(input$dependentGwr)
        req(input$independentGwr)
        req(input$approachGwr)
        req(input$kernelGwr)
        
        
        formula_reactive = as.formula(paste(input$dependentGwr, " ~ ", paste(input$independentGwr, collapse= "+")))
        mlr <- stats::lm(formula=formula_reactive, data=uploaded_data())
        
        mlr_output <- as.data.frame(mlr$residuals)
        covid_sf <- cbind(uploaded_data(), mlr_output) %>%
            rename(`MLR_RES` = `mlr.residuals`)
        covid_sp <- as_Spatial(covid_sf)
        
        # issue with getting inputs from bandwidth and longlat
        
        if (input$bandwidthtfGwr == "Fixed") {
            
            if (input$longlattfGwr == "Euclidean"){
                bw_method <- bw.gwr(formula=formula_reactive, data=covid_sp, 
                                    approach=input$approachGwr, kernel=input$kernelGwr, 
                                    adaptive=FALSE, longlat=FALSE)
                
                
                gwr_method <- gwr.basic(formula=formula_reactive, data=covid_sp, 
                                        bw=bw_method, kernel=input$kernelGwr, 
                                        adaptive=FALSE, longlat=FALSE)
            }
            
            else {
                bw_method <- bw.gwr(formula=formula_reactive, data=covid_sp, 
                                    approach=input$approachGwr, kernel=input$kernelGwr, 
                                    adaptive=FALSE, longlat=TRUE)
                
                
                gwr_method <- gwr.basic(formula=formula_reactive, data=covid_sp, 
                                        bw=bw_method, kernel=input$kernelGwr, 
                                        adaptive=FALSE, longlat=TRUE)
            }
        }
        
        else {
            
            if (input$longlattfGwr == "Euclidean") {
                bw_method <- bw.gwr(formula=formula_reactive, data=covid_sp, 
                                    approach=input$approachGwr, kernel=input$kernelGwr,
                                    adaptive=TRUE, longlat=FALSE)
                
                
                gwr_method <- gwr.basic(formula=formula_reactive, data=covid_sp, 
                                        bw=bw_method, kernel=input$kernelGwr, 
                                        adaptive=TRUE, longlat=FALSE)
            }
            
            else {
                bw_method <- bw.gwr(formula=formula_reactive, data=covid_sp, 
                                    approach=input$approachGwr, kernel=input$kernelGwr,
                                    adaptive=TRUE, longlat=TRUE)
                
                
                gwr_method <- gwr.basic(formula=formula_reactive, data=covid_sp, 
                                        bw=bw_method, kernel=input$kernelGwr, 
                                        adaptive=TRUE, longlat=TRUE)
            }
        }
        gwr_method
        
    })
    
    output$bwGwr <- renderPrint({
        bwGwr()
    })
    
    output$visualizationGwr <- renderTmap({
        input$submitbutton
        
        covid_sf <- st_as_sf(bwGwr()$SDF, crs=as.numeric(input$crsprojection))
        tmap_mode("view")
        tm_shape(covid_sf) +  
            tm_dots(col = "Local_R2",
                    border.lwd = 1,
                    size = 0.05) +
            tm_view(set.zoom.limits = c(10,15)) +
            tm_basemap("OpenStreetMap")
    })
    
    observe({
        updateSelectInput(session, "dependentGwr", label = "Dependent Variable:", choices = names(uploaded_data()))
        updateCheckboxGroupInput(session, "independentGwr", label = "Independent Variables (Select 2 or more):", choices = names(uploaded_data()))
    })
    
    # initialdf <- data.frame(names=c("Upload","a","File"))
    # 
    # uploaded_data <- reactive({
    #     req(input$file1)
    #     
    #     inFile <- input$file1
    #     if (is.null(inFile)) {
    #         uploaded_data <- initialdf
    #     } else {
    #         req(input$crsprojection)
    #         
    #         dkijkt_covid <- read_csv(inFile$datapath)
    #         # if lat in dd
    #         if ("X" %in% colnames(dkijkt_covid)) {
    #             dkijkt_covid_sf <- st_as_sf(dkijkt_covid,
    #                                         coords = c("X", "Y"),
    #                                         crs=as.numeric(input$crsprojection))
    #         } else if ("LAT" %in% colnames(dkijkt_covid)) {
    #             dkijkt_covid_sf <- st_as_sf(dkijkt_covid,
    #                                         coords = c("LONG", "LAT"),
    #                                         crs=4326) %>% 
    #                 st_transform(crs=as.numeric(input$crsprojection))
    #         }
    #     }
    # })
    
    output$aTable <- DT::renderDataTable({
        if(input$showData){
            req(input$dependent)
            req(input$independent)
            
            DT::datatable(data = uploaded_data() %>%
                              dplyr::select(input$dependent, input$independent),
                          options= list(pageLength = 5),
                          rownames = FALSE)
        }
    })
    
    output$formula <- renderText({
        req(input$dependent)
        req(input$independent)
        
        formula = paste(input$dependent, "~", paste(input$independent, collapse= " + "))
        paste("Formula: ", formula)
    })
    
    mlrsummary <- reactive({
        req(input$dependent)
        req(input$independent)
        
        formula_reactive = as.formula(paste(input$dependent, " ~ ", paste(input$independent, collapse= "+")))
        summary(stats::lm(formula=formula_reactive, data=uploaded_data()))
    })
    
    output$mlrsummary <- renderPrint({
        mlrsummary()
    })
    
    bw <- reactive ({
        input$submitbutton
        
        req(input$dependent)
        req(input$independent)
        req(input$approach)
        req(input$kernel)
        
        
        formula_reactive = as.formula(paste(input$dependent, " ~ ", paste(input$independent, collapse= "+")))
        mlr <- stats::lm(formula=formula_reactive, data=uploaded_data())
        
        mlr_output <- as.data.frame(mlr$residuals)
        covid_sf <- cbind(uploaded_data(), mlr_output) %>%
            rename(`MLR_RES` = `mlr.residuals`)
        covid_sp <- as_Spatial(covid_sf)
        
        # issue with getting inputs from bandwidth and longlat
        
        if (input$bandwidthtf == "Fixed") {
            
            if (input$longlattf == "Euclidean"){
                bw_method <- bw.gwr(formula=formula_reactive, data=covid_sp,
                                    approach=input$approach, kernel=input$kernel,
                                    adaptive=FALSE, longlat=FALSE)
                
                
                gwr_method <- gwr.predict(formula=formula_reactive, data=covid_sp,
                                          bw=bw_method, kernel=input$kernel,
                                          adaptive=FALSE, longlat=FALSE)
            }
            
            else {
                bw_method <- bw.gwr(formula=formula_reactive, data=covid_sp,
                                    approach=input$approach, kernel=input$kernel,
                                    adaptive=FALSE, longlat=TRUE)
                
                
                gwr_method <- gwr.predict(formula=formula_reactive, data=covid_sp,
                                          bw=bw_method, kernel=input$kernel,
                                          adaptive=FALSE, longlat=TRUE)
            }
        }
        
        else {
            
            if (input$longlattf == "Euclidean") {
                bw_method <- bw.gwr(formula=formula_reactive, data=covid_sp,
                                    approach=input$approach, kernel=input$kernel,
                                    adaptive=TRUE, longlat=FALSE)
                
                
                gwr_method <- gwr.predict(formula=formula_reactive, data=covid_sp,
                                          bw=bw_method, kernel=input$kernel,
                                          adaptive=TRUE, longlat=FALSE)
            }
            
            else {
                bw_method <- bw.gwr(formula=formula_reactive, data=covid_sp,
                                    approach=input$approach, kernel=input$kernel,
                                    adaptive=TRUE, longlat=TRUE)
                
                
                gwr_method <- gwr.predict(formula=formula_reactive, data=covid_sp,
                                          bw=bw_method, kernel=input$kernel,
                                          adaptive=TRUE, longlat=TRUE)
            }
        }
        gwr_method
        
    })
    
    output$bw <- renderPrint({
        bw()
    })
    
    output$visualization <- renderTmap({
        input$submitbutton
        
        covid_sf <- st_as_sf(bw()$SDF, crs=as.numeric(input$crsprojection))
        
        tmap_mode("view")
        tm_shape(covid_sf) +
            tm_dots(col = "prediction",
                    border.lwd = 1,
                    size = 0.05) +
            tm_view(set.zoom.limits = c(10,15)) +
            tm_basemap("OpenStreetMap")
    })
    
    observe({
        updateSelectInput(session, "dependent", label = "Dependent Variable:", choices = names(uploaded_data()))
        updateCheckboxGroupInput(session, "independent", label = "Independent Variables (Select 2 or more):", choices = names(uploaded_data()))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
