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
                 tabPanel("EDA"),
                 tabPanel("GWR",
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
                                      selectInput(inputId = "dependentGwr",
                                                  label = "Dependent Variable:",
                                                  choices = ""),
                                      checkboxGroupInput(inputId = "independentGwr",
                                                         label = "Independent Variables (Select 2 or more):",
                                                         choices = ""),
                                      selectInput(inputId = "bandwidthtfGwr",
                                                  label = "Bandwidth:",
                                                  choices = list("Fixed" = "Fixed",
                                                                 "Adaptive" = "Adaptive"),
                                                  selected = "Fixed"),
                                      selectInput(inputId = "approachGwr",
                                                  label = "Approach:",
                                                  choices = list("Cross Validation (CV)" = "CV",
                                                                 "Akaike information Criterion (AIC)" = "AIC"),
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
                                          tabPanel("Summary", verbatimTextOutput("mlrsummaryGwr")),
                                          tabPanel("Multicollinearity", verbatimTextOutput("olsviftolGwr")),
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
                                          tabPanel("Multicollinearity", verbatimTextOutput("olsviftol"),
                                                   HTML("Tips: 
                                                        <br/> Consider removing independent variables with VIF > 10 because it means that there is a sign of multicollinearity.")),
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
    
    olsviftol <- reactive({
        req(input$dependent)
        req(input$independent)
        
        formula_reactive = as.formula(paste(input$dependent, " ~ ", paste(input$independent, collapse= "+")))
        ols_vif_tol(stats::lm(formula=formula_reactive, data=uploaded_data()))
    })
    
    output$olsviftol <- renderPrint({
        olsviftol()
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
