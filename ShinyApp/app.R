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
ui <- navbarPage("Team 5",
                 theme = shinytheme('spacelab'),
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
                                                         label = "Independent Variables:",
                                                         choices = ""),
                                      selectInput(inputId = "bandwidthtfGwr",
                                                  label = "Bandwidth:",
                                                  choices = list("Fixed" = "Fixed",
                                                                 "Adaptive" = "Adaptive"),
                                                  selected = "Fixed"),
                                      selectInput(inputId = "approachGwr",
                                                  label = "Approach:",
                                                  choices = list("Cross Validation" = "CV",
                                                                 "AIC" = "AIC"),
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
                                                  label = "Distances:",
                                                  choices = list("Euclidean" = "Euclidean",
                                                                 "Great Circle" = "Great Circle"),
                                                  selected = "Euclidean"),
                                      submitButton("Apply changes"),
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
                 tabPanel("GWR - Predict",
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
                                                         label = "Independent Variables:",
                                                         choices = ""),
                                      selectInput(inputId = "bandwidthtf",
                                                  label = "Bandwidth:",
                                                  choices = list("Fixed" = "Fixed",
                                                                 "Adaptive" = "Adaptive"),
                                                  selected = "Fixed"),
                                      selectInput(inputId = "approach",
                                                  label = "Approach:",
                                                  choices = list("Cross Validation" = "CV",
                                                                 "AIC" = "AIC"),
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
                                                  label = "Distances:",
                                                  choices = list("Euclidean" = "Euclidean",
                                                                 "Great Circle" = "Great Circle"),
                                                  selected = "Euclidean"),
                                      submitButton("Apply changes"),
                                      checkboxInput(inputId = "showData",
                                                    label = "Show data table",
                                                    value = TRUE)
                                  ),
                                  mainPanel(
                                      tabsetPanel(
                                          tabPanel("Formula", verbatimTextOutput("formula")),
                                          tabPanel("Summary", verbatimTextOutput("mlrsummary")),
                                          tabPanel("Multicollinearity", verbatimTextOutput("olsviftol")),
                                          tabPanel("Prediction", verbatimTextOutput("bw")),
                                          tabPanel("Visualization", tmapOutput("visualization")),
                                          tabPanel("Data Table", DT::dataTableOutput("aTable"))
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
                    border.col = "gray60",
                    border.lwd = 1) +
            tm_view(set.zoom.limits = c(10,15)) +
            tm_basemap("OpenStreetMap")
    })
    
    observe({
        updateSelectInput(session, "dependentGwr", label = "Dependent Variable:", choices = names(uploaded_data()))
        updateCheckboxGroupInput(session, "independentGwr", label = "Independent Variables:", choices = names(uploaded_data()))
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
                    border.lwd = 1) +
            tm_view(set.zoom.limits = c(10,15)) +
            tm_basemap("OpenStreetMap")
    })
    
    observe({
        updateSelectInput(session, "dependent", label = "Dependent Variable:", choices = names(uploaded_data()))
        updateCheckboxGroupInput(session, "independent", label = "Independent Variables:", choices = names(uploaded_data()))
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
