library(shiny)
library(tidyverse)
library(tools)
library(sf)
library(tmap)
#library(readr)
library(shinythemes)
library(base)

options(shiny.maxRequestSize = 30*1024^2)

ui <- fluidPage(
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

server <- function(input, output, session){
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
}
    

shinyApp(ui=ui,server=server)