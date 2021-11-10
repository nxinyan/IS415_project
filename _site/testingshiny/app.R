library(shiny)
library(readxl)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Team 5"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            
            # Upload a file
            fileInput("file1", "Choose an Excel file",
            multiple = FALSE,
            accept = c(".xlsx")),
            checkboxGroupInput("variables",
                                "Variables:",
                                choices = ""),
            selectInput("weighting_function",
                        "Weighting Function:",
                        choices = list("blues" = "Blues", 
                                       "reds" = "Reds", 
                                       "greens" = "Greens",
                                       "Yellow-Orange-Red" = "YlOrRd",
                                       "Yellow-Orange-Brown" = "YlOrBr",
                                       "Yellow-Green" = "YlGn",
                                       "Orange-Red" = "OrRd")),
            selectInput("weighting_scheme",
                        "Weighting Scheme:",
                        choices = list("blues" = "Blues", 
                                       "reds" = "Reds", 
                                       "greens" = "Greens",
                                       "Yellow-Orange-Red" = "YlOrRd",
                                       "Yellow-Orange-Brown" = "YlOrBr",
                                       "Yellow-Green" = "YlGn",
                                       "Orange-Red" = "OrRd")),
            selectInput("bandwidth",
                        "Bandwidth:",
                        choices = list("blues" = "Blues", 
                                       "reds" = "Reds", 
                                       "greens" = "Greens",
                                       "Yellow-Orange-Red" = "YlOrRd",
                                       "Yellow-Orange-Brown" = "YlOrBr",
                                       "Yellow-Green" = "YlGn",
                                       "Orange-Red" = "OrRd"))
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
           tableOutput("contents")
        )
    )
)

server <- function(input, output, session) {
    
    initialdf <- data.frame(names=c("Upload","a","File"))
    
    uploaded_data <- reactive({
        req(input$file1)
        inFile <- input$file1
        if (is.null(inFile)) {
            uploaded_data <- initialdf
        } else {
            uploaded_data <- read_excel(inFile$datapath)
        }
    })
    
    output$contents <- renderTable({
        uploaded_data()
    })
    
    observe({
        variables <- names(uploaded_data())
        options <- list()
        options[variables] <- variables
        listofvariables <- colnames(uploaded_data)
        updateCheckboxGroupInput(session, "variables", label = "Variables:", choices = options)
    })

}

# Run the application 
shinyApp(ui = ui, server = server)
