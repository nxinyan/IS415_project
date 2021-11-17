library(shiny)
library(tidyverse)

exam <- read_csv("data/Exam_data.csv")

ui <- fluidPage(
    titlePanel("Pupils Examination Results Dashboard"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "variable",
                        label = "Subject:",
                        choices = c("English" = "ENGLISH",
                                    "Maths" = "MATHS",
                                    "Science" = "SCIENCE"),
                        selected = "ENGLISH"),
            sliderInput(inputId = "bins",
                        label = "Number of Bins",
                        min = 5,
                        max = 20,
                        value = 10),
            checkboxInput(inputId = "show_data", 
                          label = "Show data table",
                          value = TRUE) 
        ),
        mainPanel(
            plotOutput(outputId = "distPlot"),
            DT::dataTableOutput(outputId = "examtable")
        )
        
    )
)

server <- function(input, output){
    output$distPlot <- renderPlot({
        x <- unlist(exam[,input$variable])
        ggplot(exam, aes(x)) + 
            geom_histogram(bins = input$bins,
                           color = "black",
                           fill = "light blue")
        
    })
}

shinyApp(ui = ui, server = server)
