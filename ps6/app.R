# Load required packages
library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(rsconnect)
library(shiny)
library(tidyverse)

# Load the dataset
uah_data <- read.delim("UAH-lower-troposphere-long.csv.bz2")

# Define UI
ui <- fluidPage(
  
  # Set page title
  titlePanel("UAH Lower Troposphere Temperature"),
  
  # Main panel with tabset
  mainPanel(
    tabsetPanel(
      
      # Opening page with dataset information
      tabPanel("Information", 
               tags$div(
                 h2("Dataset Information"), 
                 br(), 
                 "This dataset contains monthly temperature readings from satellite observations 
                 of the lower troposphere (the lowest layer of the Earth's atmosphere) from January 
                 1979 to December 2020. The data was collected by the University of Alabama in 
                 Huntsville (UAH) and is provided by Remote Sensing Systems (RSS).", 
                 br(), 
                 tags$div(
                   tags$h2("General Information"),
                   tags$p(
                     tags$strong("Dataset: "), "UAH Lower Troposphere ",
                     tags$em("Long-Term Data Record")
                   ),
                   tags$p(
                     tags$strong("Source: "),
                     "University of Alabama, Huntsville (UAH), ",
                     tags$em("Global Temperature Monitoring")
                   ),
                   tags$p(
                     tags$strong("Description: "),
                     "This dataset provides monthly global average temperature anomalies in the Earth's lower atmosphere, measured from satellite microwave sensors."
                   ),
                   tags$p(
                     tags$strong("Variables: "),
                     "year, month, temperature anomaly (Kelvin)"
                   )
                 )
               )),
      
      # Plot page with temperature graph
      tabPanel("Plot", 
               h1(strong("Check Temperature by Year")),
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "year", label = "Select Year:",
                     choices = sort(unique(uah_data$year)), selected = NULL
                   ),
                   checkboxGroupInput(
                     inputId = "mon", label = "Select Month(s):",
                     choices = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12"),
                     selected = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11", "12")
                   ),
                   selectInput(
                     inputId = "color", label = "Select Point Color:",
                     choices = c("blue", "red", "green"), selected = "blue"
                   ),
                   sliderInput(
                     inputId = "size", label = "Select Point Size:",
                     min = 1, max = 5, value = 3
                   )
                 ),
                 mainPanel(
                   plotlyOutput("chart1"),
                   verbatimTextOutput("text1"),
                   hr()
                 )
               )
      ),
      tabPanel("Table",
               h1(strong("Define Region by Month")),
               sidebarLayout(
                 sidebarPanel(
                   selectInput(
                     inputId = "month", label = "Select Month:",
                     choices = unique(uah_data$month)
                   )
                 ),
                 mainPanel(
                   dataTableOutput("chart2"),
                   verbatimTextOutput("text2"),
                   hr()
                 )
               )
      )
    )
  )
)


# Define server function
server <- function(input, output) {
  
  graph_data <- reactive({
    uah_data %>%
      filter(year == input$year) %>%
      filter(month %in% input$mon) %>%
      group_by(month) %>%
      summarize(total_temp = sum(temp))
  })
  
  output$chart1 <- renderPlotly({
    ggplotly(
      ggplot(graph_data(), aes(x = month, y = total_temp)) +
        geom_point(color = input$color, size = input$size) +
        labs(title = "Temperature change by month", x = "Month", y = "Temp Change") +
        scale_x_continuous(breaks = 1:12)
    )
  })
  
  table_data <- reactive({
    uah_data %>%
      filter(month == input$month) %>%
      select(region, temp)
  })
  
  output$chart2 <- renderDataTable({
    table_data()
  })
  
  output$text1 <- renderText({
    paste("Selected subset contains", nrow(graph_data()), "observations")
  })
  
  output$text2 <- renderText({
    paste("Selected subset contains", nrow(table_data()), "observations")
  })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)