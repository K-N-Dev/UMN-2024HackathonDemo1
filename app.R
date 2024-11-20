library(shiny)
library(vroom)
library(ggplot2)
library(scales)
library(tidyverse)
# Load CSV file with Vroom
VisitTable <- vroom::vroom("RecreationVisits.csv")

# Define UI for application
ui <- fluidPage(
        fluidRow(
          #Region drop down selector input
          column(6, selectInput("Region", "Region", choices = unique(VisitTable$Region), selected = unique(VisitTable$Region), multiple = FALSE)),
          #Year range slider
          column(4, tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
          sliderInput("years", "Years",
                     min(VisitTable$Year), max(VisitTable$Year),
                     value = c(2020, 2023),
                     sep = ""))),
        fluidRow(
          plotOutput("chart")
        )
)

# Define server logic
server <- function(input, output) {
    #Reactive Filter applied to data table to match information from inputs
  data <- reactive({
    VisitTable %>% 
    filter(between(Year, input$years[1], input$years[2]) & Region %in% input$Region)
  }) 
    #ggplot function to draw simple line chart
    output$chart <- renderPlot({
      ggplot( data(), aes(x=Year, y=RecreationVisits)) +
        geom_line(aes(color = ParkName)) +
        geom_point(aes(color = ParkName)) +
        scale_y_continuous(labels = scales::comma)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
