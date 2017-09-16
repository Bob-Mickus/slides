## Load libraries and prepare non-reactive data -----

library(tidyverse)
library(lubridate)
library(shiny)
library(shinydashboard)
library(leaflet)

source("source_functions.R")

crime_dat <- readRDS("crime_dat_2016.RDS")

crimes <- crime_dat %>%
  group_by(Crime) %>%
  summarize(n = n()) %>%
  arrange(desc(n)) %>%
  pull(Crime)

date_range <- floor_date(range(crime_dat$datetime), "days") %>% as.Date

## User Interface --------

ui <- dashboardPage(
  dashboardHeader(title = "Chicago Crimes 2016"),
  dashboardSidebar(
    selectInput(inputId = "crime_choice", 
                label = "Select a crime", 
                choices = crimes, 
                selected = "Burglary", 
                multiple = FALSE), 
    dateRangeInput(inputId = "date_select", 
                   label = "Choose date range", 
                   start = date_range[1], 
                   end = date_range[2], 
                   format = "mm-dd-yyyy", startview = "month", 
                   language = "en", separator = " to "),
    selectInput(inputId = "color_by", 
                label = "Choose a descriptor to color on:", 
                choices = c("Description", "Arrest", "Domestic", "Location"), 
                selected = "Description", 
                multiple = FALSE, selectize = TRUE),
    radioButtons(inputId = "seasonality_unit", 
                 label = "Choose a unit of time:", 
                 choices = c("hour", "month", "day of week"), 
                 selected = "hour")
  ),
  dashboardBody(
    fluidRow(
      box(title = "Map",
        leafletOutput("chicago_map", height = "300px"), width = NULL)
    ),
    fluidRow(
      box(title = "Seasonality Bar Plot",
          plotOutput("barplot", height = "150px"), width = NULL)
    )
  )
)

## Server -----------

server <- function(input, output) {
  
  crime_filter <- reactive({
    
      validate(
        need(input$date_select[1] < input$date_select[2], 
             "Please select appropriate date range.")
      )
      
      crime_dat %>%
        filter_crime_data(crime = input$crime_choice, 
                         start = input$date_select[1],
                         end = input$date_select[2],
                         group = input$color_by)
  })
  
  output$chicago_map <- renderLeaflet({
    leaflet() %>% 
      setView(lng= -87.6298, lat=41.8781, zoom=9) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(minZoom = 9)) %>%
      addCircleMarkers(data = crime_filter(), 
                       lng = ~ Longitude, 
                       lat = ~ Latitude,
                       radius = 1.5, fill = TRUE,
                       color = crime_filter()$Group_plot_color, opacity = .5)
  })

  bounds <- reactive(
    input$chicago_map_bounds
  )
  
  output$barplot <- renderPlot({
    data_list <- create_barplot_data(crime_filter = crime_filter(),
                                     time_measure = input$seasonality_unit,
                                     north = bounds()$north, 
                                     east = bounds()$east,
                                     south = bounds()$south, 
                                     west = bounds()$west)
    
    plot_barplot_by_time_loc(barplot_data_list = data_list)
  })
  
}

shinyApp(ui = ui, server = server)