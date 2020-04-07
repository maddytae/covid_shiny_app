# Load packages ----
library(shiny)
library(tidyverse)
library(readr)
library(scales)
rm(list = ls())


source("graphs.R")

# Load data ----

covid_data <- readRDS(file.path("data","clean_long_data.rds")) 


top.n.country <- covid_data %>% filter(Measure == 'confirmed') %>%
  select(Country, daily_change)  %>% group_by(Country) %>%
  summarise(daily_change = sum(daily_change)) %>% arrange(desc(daily_change))

graph_data <- covid_data %>% group_by_if( ~
                                                           !is.numeric(.)) %>%
  summarise(value = sum(value),
            daily_change = sum(daily_change))

k <- select(covid_data,-Country) %>% group_by_if( ~
                                                           !is.numeric(.)) %>%
  summarise(value = sum(value),
            daily_change = sum(daily_change)) %>% mutate(Country = 'Global') %>%
  select(Country, Date, Measure, value, daily_change)

graph_data <- rbind(graph_data, k)

graph_data$Date <- as.POSIXct(graph_data$Date)


# User interface ----
ui <- fluidPage(titlePanel("Covid-19 Dashboard"),
                
                sidebarLayout(
                  sidebarPanel(
                    helpText("Basic covid-19 status visualisation
                              based on CSSE data from
                              Johns Hopkins University"),
                    
                    selectInput(
                      "graph_type",
                      label = "Choose a graph to display",
                      choices = c(
                        "original_scale",
                        "log_scale",
                        "daily_change"
                      ),
                      selected = "daily_change"
                    ),
                    numericInput("num",
                                 label = "Select top n countries",
                                 value = 20)
                  ,textInput("text", 
                             label = "Add additional countries
                             separated by comma", 
                             value = "Singapore,India")
                  ),
                  
                  mainPanel(plotOutput("graph"))
                ))
# Server logic ----
server <- function(input, output) {
  options(shiny.sanitize.errors = TRUE)
  output$graph <- renderPlot({
    plot_show <- switch(
      input$graph_type,
      "log_scale" = covid_graph(
        graph_data = graph_data,
        country_list = unique(c(top.n.country$Country[1:min(nrow(top.n.country),input$num)], unlist(strsplit(input$text, ",")))),
        y_scale='log10'
      ),
      "original_scale" = covid_graph(
        graph_data = graph_data,
        country_list = unique(c(top.n.country$Country[1:min(nrow(top.n.country),input$num)], unlist(strsplit(input$text, ",")))),
        y_scale='original'
      ),
      "daily_change" =  covid_graph_daily_change(
        graph_data = graph_data,
        country_list = unique(c(top.n.country$Country[1:min(nrow(top.n.country),input$num)], unlist(strsplit(input$text, ","))))
      )
    )
    plot_show
  })
}

# Run app ----
shinyApp(ui, server)