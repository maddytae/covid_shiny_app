# Load packages ----
library(shiny)
library(tidyverse)
library(readr)
library(scales)
library(reshape2)
rm(list = ls())


source("graphs.R")


# User interface ----
ui <- fluidPage(titlePanel("Covid-19 Dashboard"),
                
                sidebarLayout(
                  sidebarPanel(
                    helpText("Basic covid-19 status visualisation
                              based on CSSE data(live link) from
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
  
  
  combined.data <- reactive({
    
    files <- c(
      'time_series_covid19_confirmed_global.csv',
      'time_series_covid19_deaths_global.csv',
      'time_series_covid19_recovered_global.csv'
    )
    path <-'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/'
    
    
    raw.data.confirmed <-
      read_csv(file.path(path,files[1]),na = "") %>%
      melt(id.vars = c('Province/State', 'Country/Region', 'Lat', 'Long')) %>%
      rename(
        State = 'Province/State',
        Country = 'Country/Region',
        confirmed = value,
        Date = variable
      ) %>%
      mutate_if(is.factor, as.character) %>%
      group_by(Country,Date) %>% summarise(confirmed=sum(confirmed))
    
    raw.data.deaths <-
      read_csv(file.path(path,files[2]),na = "") %>%
      melt(id.vars = c('Province/State', 'Country/Region', 'Lat', 'Long')) %>%
      rename(
        State = 'Province/State',
        Country = 'Country/Region',
        deaths = value,
        Date = variable
      ) %>%
      mutate_if(is.factor, as.character) %>%
      group_by(Country,Date) %>% summarise(deaths=sum(deaths))
    
    raw.data.recovered <-
      read_csv(file.path(path,files[3]),na="") %>%
      melt(id.vars = c('Province/State', 'Country/Region', 'Lat', 'Long')) %>%
      rename(
        State = 'Province/State',
        Country = 'Country/Region',
        recovered = value,
        Date = variable
      ) %>%
      mutate_if(is.factor, as.character) %>%
      group_by(Country,Date) %>% summarise(recovered=sum(recovered))
    
    #combine in a single dataframe
    combined.data <-
      raw.data.confirmed %>% left_join(raw.data.deaths, by = c('Country', 'Date')) %>%
      left_join(raw.data.recovered, by = c( 'Country', 'Date')) %>% group_by_if(~
                                                                                  !is.numeric(.)) %>%
      summarise(
        confirmed = sum(confirmed, na.rm = TRUE),
        deaths = sum(deaths, na.rm = TRUE),
        recovered = sum(recovered, na.rm = TRUE)
      ) %>% mutate(Date = as.Date(Date, format = '%m/%d/%y')) %>%
      melt(id.vars = c( 'Country', 'Date')) %>% rename(Measure = variable) %>% mutate_if(is.factor, as.character) %>%
      arrange((Date)) %>%
      group_by_if(is.character) %>%
      mutate(daily_change = value - ifelse(is.na(lag(value)), 0, lag(value))) %>% ungroup()
    
    combined.data$Country[combined.data$Country=='Taiwan*']<-'Taiwan'
    combined.data$Country[combined.data$Country=='United Arab Emirates']<-'UAE'
    combined.data$Country[combined.data$Country=='United Kingdom']<-'UK'
    combined.data$Country[combined.data$Country=='Korea, South']<-'South Korea'
    combined.data
  })
  
  top.n.country <- reactive({
    combined.data<-combined.data()
    #message(glimpse(combined.data))
    top.n.country <- combined.data %>% filter(Measure == 'confirmed') %>%
      select(Country, daily_change)  %>% group_by(Country) %>%
      summarise(daily_change = sum(daily_change)) %>% arrange(desc(daily_change))
    top.n.country
  })
  
   graph_data<-reactive({
  
     combined.data<-combined.data()
     
    graph_data <- combined.data() %>% group_by_if( ~
                                                !is.numeric(.)) %>%
      summarise(value = sum(value),
                daily_change = sum(daily_change))
    
    k <- select(combined.data,-Country) %>% group_by_if( ~
                                                        !is.numeric(.)) %>%
      summarise(value = sum(value),
                daily_change = sum(daily_change)) %>% mutate(Country = 'Global') %>%
      select(Country, Date, Measure, value, daily_change)
  
    graph_data <- rbind(graph_data, k)
   
    graph_data$Date <- as.POSIXct(graph_data$Date)
    
    graph_data
  })
  
  
  output$graph <- renderPlot({
  
    top.n.country<-top.n.country()
    
    plot_show <- switch(
      input$graph_type,
      "log_scale" = covid_graph(
        graph_data = graph_data(),
        country_list = unique(c(top.n.country$Country[1:min(nrow(top.n.country),input$num)], unlist(strsplit(input$text, ",")))),
        y_scale='log10'
      ),
      "original_scale" = covid_graph(
        graph_data = graph_data(),
        country_list = unique(c(top.n.country$Country[1:min(nrow(top.n.country),input$num)], unlist(strsplit(input$text, ",")))),
        y_scale='original'
      ),
      "daily_change" =  covid_graph_daily_change(
        graph_data = graph_data(),
        country_list = unique(c(top.n.country$Country[1:min(nrow(top.n.country),input$num)], unlist(strsplit(input$text, ","))))
      )
    )
    plot_show
  })
}

# Run app ----
shinyApp(ui, server)