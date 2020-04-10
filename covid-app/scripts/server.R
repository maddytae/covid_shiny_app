server <- function(input, output) {
  options(shiny.sanitize.errors = TRUE)
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
    #message(glimpse(confirmed())) 
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