server <- function(input, output) {
  options(shiny.sanitize.errors = TRUE)

  deep_dive_countries<-reactive({
    deep_country<-combined.data() %>% filter(!is.na(State)) %>% select(Country) %>% unique() 
    deep_country$Country
  })
  graph_data <- reactive({
   


    combined.data <- combined.data() %>% select(-State)
    
    graph_data <- combined.data %>% group_by_if(~
                                                    !is.numeric(.)) %>%
      summarise(value = sum(value),
                daily_change = sum(daily_change))
    
    k <- select(combined.data, -Country) %>% group_by_if(~
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
    top.n.country <- top.n.country()
    #write_csv(graph_data(), 'graph_data.csv')
    plot_show <- switch(
      input$graph_type,
      "graph: log10_scale" = covid_graph(
        graph_data = graph_data(),
        country_list = c('Global', unique(c(
          top.n.country$Country[1:min(nrow(top.n.country), input$num)],
          unlist(strsplit(input$text, ","))
        ))),
        y_scale = 'log10'
      ),
      "graph: linear_scale" = covid_graph(
        graph_data = graph_data(),
        country_list = c('Global', unique(c(
          top.n.country$Country[1:min(nrow(top.n.country), input$num)],
          unlist(strsplit(input$text, ","))
        ))),
        y_scale = 'original'
      ),
      "graph: daily_change" =  covid_graph_daily_change(
        graph_data = graph_data(),
        country_list = c('Global', unique(c(
          top.n.country$Country[1:min(nrow(top.n.country), input$num)],
          unlist(strsplit(input$text, ","))
        )))
      ),
      "graph: case_distribution" =  covid_graph_distribution(
        graph_data = graph_data(),
        country_list = c('Global', unique(c(
          top.n.country$Country[1:min(nrow(top.n.country), input$num)],
          unlist(strsplit(input$text, ","))
        )))
      )
    )
    plot_show
  })
  
  output$tableView <- renderDataTable({
    top.n.country <- top.n.country()
    data_output(graph_data = graph_data(),
                country_list = c('Global', unique(c(
                  top.n.country$Country[1:min(nrow(top.n.country), input$num)],
                  unlist(strsplit(input$text, ","))
                ))))
    
  })
  
  output$graphdeep1 <- renderPlot({
    validate(need(input$deep_dive, 'Please provide a country!'))
    
    
    k <- graph_data() %>% ungroup() %>%
      filter(Country == input$deep_dive,
             Measure == 'confirmed',
             value > 100)
    first_100 <- k  %>% filter(Date == min(Date))
    k <- graph_data() %>% ungroup() %>% filter(Date >= first_100$Date)
    
    
    covid_graph(
      graph_data = k,
      country_list = input$deep_dive,
      y_scale = 'original',
      title = FALSE
    )
  })
  output$graphdeep2 <- renderPlot({
    validate(need(input$deep_dive, ''))
    
    k <- graph_data() %>% ungroup() %>%
      filter(Country == input$deep_dive,
             Measure == 'confirmed',
             value > 100)
    first_100 <- k  %>% filter(Date == min(Date))
    k <- graph_data() %>% ungroup() %>% filter(Date >= first_100$Date)
    
    covid_graph(
      graph_data = k ,
      country_list = input$deep_dive,
      y_scale = 'log10',
      title = FALSE
    )
  })
  
  output$graphdeep3 <- renderPlot({
    validate(need(input$deep_dive, ''))
    k <- graph_data() %>% ungroup() %>%
      filter(Country == input$deep_dive,
             Measure == 'confirmed',
             value > 100)
    first_100 <- k  %>% filter(Date == min(Date))
    k <- graph_data() %>% ungroup() %>% filter(Date >= first_100$Date)
    
    covid_graph_daily_change(
      graph_data = k,
      country_list = input$deep_dive,
      title = FALSE
    )
  })
  
  # Pulling the list of variable for choice of variable x
  output$varx <- renderUI({
    message(print(deep_dive_countries))
    selectInput("deep_dive", "Provide a country for deep dive", choices=deep_dive_countries())
  })
  
  
  
  
  
  
}