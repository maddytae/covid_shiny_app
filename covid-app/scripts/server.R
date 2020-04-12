server <- function(input, output) {
  options(shiny.sanitize.errors = TRUE)
  
  ####Status Report######
  output$graph <- renderPlot({
    top.n.country <- top.n.country()
    
    
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

  #####
  
  
  ######Deep Dive#####
  
  output$graphdeep1 <- renderPlot({
    validate(need(input$deep_dive, 'Please provide a country!'))
    
    
    k <- graph_data() %>% ungroup() %>%
      filter(Country == input$deep_dive,
             Measure == 'confirmed',
             value > 100)
    first_100 <- k  %>% filter(Date == min(Date))
    k <-
      graph_data() %>% ungroup() %>% filter(Date >= first_100$Date)
    
    
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
    k <-
      graph_data() %>% ungroup() %>% filter(Date >= first_100$Date)
    
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
    k <-
      graph_data() %>% ungroup() %>% filter(Date >= first_100$Date)
    
    covid_graph_daily_change(
      graph_data = k,
      country_list = input$deep_dive,
      title = FALSE
    )
  })
  
  output$graph_deep <- renderPlot({
    validate(need(input$deep_dive, ''))
    k <- graph_data() %>% ungroup() %>%
      filter(Country == input$deep_dive,
             Measure == 'confirmed',
             value > 100)
    first_100 <- k  %>% filter(Date == min(Date))
    k <-
      graph_data() %>% ungroup() %>% filter(Date >= first_100$Date)
    
    covid_graph_daily_change(
      graph_data = k,
      country_list = input$deep_dive,
      title = FALSE
    )
  })
  
  #This is rendering ui in deep dive from server itself;
  output$varx <- renderUI({
    graph_data_deep()
    selectInput("deep_dive", "Provide a country for deep dive", choices =
                  deep_dive_countries())
  })
  
  
#####
  
  
  
}