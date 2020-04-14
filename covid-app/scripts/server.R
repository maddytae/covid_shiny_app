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
  

  output$graph_deep <- renderPlot({
    top.n.country <- top.n.country()
    
    
    plot_show <- switch(
      input$graph_type_deep,
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
  
  output$tableView_deep <- renderDataTable({
    top.n.state <- top.n.country.state() %>% ungroup() %>% filter(Country==input$deep_dive) %>%
      select(-Country)  
 
    
    graph_data<-graph_data_deep() %>% ungroup() %>% filter(Country==input$deep_dive) %>%
      select(-Country)
    country_data<-graph_data_deep() %>% ungroup() %>% filter(Country==input$deep_dive) %>% select(-State) %>%
      group_by_if(~
                    !is.numeric(.)) %>%
      summarise(value = sum(value),
                daily_change = sum(daily_change)) %>% ungroup() %>%
    
      select(-Country) %>% mutate(State=input$deep_dive) %>% select(names(graph_data)) 
  
    graph_data<-rbind( graph_data,country_data) 
    
   write_csv(graph_data,"graph_data.csv")
    data_output_state(graph_data = graph_data,
                state_list = c(input$deep_dive, unique(c(
                  top.n.state$State[1:min(nrow(top.n.state), input$num_state)]
                  
                ))))
    
  })
  
  #This is rendering ui in deep dive from server itself;
  output$varx <- renderUI({
   # graph_data_deep()
    selectInput("deep_dive", "Provide a country for deep dive", choices =
                  deep_dive_countries())
  })
  
  
#####
  
  
  
}