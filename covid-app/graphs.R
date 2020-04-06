


covid_graph<-function(graph_data,country_list,y_scale){
  max_date<-graph_data$Date %>% max() %>% as.Date()
  add_units <- function(n) {
    labels <- ifelse(n < 1000, n,  # less than thousands
                     ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                            ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                   ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                          'too big!'
                                   ))))
    return(labels)
  }

  graph_data<- graph_data %>% select(-daily_change) %>% 
    filter(Country %in% c('Global', country_list))
  graph_data$Country <-
    factor(graph_data$Country ,
           levels = c('Global', country_list)) #this controls facets order

  
 g1<-ggplot(data = graph_data, aes(x = Date, y = value, colour = Measure)) +
    facet_wrap(. ~ Country, scales = 'free_y') + geom_line() +
    scale_color_manual(values = alpha(c("blue", "red","green"), 0.5))+
    scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) +
    labs(y="value", x = "Months")
 if(y_scale=='log10'){
   g1<-g1 + scale_y_continuous(labels = add_units,trans = 'log10')+ 
     ggtitle( paste0('Current Situation in log10 scale',': Data updated as of ',max_date))
 } else{
   g1<-g1 + scale_y_continuous(labels = add_units)+ 
     ggtitle(paste0('Current Situation in original scale',': Data updated as of ',max_date))
 }
 
}

covid_graph_daily_change<-function(graph_data,country_list){
  max_date<-graph_data$Date %>% max() %>% as.Date()
  add_units <- function(n) {
    labels <- ifelse(n < 1000, n,  # less than thousands
                     ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                            ifelse(n < 1e9, paste0(round(n/1e6), 'M'),  # in millions
                                   ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                          'too big!'
                                   ))))
    return(labels)
  }
######
  graph_data<- graph_data %>% select(-value) %>% rename(value=daily_change) %>% 
    filter(Country %in% c('Global', country_list))
  
  graph_data$Country <-
    factor(graph_data$Country ,
           levels = c('Global', country_list)) #this controls facets order
  
    g1<-ggplot(data = graph_data, aes(x = Date, y = value, fill = Measure)) +
      facet_wrap(. ~ Country, scales = 'free_y') + geom_area() +
      scale_fill_manual(values = alpha(c("blue", "red", "green"), 0.5)) +
      scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) +
      scale_y_continuous(labels = add_units) + ggtitle(paste0('Daily Change',': Data updated as of ',max_date)) + 
      labs(y="Daily Change", x = "Months")
}