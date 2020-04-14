


covid_graph<-function(graph_data,country_list,y_scale,title=TRUE){
 
  max_date<-graph_data$Date %>% max() %>% as.Date()
  add_units <- function(n) {
    labels <- ifelse(n < 1000, n,  # less than thousands
                     ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                            ifelse(n < 1e9, paste0(round(n/1e6,digits=2), 'M'),  # in millions
                                   ifelse(n < 1e12, paste0(round(n/1e9,digits=2), 'B'), # in billions
                                          'too big!'
                                   ))))
    return(labels)
  }

  graph_data<- graph_data %>% select(-daily_change) %>% 
    filter(Country %in% c(country_list))
  graph_data$Country <-
    factor(graph_data$Country ,
           levels = c( country_list)) #this controls facets order

  
 g1<-ggplot(data = graph_data, aes(x = Date, y = value, colour = Measure)) +
    facet_wrap(. ~ Country, scales = 'free_y') + geom_line() +
    scale_color_manual(values = alpha(c("blue", "red","green"), 0.5))+
    scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) +
    labs(y="value", x = "Months")
 if(y_scale=='log10'){
   g1<-g1 + scale_y_continuous(labels = add_units,trans = 'log10')
   
   if(title){
   
     g1<-g1+ggtitle( paste0('Status as of: ',max_date,' (Log10 Scale)'))}
   
 } else{
   g1<-g1 + scale_y_continuous(labels = add_units)   
   
   if(title){
     
     g1<-g1+ggtitle( paste0('Status as of: ',max_date,' (Linear Scale)'))}
 }
 if(!title & y_scale=='log10'){
 g1<-g1+theme(aspect.ratio=2)+ggtitle('Log10')}
 
 if(!title & y_scale!='log10'){
   g1<-g1+theme(aspect.ratio=2)+ggtitle('Linear')
   
   }
 
 g1
}

covid_graph_daily_change<-function(graph_data,country_list,title=TRUE){
  max_date<-graph_data$Date %>% max() %>% as.Date()
  #message(max_date)
  add_units <- function(n) {
    labels <- ifelse(n < 1000, n,  # less than thousands
                     ifelse(n < 1e6, paste0(round(n/1e3), 'k'),  # in thousands
                            ifelse(n < 1e9, paste0(round(n/1e6,digits=2), 'M'),  # in millions
                                   ifelse(n < 1e12, paste0(round(n/1e9), 'B'), # in billions
                                          'too big!'
                                   ))))
    return(labels)
  }
######
  graph_data<- graph_data %>% select(-value) %>% rename(value=daily_change) %>% 
    filter(Country %in% c( country_list))
  
  graph_data$Country <-
    factor(graph_data$Country ,
           levels = c(country_list)) #this controls facets order
  
    g1<-ggplot(data = graph_data, aes(x = Date, y = value, fill = Measure)) +
      facet_wrap(. ~ Country, scales = 'free_y') + geom_area( ) +
      scale_fill_manual(values = alpha(c("blue", "red", "green"), 0.5)) +
      scale_x_datetime(date_breaks = "1 month", labels = date_format("%b")) +
      scale_y_continuous(labels = add_units)  + 
      labs(y="Daily Change", x = "Months")
    
    
    if(title){
      
      g1<-g1+ ggtitle(paste0('Daily Change (Updated as of: ',max_date,')'))
      }
    if(!title){
      g1<-g1+theme(aspect.ratio=2)+ggtitle('Daily Change')}
    
    
    
g1
    }

covid_graph_distribution<-function(graph_data,country_list){
  #write_csv(graph_data,"graph_data.csv")
  message(country_list)
  
  max_date<-graph_data$Date %>% max() %>% as.Date()
  graph_data<-ungroup(graph_data)
  
  
 
  graph_data<-graph_data %>% filter(Country %in% c(country_list),
                           Date==max(Date)) %>% ungroup() %>%
    select(-Date,-daily_change)  %>%
    dcast(Country~Measure) 
    
    graph_data$recovered[is.na(graph_data$recovered)]<-0 #US don't have recovery data for states 
  #and will cause issue otherwise
    
    graph_data<-graph_data %>% mutate(active=confirmed-deaths-recovered) %>% select(-confirmed) %>%
    melt(id.vars = c('Country')) %>% rename(Measure=variable)  %>% mutate(Measure=as.character(Measure)) %>%
    group_by(Country) %>% mutate(share=(round(value/sum(value),digits = 4))) %>%  select(-value) %>% 
    ungroup() 
  
  graph_data$Country <-
    factor(graph_data$Country ,
           levels = c(country_list)) #this controls  order
  
  ggplot(data=graph_data,aes(x=Country,y=share,fill=Measure))+
    geom_bar(stat="identity") +
    ylab('percentage distribution') + 
    scale_y_continuous(labels = scales::percent_format(accuracy = 1))+
    #geom_text(aes(x = Country, 
    #                 y = share , label = percent(round(share,4))),size=3) +
    scale_fill_manual(values = alpha(c("blue", "red", "green"), 0.5))+
    ggtitle(paste0('Status as of: ',max_date))+ 
    theme_bw() + 
    theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank())+
    theme(axis.text.x = element_text(angle = 270)) 
  
}