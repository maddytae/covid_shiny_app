
data_output<-function(graph_data,country_list){

#graph_data<-read_csv(file.path('covid-app','scripts','graph_data.csv'))
#country_list<-c('UK','India')
max_date<-graph_data$Date %>% max() %>% as.Date()
#message(max_date)
graph_data<-graph_data %>% ungroup() %>% filter(Date==max(Date),
                Country %in% c('Global', country_list)) %>% select(-Date,-daily_change) %>%

#group_by(Country,Measure) %>%
#  summarise(value=sum(value)) %>%
dcast(Country~Measure) %>% 
arrange(desc(confirmed))%>%
mutate(confirmed=formatC(confirmed, format = "f", big.mark = ",", drop0trailing = TRUE),
       deaths=formatC(deaths, format = "f", big.mark = ",", drop0trailing = TRUE),
       recovered=formatC(recovered, format = "f", big.mark = ",", drop0trailing = TRUE),
       Date=max_date) %>% select(Country,Date,confirmed,deaths,recovered)

} 


data_output_state<-function(graph_data,state_list){
  
 
  max_date<-graph_data$Date %>% max() %>% as.Date()


  
  graph_data<-graph_data %>% ungroup() %>% filter(Date==max(Date)) %>% select(-Date,-daily_change) %>%
    
  filter(State %in% c(state_list)) %>%
 
    dcast(State~Measure) %>% 
    arrange(desc(confirmed))%>%
    mutate(confirmed=formatC(confirmed, format = "f", big.mark = ",", drop0trailing = TRUE),
           deaths=formatC(deaths, format = "f", big.mark = ",", drop0trailing = TRUE),
           recovered=formatC(recovered, format = "f", big.mark = ",", drop0trailing = TRUE),
           Date=max_date) %>% select(State,Date,confirmed,recovered,deaths)
  
  
  
}