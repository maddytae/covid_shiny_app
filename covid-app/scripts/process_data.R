files <- c(
  'time_series_covid19_confirmed_global.csv',
  'time_series_covid19_deaths_global.csv',
  'time_series_covid19_recovered_global.csv',
  'time_series_covid19_confirmed_US.csv',
  'time_series_covid19_deaths_US.csv'
)
path <-'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/'



for(i in 1:length(files)){
  f<-gsub('time_series_covid19_','',files[i])
  f<-gsub('.csv','',f) 
  assign(f, reactiveFileReader(3.6e6, NULL, file.path(path,files[i]), read_csv))
}

req_cols<-c('Province/State','Country/Region','Province_State','Country_Region')
align_data<-reactive({
 
  k<-names(confirmed_global())
  raw.data.confirmed.global<-confirmed_global() %>% select(any_of(req_cols),grep('1/22/20', k):length(k)) %>% 
    mutate(Measure='confirmed') %>% filter(`Country/Region`!='US') %>% rename(State=`Province/State`,
                                                                              Country=`Country/Region`)
  
 
  
  k<-names(deaths_global())
  raw.data.deaths.global<-deaths_global() %>% select(any_of(req_cols),grep('1/22/20', k):length(k)) %>%
    mutate(Measure='deaths') %>% filter(`Country/Region`!='US') %>% rename(State=`Province/State`,
                                                                           Country=`Country/Region`)
  
  k<-names(recovered_global())
  raw.data.recovered.global<-recovered_global() %>% select(any_of(req_cols),grep('1/22/20', k):length(k)) %>%
    mutate(Measure='recovered') %>% rename(State=`Province/State`,
                                           Country=`Country/Region`)
  
  k<-names(confirmed_US())
  raw.data.confirmed.US<-confirmed_US() %>% select(any_of(req_cols),grep('1/22/20', k):length(k)) %>%
    mutate(Measure='confirmed') %>% rename(State=Province_State,
                                           Country=Country_Region)
  
  k<-names(deaths_US())
  raw.data.deaths.US<-deaths_US() %>% select(any_of(req_cols),grep('1/22/20', k):length(k)) %>%
    mutate(Measure='deaths') %>% rename(State=Province_State,
                                        Country=Country_Region)
  raw.data.full<-rbind(raw.data.confirmed.global,raw.data.deaths.global,raw.data.recovered.global,
                       raw.data.confirmed.US,raw.data.deaths.US)
    


  
  raw.data.full <-
    raw.data.full %>% 
    melt(id.vars = c('Country','State','Measure')) %>%
    rename(Date = variable) %>%
    mutate_if(is.factor, as.character) %>%
    group_by(Country,State,Date,Measure) %>% summarise(value=sum(value,na.rm = TRUE)) %>% ungroup() %>%
    mutate(Date = as.Date(Date, format = '%m/%d/%y')) %>% 
    arrange((Date)) %>% group_by_if(is.character) %>%
    mutate(daily_change = value - ifelse(is.na(lag(value)), 0, lag(value))) %>% ungroup() %>%
    select(Date,Country,State,Measure,value,daily_change) 
  
  saveRDS(raw.data.full,file.path('data','clean_long_data.rds'))
  

 }
)

combined.data<-reactive({
  align_data()
  readRDS(file.path('data', 'clean_long_data.rds')) 
  
  })
top.n.country <- reactive({
  combined.data<-combined.data()  %>% select(-State)
  top.n.country <- combined.data %>% filter(Measure == 'confirmed') %>%
    select(Country, daily_change)  %>% group_by(Country) %>%
    summarise(daily_change = sum(daily_change)) %>% arrange(desc(daily_change))
  top.n.country
})

top.n.country.state <- reactive({
  combined.data<-combined.data()  
  top.n.country <- combined.data %>% filter(Measure == 'confirmed') %>%
    select(Country, daily_change)  %>% group_by(Country,State) %>%
    summarise(daily_change = sum(daily_change)) %>% arrange(desc(daily_change))
  top.n.country.state
})


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

graph_data_deep<-reactive({
  combined.data.deep <- combined.data() 
  graph_data_deep <- combined.data.deep %>% group_by_if(~
                                                          !is.numeric(.)) %>%
    summarise(value = sum(value),
              daily_change = sum(daily_change))
  graph_data_deep$Date <- as.POSIXct(graph_data_deep$Date)
  
  graph_data_deep
  write_csv(graph_data_deep,"graph_data_deep.csv")
})




