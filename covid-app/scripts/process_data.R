files <- c(
  'time_series_covid19_confirmed_global.csv',
  'time_series_covid19_deaths_global.csv',
  'time_series_covid19_recovered_global.csv'
)
path <-'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/'


confirmed<-reactiveFileReader(3.6e6, NULL, file.path(path,files[1]), read_csv) #check once every hour
deaths<-reactiveFileReader(3.6e6, NULL, file.path(path,files[2]), read_csv)   #check once every hour
recovered<-reactiveFileReader(3.6e6, NULL, file.path(path,files[3]), read_csv) #check once every hour

make.combined.data<-reactive({
  raw.data.confirmed <-
    confirmed() %>%
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
    deaths() %>%
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
    recovered() %>%
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
  combined.data.process <-
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
  
  combined.data.process$Country[combined.data.process$Country=='Taiwan*']<-'Taiwan'
  combined.data.process$Country[combined.data.process$Country=='United Arab Emirates']<-'UAE'
  combined.data.process$Country[combined.data.process$Country=='United Kingdom']<-'UK'
  combined.data.process$Country[combined.data.process$Country=='Korea, South']<-'South Korea'
  
  
  saveRDS(combined.data.process,file.path('data','clean_long_data.rds'))
  ###save source files in csv for any analysis
  #write_csv(confirmed(),file.path('data',files[1]))
  #write_csv(deaths(),file.path('data',files[2]))
  #write_csv(recovered(),file.path('data',files[3]))
  
})


combined.data<-reactive({
  make.combined.data()
  readRDS(file.path('data', 'clean_long_data.rds'))})
top.n.country <- reactive({
  combined.data<-combined.data()
  top.n.country <- combined.data %>% filter(Measure == 'confirmed') %>%
    select(Country, daily_change)  %>% group_by(Country) %>%
    summarise(daily_change = sum(daily_change)) %>% arrange(desc(daily_change))
  top.n.country
})

