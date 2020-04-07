library(tidyverse)
library(reshape2)
library(scales)

rm(list = ls())


#######Pull Input Files######


files <- c(
  'time_series_covid19_confirmed_global.csv',
  'time_series_covid19_deaths_global.csv',
  'time_series_covid19_recovered_global.csv',
  'time_series_covid19_confirmed_US.csv',
  'time_series_covid19_deaths_US.csv'
)
path <-
  paste0(
    'https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/'
  )

download <- function(filename) {
  url <- file.path(path, filename)
  dest <- file.path('covid-app','data','raw_data', filename)
  download.file(url, dest)
  
}
bin <- lapply(files, download)


######

#confirmed cases
raw.data.confirmed <-
  read_csv(file.path('covid-app','data','raw_data', 'time_series_covid19_confirmed_global.csv'),
           na = "") %>%
  melt(id.vars = c('Province/State', 'Country/Region', 'Lat', 'Long')) %>%
  rename(
    State = 'Province/State',
    Country = 'Country/Region',
    confirmed = value,
    Date = variable
  ) %>%
  mutate_if(is.factor, as.character) %>%
  group_by(Country,Date) %>% summarise(confirmed=sum(confirmed))

#deaths
raw.data.deaths <-
  read_csv(file.path('covid-app','data','raw_data', 'time_series_covid19_deaths_global.csv'),
           na = "") %>%
  melt(id.vars = c('Province/State', 'Country/Region', 'Lat', 'Long')) %>%
  rename(
    State = 'Province/State',
    Country = 'Country/Region',
    deaths = value,
    Date = variable
  ) %>%
  select(State, Country, Date, deaths)  %>% mutate_if(is.factor, as.character) %>%
  group_by(Country,Date) %>% summarise(deaths=sum(deaths))

#recovered
raw.data.recovered <-
  read_csv(file.path('covid-app','data','raw_data', 'time_series_covid19_recovered_global.csv'),
           na = "") %>%
  melt(id.vars = c('Province/State', 'Country/Region', 'Lat', 'Long')) %>%
  rename(
    State = 'Province/State',
    Country = 'Country/Region',
    recovered = value,
    Date = variable
  ) %>%
  select(State, Country, Date, recovered)  %>% mutate_if(is.factor, as.character) %>%
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


saveRDS(combined.data, file.path('covid-app','data', 'clean_long_data.rds'))
#save clean  data
#write_csv(combined.data, file.path('covid-app','data', 'clean_long_data.csv'),na="")
