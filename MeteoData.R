library(dplyr)
library(tidyr)

# check if data exists in directory
if(!file.exists('./data/TemperaturesZurich.Rda')){
  # create empty dataframe
  Temperatures.df <- data.frame()
  # get values for 2010 - 2021
  for(i in 2010:2021){
    url <- paste("https://data.stadt-zuerich.ch/dataset/ugz_meteodaten_stundenmittelwerte/download/ugz_ogd_meteo_h1_", as.character(i), ".csv", sep = '')
    data <- read.csv(url, encoding = 'UTF-8')
    df <- data.frame(data) %>%
      mutate(Year = as.numeric(substr(X.U.FEFF.Datum, 1, 4))) %>%
      mutate(Month = as.numeric(substr(X.U.FEFF.Datum, 6, 7))) %>%
      mutate(Day = as.numeric(substr(X.U.FEFF.Datum, 9, 10))) %>%
      mutate(Hour = as.numeric(substr(X.U.FEFF.Datum, 12, 13))) %>%
      filter(Einheit=='??C') %>%
      drop_na() %>%
      select(c(Wert, Year, Month, Day, Hour)) %>%
      group_by(Year, Month, Day) %>%
      summarise(Mean = mean(Wert))
    Temperatures.df <- rbind(Temperatures.df, df)
  }
  # save data to Rda file
  save(Temperatures.df, file = './data/TemperaturesZurich.Rda')
} else {
  load('./data/TemperaturesZurich.Rda')
}

# create time series and plot components
Temperatures.ts <- ts(Temperatures.df$Mean, start = c(2010,1,1), frequency = 365)
plot(Temperatures.ts, type = 'l', col = 'red')
Temperatures.ts.a <- decompose(Temperatures.ts, type = 'additive')
plot(Temperatures.ts.a )

# EOF