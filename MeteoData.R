library(dplyr)
library(tidyr)

# check if data exists in directory
if(!file.exists('./data/MeteoDataZurich.Rda')){
  # create empty dataframe
  MeteoData.df <- data.frame()
  # get values for 2011 - 2021
  for(i in 2011:2021){
    url <- paste("https://data.stadt-zuerich.ch/dataset/ugz_meteodaten_stundenmittelwerte/download/ugz_ogd_meteo_h1_", as.character(i), ".csv", sep = '')
    data <- read.csv(url, encoding = 'UTF-8')
    df <- data.frame(data) %>%
      mutate(Year = as.numeric(substr(X.U.FEFF.Datum, 1, 4))) %>%
      mutate(Month = as.numeric(substr(X.U.FEFF.Datum, 6, 7))) %>%
      mutate(Day = as.numeric(substr(X.U.FEFF.Datum, 9, 10))) %>%
      mutate(Hour = as.numeric(substr(X.U.FEFF.Datum, 12, 13))) %>%
      pivot_wider(names_from = Parameter, values_from = Wert) %>%
      select(Year, Month, Day, Hour, T, RainDur) %>%
      group_by(Year, Month, Day, Hour) %>%
      summarise(Temperature = mean(T, na.rm = TRUE), RainDuration = mean(RainDur, na.rm = TRUE)) %>%
      ungroup %>%
      select(Year, Month, Day, Temperature, RainDuration) %>%
      group_by(Year, Month, Day) %>%
      summarise(Temperature = mean(Temperature, na.rm = TRUE), RainDuration = sum(RainDuration, na.rm = TRUE))
    MeteoData.df <- rbind(MeteoData.df, df)
  }
  # save data to Rda file
  save(MeteoData.df, file = './data/MeteoDataZurich.Rda')
} else {
  load('./data/MeteoDataZurich.Rda')
}

# check for na values
any(is.na(MeteoData.df$Temperature))
any(is.na(MeteoData.df$RainDuration))

# create time series and plot components
Temperatures.ts <- ts(MeteoData.df$Temperature, start = c(2010,1,1), frequency = 365)
plot(Temperatures.ts, type = 'l', col = 'red')
Temperatures.ts.a <- decompose(Temperatures.ts, type = 'additive')
plot(Temperatures.ts.a )

RainDuration.ts <- ts(MeteoData.df$RainDuration, start = c(2010,1,1), frequency = 365)
plot(RainDuration.ts, type = 'l', col = 'red')
RainDuration.ts.a <- decompose(RainDuration.ts, type = 'additive')
plot(RainDuration.ts.a )

# EOF