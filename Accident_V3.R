## setup
# load libraries required for this project
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(ggcorrplot)
library(GGally)


## private functions
# function to change column names (remove "Accident", whitespaces and language designator)
process_headers <- function(Headers){
  NewHeaders <- NULL
  for(i in 1:length(Headers)){
    tmp <- str_replace(Headers[i], '_', '')
    tmp <- str_replace(tmp, 'Accident', '')
    NewHeaders[i] <- str_replace(tmp, 'en', '')
  }
  return(NewHeaders)
}

# function to extract calendar day from available data
process_day <- function(Month, Weekday){
  Day <- 1
  for(i in 2:length(Weekday)){
    # increment day counter if weekday variable changes
    if(Weekday[i] == Weekday[i-1]){
      Day[i] <- Day[i-1]
    } else {
      Day[i] = Day[i-1] + 1
    }
    # reset if month variable changes
    if(Month[i] != Month[i-1]){
      Day[i] = 1
    }
  }
  return(Day)
}


## data pre-processing
# load raw data into workspace
Basedir <- "/Users/chwassme/Dropbox/dev/edu.self/cas/010_Kurse/005-LinReg/Transferbericht/data/"
AccidentFile <- paste0(Basedir, 'KTZH_00000718_00001783.csv')
RawAccident <- read.csv(AccidentFile, header = TRUE, sep = ';', stringsAsFactors = TRUE, colClasses = c("AccidentUID" = "character"))

meteoZurichFile <- paste0(Basedir, 'MeteoDataZurich.Rda')
load(meteoZurichFile)

# process raw data to extract information needed for this project
Accident.df <- RawAccident %>%
  select(6, 11, 12, 13, 14, 19, 20, 21, 23, 24, 25, 30, 35) %>%
  drop_na %>%
  rename_with(process_headers) %>%
  mutate(WeekDay = as.numeric(substr(WeekDay, 5, 5))) %>%
  mutate(Day = process_day(Month, WeekDay)) %>%
  mutate(InvolvingPedestrian = ifelse(InvolvingPedestrian == 'true', 1, 0)) %>%
  mutate(InvolvingBicycle = ifelse(InvolvingBicycle == 'true', 1, 0)) %>%
  mutate(InvolvingMotorcycle = ifelse(InvolvingMotorcycle == 'true', 1, 0)) %>%
  mutate(IsSevere = ifelse(grepl('severe', SeverityCategory) | grepl('fatalities', SeverityCategory), 1, 0)) %>%
  mutate(IsDeadly = ifelse(grepl('fatalities', SeverityCategory), 1, 0))

# add meteo information to dataframe
Accident.df <- merge(Accident.df, MeteoData.df, by=c('Year', 'Month', 'Day'))

# group data into daily sets
AccidentDaily.df <- Accident.df %>%
  select(Year, Month, Day, InvolvingPedestrian, InvolvingBicycle, InvolvingMotorcycle, IsSevere, IsDeadly, Temperature, RainDuration) %>%
  group_by(Year, Month, Day) %>%
  mutate(Total = n()) %>%
  summarise(Total = mean(Total), 
            InvolvingPedestrian = sum(InvolvingPedestrian), 
            InvolvingBicycle = sum(InvolvingBicycle), 
            InvolvingMotorcycle = sum(InvolvingMotorcycle),
            IsSevere = sum(IsSevere),
            IsDeadly = sum(IsDeadly),
            Temperature = mean(Temperature),
            RainDuration = mean(RainDuration)) %>%
  mutate(Date = as.Date(paste(as.character(Year), '-', as.character(Month), '-', as.character(Day), sep = ''), format = '%Y-%m-%d')) %>%
  select(Date, everything()) %>%
  ungroup

# further group data into monthly sets
AccidentMonthly.df <- AccidentDaily.df %>%
  group_by(Year, Month) %>%
  summarise(Total = sum(Total),
            InvolvingPedestrian = sum(InvolvingPedestrian), 
            InvolvingBicycle = sum(InvolvingBicycle), 
            InvolvingMotorcycle = sum(InvolvingMotorcycle),
            IsSevere = sum(IsSevere), 
            IsDeadly = sum(IsDeadly),
            Temperature = mean(Temperature),
            RainDuration = sum(RainDuration)) %>%
  mutate(Date = as.Date(paste(as.character(Year), '-', as.character(Month), '-01', sep = ''), format = '%Y-%m-%d')) %>%
  select(Date, everything()) %>%
  ungroup


## data exploration - histograms
# accidents vs. hour
hist(Accident.df$Hour)
hist(Accident.df$Hour[Accident.df$InvolvingPedestrian==TRUE])
hist(Accident.df$Hour[Accident.df$InvolvingBicycle==TRUE])
hist(Accident.df$Hour[Accident.df$InvolvingMotorcycle==TRUE])

# accidents vs. weekday
hist(Accident.df$WeekDay)
hist(Accident.df$WeekDay[Accident.df$InvolvingPedestrian==TRUE])
hist(Accident.df$WeekDay[Accident.df$InvolvingBicycle==TRUE])
hist(Accident.df$WeekDay[Accident.df$InvolvingMotorcycle==TRUE])

# accidents vs. month
hist(Accident.df$Month)
hist(Accident.df$Month[Accident.df$InvolvingPedestrian==TRUE])
hist(Accident.df$Month[Accident.df$InvolvingBicycle==TRUE])
hist(Accident.df$Month[Accident.df$InvolvingMotorcycle==TRUE])


## data exploration - correlation
# correlations in dataset AccidentsDaily.df
CorrDataDaily.df <- AccidentDaily.df %>%
  select(Total, InvolvingPedestrian, InvolvingBicycle, InvolvingMotorcycle, IsSevere, IsDeadly, Temperature)

CorrMatrixDaily <- cor(CorrDataDaily.df)  


# interessant waere auch die Correlation der Temperatur zu den anderen Unfallarten als Vergleich
ggcorrplot(CorrMatrixDaily, type = "lower", lab = TRUE)
#ggpairs(CorrDataDaily.df) # attention -> takes a very long time to visualize (large dataset)

# correlations in dataset AccidentsMonthly.df
CorrDataMonthly.df <- AccidentMonthly.df %>%
  select(Total, InvolvingPedestrian, InvolvingBicycle, InvolvingMotorcycle, IsSevere, IsDeadly, Temperature, RainDuration)

CorrMatrixMonthly <- cor(CorrDataMonthly.df)  

ggcorrplot(CorrMatrixMonthly, type = "lower", lab = TRUE)
ggpairs(CorrDataMonthly.df)


## data exploration - time series
# select some data to make ready to plot with ggplot
AccidentMonthly.plot.df <- AccidentMonthly.df %>%
  select(Date, InvolvingPedestrian, InvolvingBicycle, InvolvingMotorcycle) %>%
  pivot_longer(!Date, names_to = 'Involved', values_to = 'Values')

# plot data using ggplot
ggplot(AccidentMonthly.df, aes(x = Date, y = Total)) + 
  geom_line() + 
  ylab('Number of Accidents') + 
  ggtitle('Total Number of Monthly Accidents') + 
  theme_light()

ggplot(AccidentMonthly.plot.df, aes(x = Date, y = Values, color = Involved)) +
  geom_line() + 
  ylab('Number of Accidents') + 
  ggtitle('Monthly Number of Accidents involving pedestrians, bicycles and motorcycles') + 
  theme_light()
  
# create timeseries from monthly data
InvolvingBicycleMonthly.ts <- ts(AccidentMonthly.df$InvolvingBicycle, start=c(AccidentMonthly.df$Year[1], AccidentMonthly.df$Month[1]), frequency = 12)
InvolvingMotorcycleMonthly.ts <- ts(AccidentMonthly.df$InvolvingMotorcycle, start=c(AccidentMonthly.df$Year[1], AccidentMonthly.df$Month[1]), frequency = 12)

# decompose time series objects
InvolvingBicycleMonthly.ts.m <- decompose(InvolvingBicycleMonthly.ts, type = 'multiplicative')
plot(InvolvingBicycleMonthly.ts.m)  

InvolvingMotorcycleMonthly.ts.a <- decompose(InvolvingMotorcycleMonthly.ts, type = 'additive')
plot(InvolvingMotorcycleMonthly.ts.a)  


# EOF