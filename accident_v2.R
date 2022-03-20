# load packages
library(stringr)
library(tidyr)
library(dplyr)
library(ggseas)
library(ggplot2)
library(ggcorrplot)

# set base data directory
Basedir <- "./data/"

# load accident data from csv file
AccidentsFile <- paste0(Basedir, 'KTZH_00000718_00001783.csv')
RawAccident <- read.csv(AccidentsFile, header = TRUE, sep = ';', stringsAsFactors = TRUE, colClasses = c("AccidentUID" = "character"))

# get some idea what the data looks like
str(RawAccident)
summary(RawAccident)
head(RawAccident, 10)
df_len <- nrow(RawAccident)

# select data from raw dataframe (only relevant data, no duplicates)
Accident <- RawAccident[c(6, 11, 12, 13, 14, 19, 20, 21, 23, 35)]

# remove language designator and "Accident" from header
headers <- colnames(Accident)
new_headers <- NULL
for(i in 1:length(headers)){
  tmp <- strsplit(headers[i], '_')[[1]][1]
  new_headers[i] <- gsub('Accident', '', tmp)
}
colnames(Accident) <- new_headers
 
# get time information from raw data
Month <- RawAccident$AccidentMonth
Year <- RawAccident$AccidentYear

# getting the actual day of the month is a little more complex (not in dataset)
Day <- 1
Weekday <- as.numeric(substring(RawAccident$AccidentWeekDay[1], 5))
for(i in 2:df_len){
  Weekday_old <- Weekday[i-1]
  Weekday[i] <- as.numeric(substring(RawAccident$AccidentWeekDay[i], 5))
  if(Weekday[i] != Weekday_old){
    Day[i] <- Day[i-1] + 1
  }
  else{
    Day[i] <- Day[i-1]
  }
  # restart day count if month changes
  if(Month[i] != Month[i-1]){
    Day[i] <- 1
  }
}

# add time information to dataframe 
Accident$Date <- as.numeric(Day*100000 + Month*10000 + Year)
Accident$YearMonth <- as.numeric(Month*10000 + Year)

# add more information to dataframe
Accident$Year <- Year
Accident$Month <- Month
Accident$Day <- Day
Accident$Weekday <- Weekday

# drop all data from 29th of Febrary to make data consistent
Accident <- Accident[!((Accident$Month==2) & (Accident$Day==29)),]

# change data type of some columns for easier processing
Accident$InvolvingPedestrian <- ifelse(Accident$InvolvingPedestrian == 'true', 1, 0)
Accident$InvolvingBicycle <- ifelse(Accident$InvolvingBicycle == 'true', 1, 0)
Accident$InvolvingMotorcycle <- ifelse(Accident$InvolvingMotorcycle == 'true', 1, 0)

# convert factors to numeric
AccidentTypes <- unique(Accident$Type)
levels(Accident$Type) <- 1:length(AccidentTypes)

AccidentSeverityCategory <- unique(Accident$SeverityCategory)
levels(Accident$SeverityCategory) <- 1:length(AccidentSeverityCategory)

AccidentRoadType <- unique(Accident$RoadType)
levels(Accident$RoadType) <- 1:length(AccidentRoadType)

Accident$Type <- as.numeric(as.character(Accident$Type))
Accident$SeverityCategory <- as.numeric(as.character(Accident$SeverityCategory))
Accident$RoadType <- as.numeric(as.character(Accident$RoadType))

# remove rows containing NA values from dataset
Accident <- drop_na(Accident)

# count accidents by date and plot timeseries
AccidentsByDate <- table(Accident$Date)
AccidentsByDate.ts <- ts(as.vector(AccidentsByDate), start = c(2011,01,01), frequency = 365)
ggplot(data = tsdf(AccidentsByDate.ts), aes(x = x, y = y)) +
  geom_line()

# some statistics to this new dataset
mean(AccidentsByDate)
hist(AccidentsByDate)

# count accidents by year and month and plot timeseries
AccidentsByYearAndMonth <- table(Accident$YearMonth)
AccidentsByYearAndMonth.ts <- ts(as.vector(AccidentsByYearAndMonth), start = c(2011,01), frequency = 12)
ggplot(data = tsdf(AccidentsByYearAndMonth.ts), aes(x = x, y = y)) +
  geom_line()

# some statistics to this new dataset
mean(AccidentsByYearAndMonth.ts)

# add some more info to this dataset
AYM.df <- data.frame(AccidentsByYearAndMonth)
colnames(AYM.df) <- c('MonthYear', 'Total')
AYM.df$MonthYear <- as.numeric(as.character(AYM.df$MonthYear))
AYM.df$Total <- as.numeric(as.character(AYM.df$Total))

AYM.df$Month <- as.integer(AYM.df$MonthYear / 10000)

AYM.df$Pedestrian <- table(Accident$YearMonth[Accident$InvolvingPedestrian == TRUE])
AYM.df$Bicycle <- table(Accident$YearMonth[Accident$InvolvingBicycle == TRUE])
AYM.df$Motorcycle <- table(Accident$YearMonth[Accident$InvolvingMotorcycle == TRUE])
AYM.df$Rest <- AYM.df$Total - AYM.df$Pedestrian - AYM.df$Bicycle - AYM.df$Motorcycle

# create groups by months
AYM.df.g <- AYM.df %>%
  group_by(Month) %>%
  summarise(Total = sum(Total), Pedestrian = sum(Pedestrian), Bicycle = sum(Bicycle), Motorcycle = sum(Motorcycle), Rest = sum(Rest))
  
# plot data which is now grouped by month
ggplot(AYM.df.g, aes(x = Month, y = Motorcycle)) + 
  geom_bar(stat="identity")

# correlation between different data columns (core data)
DataToCorrelate <- Accident[c(2, 3, 4, 5, 9)]
CorrelationMatrix <- cor(DataToCorrelate)
ggcorrplot(CorrelationMatrix, lab = TRUE) 

# EOF