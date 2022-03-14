library(magrittr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stargazer)
library(car)
library(moments)


basedir <- '/Users/chwassme/Dropbox/dev/edu.self/cas/010_Kurse/005-LinReg/Transferbericht/js/data/';

# Garmin: Heartrate
heartRateFile <- paste0(basedir, 'heartrate.csv');
heartRateRaw <- read.csv(heartRateFile, header = TRUE, sep = ';') %>%
  mutate(calendarDateObj = as.Date(calendarDate)) %>%
  select(-calendarDate)

str(heartRateRaw)
summary(heartRateRaw)
head(heartRateRaw, 15)

# Garmin: Sleep
sleepFile <- paste0(basedir, 'sleep.csv');
sleepRaw <- read.csv(sleepFile, header = TRUE, sep = ';') %>%
  mutate(calendarDateObj = as.Date(calendarDate)) %>%
  select(-calendarDate) %>% # not need anymore
  filter(sleepWindowConfirmed == 'true') %>%  # remove unconfirmed sleep windows
  select(-retro, -deviceRemCapable, -sleepQualityTypePK, -sleepResultTypePK, -userProfilePK, -id, -napTimeSeconds, -sleepWindowConfirmed) %>% # remove columns not needed / all equal
  select(-sleepStartTimestampLocal, -sleepEndTimestampLocal) %>% # remove local dates
  mutate(sleepStartLocal = as.POSIXlt(sleepStartTimestampGMT / 1000, origin = "1970-01-01", tz = "CET")) %>%
  mutate(sleepEndLocal = as.POSIXlt(sleepEndTimestampGMT / 1000, origin = "1970-01-01", tz = "CET")) %>%
  mutate(autoSleepStartLocal = as.POSIXlt(autoSleepStartTimestampGMT / 1000, origin = "1970-01-01", tz = "CET")) %>%
  mutate(autoSleepEndLocal = as.POSIXlt(autoSleepEndTimestampGMT / 1000, origin = "1970-01-01", tz = "CET")) %>%
  select(-sleepStartTimestampGMT, -sleepEndTimestampGMT, -autoSleepStartTimestampGMT, -autoSleepEndTimestampGMT) %>%
  mutate(remRel = remSleepSeconds / sleepTimeSeconds) %>%
  mutate(lightRel = lightSleepSeconds / sleepTimeSeconds) %>%
  mutate(deepRel = deepSleepSeconds / sleepTimeSeconds)

tail(sleepRaw, 6)
head(sleepRaw, 6)
dim(sleepRaw)
str(sleepRaw)
summary(sleepRaw)
unique(sleepRaw$sleepEndTimestampGMT)
unique(sleepRaw$deviceRemCapable)
unique(sleepRaw$napTimeSeconds)

# Garmin: Stairs
stairsFile <- paste0(basedir, 'stairs.csv');
stairsRaw <- read.csv(stairsFile, header = TRUE, sep = ';') %>%
  mutate(calendarDateObj = as.Date(calendarDate)) %>%
  select(-calendarDate)

str(stairsRaw)
summary(stairsRaw)
head(stairsRaw, 15)

# Garmin: Stress
stressFile <- paste0(basedir, 'stress.csv');
stressRaw <- read.csv(stressFile, header = TRUE, sep = ';') %>%
  mutate(calendarDateObj = as.Date(calendarDate)) %>%
  select(-calendarDate)

str(stressRaw)
summary(stressRaw)
head(stressRaw, 15)

# Garmin: Steps
stepsFile <- paste0(basedir, 'steps.csv');
stepsRaw <- read.csv(stepsFile, header = TRUE, sep = ';')
str(stepsRaw)
summary(stepsRaw)
head(stepsRaw, 100)
tail(stepsRaw, 10)

stepsWithDate <- stepsRaw %>%
  # head(10) %>%
  mutate(startCET = as.POSIXlt(as.POSIXct(startGMT, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT"), tz = "CET")) %>%
  mutate(endCET = as.POSIXlt(as.POSIXct(endGMT, format = "%Y-%m-%dT%H:%M:%S", tz = "GMT"), tz = "CET")) %>%
  select(-startGMT, -endGMT) %>%
  mutate(calendarDateObj = ymd(paste(startCET$year + 1900, startCET$mon + 1, startCET$mday, sep = '-')))

stepsByDate <- stepsWithDate %>%
  group_by(calendarDateObj) %>%
  summarise(totalSteps = sum(steps)) %>%
  mutate(totalSteps1000 = totalSteps / 1000)

# stepsByDateActivityLevel <- stepsWithDate %>%
#   pivot_wider(names_from = primaryActivityLevel, values_from = steps , values_fn = sum, values_fill = 0, id_cols = c(date))
#
# stepsByDateActivityLevelConstant <- stepsWithDate %>%
#   pivot_wider(names_from = c(primaryActivityLevel, activityLevelConstant), values_from = steps , values_fn = sum, values_fill = 0, id_cols = c(date))


# join data
data <- stepsByDate %>%
  full_join(heartRateRaw, by = 'calendarDateObj') %>%
  full_join(sleepRaw, by = 'calendarDateObj') %>%
  full_join(stairsRaw, by = 'calendarDateObj') %>%
  full_join(stressRaw, by = 'calendarDateObj') %>%
  filter(!is.na(sleepTimeSeconds), sleepTimeSeconds > 0) # remove days without sleep


dim(data)
View(data)
head(data)
str(data)

# regression
attributes(data)
lmDeep <- lm(deepRel ~ totalSteps + minHeartRate + restingHeartRate + lastSevenDaysAvgRestingHeartRate + nrOfFloorsClimbed + avgStressLevel, data)
lmLight <- lm(lightRel ~ totalSteps + minHeartRate + restingHeartRate + lastSevenDaysAvgRestingHeartRate + nrOfFloorsClimbed + avgStressLevel, data)
lmRem <- lm(remRel ~ totalSteps + minHeartRate + restingHeartRate + lastSevenDaysAvgRestingHeartRate + nrOfFloorsClimbed + avgStressLevel, data)
summary(lm)

lm2 <- lm(deepRel ~ totalSteps + avgStressLevel, data)
summary(lm2)
layout(matrix(1:4, 2, 2))  # zusammen mit plot ausführen
plot(lmRem)

lmHR <- lm(restingHeartRate ~ totalSteps + nrOfFloorsClimbed + avgStressLevel, data)
summary(lmHR)

res <- as.data.frame(summary(lmLight)$residuals)
names(res) <- c('residuals')
head(sort(-summary(lmLight)$residuals))
quantile(summary(lmLight)$residuals, c(0.9, 0.95, 0.99))

hist(summary(lmLight)$residuals)

qqPlot(summary(lmLight)$residuals)
jarque.test(summary(lm)$residuals)

qqPlot(summary(lm2)$residuals)
jarque.test(summary(lm2)$residuals)

# Ernuechternd: Residuen sind nicht normaleverteilt

stargazer(lmDeep, lmLight, lmRem,
          digits = 2,
          digit.separator = "'",
          object.names = T,
          type = "text", single.row = T,
          intercept.bottom = F
)

trunc(Sys.time(), "day")


as.POSIXlt(as.POSIXct('2021-05-19T05:00:00.0', format = "%Y-%m-%dT%H:%M:%S", tz = "GMT"), tz = "CET")$hour
trunc(as.POSIXlt(as.POSIXct('2021-05-19T05:00:00.0', format = "%Y-%m-%dT%H:%M:%S", tz = "GMT"), tz = "CET"))$hour
as.POSIXlt(as.POSIXct('2021-05-19T23:00:00.0', format = "%Y-%m-%dT%H:%M:%S", tz = "GMT"), tz = "CET")$hour
as.POSIXlt('2021-05-20T04:15:00.0', format = "%Y-%m-%dT%H:%M:%S", tz = "GMT")$zone
as.POSIXlt('2021-05-20T04:15:00.0', format = "%Y-%m-%dT%H:%M:%S")$zone
as.POSIXlt('2021-05-20T04:15:00.0')$min
attributes(as.POSIXlt('2021-05-20T04:15:00.0'))
attributes(as.POSIXct('2021-05-20T04:15:00.0'))
as.POSIXct('2021-05-20T04:15:00.0')
ymd(paste(2022, 3, 15, '-'))