library(magrittr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stargazer)
library(car)
library(moments)

basedir <- '/Users/chwassme/Dropbox/dev/edu.self/cas/010_Kurse/005-LinReg/Transferbericht/data/';
# Datum wird nicht als Date oder POSIXct erkannt (Zeit wird abgeschnitten)

# Motorisierter Verkehr
trafficMotorFile <- paste0(basedir, 'sid_dav_verkehrszaehlung_miv_OD2031_2021.csv');
# unzip(trafficMotor)
rawTrafficMotor <- read.csv(trafficMotorFile, header = TRUE, stringsAsFactors = TRUE, colClasses = c("MSID" = "character", "MessungDatZeit" = "character", "LieferDat" = "character"))
str(rawTrafficMotor)
summary(rawTrafficMotor)
head(rawTrafficMotor, 10)

# Fehlende Werte: NA
trafficMotor <- rawTrafficMotor %>%
  select(MSID, ZSID, Achse, AnzDetektoren, MessungDatZeit, AnzFahrzeuge, AnzFahrzeugeStatus, Richtung) %>%
  mutate(MDateTime = as.POSIXlt(MessungDatZeit, format = "%Y-%m-%dT%H:%M:%S")) %>%
  mutate(MHour = MDateTime$hour, MYear = MDateTime$year + 1900) %>%
  mutate(MMonth = MDateTime$mon + 1, MWeekday = MDateTime$wday, MWeekOfYear = isoweek(MDateTime)) %>%
  select(-MessungDatZeit)

# Fehlende Daten
trafficMotorSummary <- trafficMotor %>%
  group_by(MSID, AnzFahrzeugeStatus) %>%
  summarise(n = n()) %>%
  pivot_wider(names_from = AnzFahrzeugeStatus, values_from = n, values_fill = 0) %>%
  mutate(TotalDaten = Fehlend + Imputiert + Gemessen, FehlendOderImputiertAbs = Fehlend + Imputiert) %>%
  mutate(FehlendOderImputiertRel = FehlendOderImputiertAbs / TotalDaten) %>%
  mutate(ImputiertRel = Imputiert / TotalDaten, FehlendRel = Fehlend / TotalDaten) %>%
  arrange(-FehlendRel)

hist(trafficMotorSummary$FehlendRel, breaks = 100)
quantile(trafficMotorSummary$FehlendRel, probs = c(0.88, 0.89, 0.9, 0.95, 0.99))

# do not allow more than 5% missing data
thresholdMissingData <- 0.05
validMSIDs <- trafficMotorSummary %>%
  filter(FehlendRel < thresholdMissingData) %>%
  select(MSID)

invalidMSIDs <- trafficMotorSummary %>%
  filter(FehlendRel >= thresholdMissingData) %>%
  select(MSID)
nrow(invalidMSIDs)

nrow(validMSIDs) / length(unique(trafficMotor$MSID))

# Filter data by MSID

cMSIDS <- c(validMSIDs)
cMSIDS <- c('a', 'b')
class(cMSIDS)
typeof(cMSIDS)

# validMSIDs <- as.data.frame(rbind("Z001M001", "Z001M002", "Z002M001", "Z002M002"))
# names(validMSIDs) <- 'MSID'

trafficMotorByMSID <- trafficMotor %>%
  inner_join(validMSIDs, by = 'MSID') %>%
  filter(AnzFahrzeugeStatus %in% c('Gemessen', 'Imputiert')) %>%
  pivot_wider(names_from = MSID, values_from = AnzFahrzeuge, values_fill = 0, id_cols = c(MDateTime))

head(trafficMotorByMSID, 10)
head(trafficMotor)

lmTrafficMotorByMSID <- lm(Z001M001 ~ ., trafficMotorByMSID %>% select(-MDateTime))
summary(lmTrafficMotorByMSID)

layout(matrix(1:4, 2, 2))  # zusammen mit plot ausführen
plot(lmTrafficMotorByMSID)
qqPlot(summary(lmTrafficMotorByMSID)$residuals)
jarque.test(summary(lmTrafficMotorByMSID)$residuals)

dim(trafficMotorValid)
dim(trafficMotor)

c(validMSIDs[validMSIDs == 'Z057M002'])

### next steps
# - Daten nach MSID filtern
# - pivot_wide MSID
# - lm(MSID)


# Fussgaenger/Velo
trafficPedBikeFile <- paste0(basedir, '2021_verkehrszaehlungen_werte_fussgaenger_velo.csv')
rawTrafficPedBike <- read.csv(trafficPedBikeFile, header = TRUE, stringsAsFactors = TRUE, colClasses = c("DATUM" = "character"))
str(rawTrafficPedBike)
summary(rawTrafficPedBike)
head(rawTrafficMotor, 10)

# Meteo
meteoFile <- paste0(basedir, 'ugz_ogd_meteo_h1_2021.csv')
rawMeteo <- read.csv(meteoFile, header = TRUE, stringsAsFactors = TRUE, colClasses = c("Datum" = "character"))
str(rawMeteo)
summary(rawMeteo)
head(rawMeteo, 10)
unique(rawMeteo$Parameter)

# Luftqualitaet
airFile <- paste0(basedir, 'ugz_ogd_air_h1_2021.csv')
rawAir <- read.csv(airFile, header = TRUE, stringsAsFactors = TRUE, colClasses = c("Datum" = "character"))
str(rawAir)
summary(rawAir)
head(rawAir, 10)
unique(rawAir$Parameter)

# Unfall
accidentsFile <- paste0(basedir, 'KTZH_00000718_00001783.csv')
rawAccident <- read.csv(accidentsFile, header = TRUE, sep = ';', stringsAsFactors = TRUE, colClasses = c("AccidentUID" = "character"))
str(rawAccident)
summary(rawAccident)
head(rawAccident, 10)
nrow(rawAccident) # 147971


accidentData <- rawAccident %>%
  select(AccidentUID, AccidentType, AccidentSeverityCategory, RoadType, MunicipalityCode, AccidentYear, AccidentMonth, AccidentWeekDay, AccidentHour) %>%
  mutate(YearMonth = paste(AccidentYear,sprintf("%02.f", AccidentMonth), sep = '-'))

head(accidentData)

accidentData %>%
    pivot_wider(names_from = AccidentType, values_from = gr, values_fill = 0, id_cols = c(MDateTime))

table(accidentData$AccidentMonth, accidentData$AccidentHour)


# circa 1000 Unfaelle pro Monat, Total 150k
accidentByMonth <- rawAccident %>%
  group_by(AccidentYear, AccidentMonth) %>%
  summarise(TotalAccidents = n())
accidentByMonth
View(accidentByMonth)

mtcars

#  Unfall mit Sachschaden
#  Unfall mit Leichtverletzten
#  Unfall mit Schwerverletzten
#  Unfall mit Getöteten
unique(rawAccident$AccidentSeverityCategory_de)

# [1] Schleuder- oder Selbstunfall           Überqueren der Fahrbahn                Abbiegeunfall
# [4] Auffahrunfall                          Parkierunfall                          Überholunfall oder Fahrstreifenwechsel
# [7] Fussgängerunfall                       Einbiegeunfall                         Tierunfall
# [10] Andere                                Frontalkollision
unique(rawAccident$AccidentType_de)

# Hauptstrasse
# Nebenstrasse
# Autobahn
# andere
# Autostrasse
# Nebenanlage
unique(rawAccident$RoadType_de)

#AccidentInvolvingPedestrian, AccidentInvolvingBicycle, AccidentInvolvingMotorcycle

# Gemeindedaten
communeFile <- paste0(basedir, 'gemeindedaten.csv')
communeRaw <- read.csv(communeFile, encoding = "UTF-8", na.strings = c("*", NA)) %>% # mark missing values as NA (instead of '*')
  rename_with(., ~gsub("\\.", "", .x))
str(communeRaw)
summary(communeRaw)
head(communeRaw, 10)
