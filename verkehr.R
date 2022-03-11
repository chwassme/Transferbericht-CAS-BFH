library(magrittr)
library(dplyr)

basedir <- '/Users/chwassme/Dropbox/dev/edu.self/cas/010_Kurse/005-LinReg/Transferbericht/R/data/';
# Datum wird nicht als Date oder POSIXct erkaennt (Zeit wird abgeschnitten)

# Motorisierter Verkehr
trafficMotor <- paste0(basedir, 'sid_dav_verkehrszaehlung_miv_OD2031_2021.csv.zip');
unzip(trafficMotor)
rawTrafficMotor <- read.csv(trafficMotor, header = TRUE, stringsAsFactors = TRUE, colClasses = c("MessungDatZeit"="character", "LieferDat"="character"))
str(rawTrafficMotor)
summary(rawTrafficMotor)
head(rawTrafficMotor, 10)

# Fussgaenger/Velo
trafficPedBike <- paste0(basedir, '2021_verkehrszaehlungen_werte_fussgaenger_velo.csv')
rawTrafficPedBike <- read.csv(trafficPedBike, header = TRUE, stringsAsFactors = TRUE, colClasses = c("DATUM"="character"))
str(rawTrafficPedBike)
summary(rawTrafficPedBike)
head(rawTrafficMotor, 10)

# Meteo
meteo <- paste0(basedir, 'ugz_ogd_meteo_h1_2021.csv')
rawMeteo <- read.csv(meteo, header = TRUE, stringsAsFactors = TRUE, colClasses = c("Datum"="character"))
str(rawMeteo)
summary(rawMeteo)
head(rawMeteo, 10)

# Luftqualitaet
air <- paste0(basedir, 'ugz_ogd_air_h1_2021.csv')
rawAir <- read.csv(air, header = TRUE, stringsAsFactors = TRUE, colClasses = c("Datum"="character"))
str(rawAir)
summary(rawAir)
head(rawAir, 10)

# Unfall
accidents <- paste0(basedir, 'KTZH_00000718_00001783.csv')
rawAccident <- read.csv(accidents, header = TRUE, sep = ';', stringsAsFactors = TRUE, colClasses = c("AccidentUID" = "character"))
str(rawAccident)
summary(rawAccident)
head(rawAccident, 10)
