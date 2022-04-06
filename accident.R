library(magrittr)
library(dplyr)
library(lubridate)
library(tidyr)
library(stargazer)
library(car)
library(moments)
library(stringr)
library(leaps)

basedir <- '/Users/chwassme/Dropbox/dev/edu.self/cas/010_Kurse/005-LinReg/Transferbericht/data/';

normalizeText <- function(x, length = 9) {
  res <- str_trim(x, side = 'both')
  res <- str_replace_all(res, ' ', '_')
  substr(res, 0, length)
}

# Unfall: heruntergeladen am 11.03.2022
accidentsFile <- paste0(basedir, 'KTZH_00000718_00001783.csv')
rawAccident <- read.csv(accidentsFile, header = TRUE, sep = ';', stringsAsFactors = TRUE, colClasses = c("AccidentUID" = "character"))
# str(rawAccident)
# summary(rawAccident)
# head(rawAccident, 10)
# nrow(rawAccident) # 147971

accidentData <- rawAccident %>%
  select(AccidentUID, AccidentType_de, AccidentSeverityCategory_de, RoadType_de, MunicipalityCode, AccidentYear, AccidentMonth, AccidentWeekDay, AccidentHour) %>%
  mutate(AccidentType_de = normalizeText(AccidentType_de)) %>%
  mutate(AccidentSeverityCategory_de = normalizeText(AccidentSeverityCategory_de, 20)) %>%
  mutate(RoadType_de = normalizeText(RoadType_de)) %>%
  mutate(YearMonth = paste(AccidentYear, sprintf("%02.f", AccidentMonth), sep = '-'))

head(accidentData)

accidentByType <- table(accidentData$YearMonth, accidentData$AccidentType) %>%
  as.data.frame() %>%
  pivot_wider(names_from = Var2, values_from = Freq, values_fill = 0) %>%
  rename(YearMonth = Var1)
tail(accidentByType, 12)

accidentBySeverity <- table(accidentData$YearMonth, accidentData$AccidentSeverityCategory) %>%
  as.data.frame() %>%
  pivot_wider(names_from = Var2, values_from = Freq, values_fill = 0) %>%
  rename(YearMonth = Var1)
tail(accidentBySeverity, 12)

accidentByRoadType <- table(accidentData$YearMonth, accidentData$RoadType) %>%
  as.data.frame() %>%
  pivot_wider(names_from = Var2, values_from = Freq, values_fill = 0) %>%
  rename(YearMonth = Var1)
tail(accidentByRoadType, 12)

accidentByHour <- table(accidentData$YearMonth, accidentData$AccidentHour) %>%
  as.data.frame() %>%
  pivot_wider(names_from = Var2, values_from = Freq, values_fill = 0) %>%
  rename(YearMonth = Var1)
tail(accidentByHour, 12)

accidentByWeekDay <- table(accidentData$YearMonth, accidentData$AccidentWeekDay) %>%
  as.data.frame() %>%
  pivot_wider(names_from = Var2, values_from = Freq, values_fill = 0) %>%
  rename(YearMonth = Var1)
tail(accidentByWeekDay, 12)

# circa 1000 Unfaelle pro Monat, Total 150k
accidentByMonth <- accidentData %>%
  group_by(YearMonth) %>%
  summarise(TotalAccidents = n())
tail(accidentByMonth, 12)

accidentFreq <- accidentByMonth %>%
  # full_join(accidentByHour, by = 'YearMonth') %>%
  full_join(accidentByWeekDay, by = 'YearMonth') %>%
  full_join(accidentByType, by = 'YearMonth') %>%
  full_join(accidentBySeverity, by = 'YearMonth') %>%
  full_join(accidentByRoadType, by = 'YearMonth')


#  as4: Unfall mit Sachschaden
#  as3: Unfall mit Leichtverletzten
#  as2: Unfall mit Schwerverletzten
#  as1: Unfall mit Getöteten
# unique(paste(rawAccident$AccidentSeverityCategory, rawAccident$AccidentSeverityCategory_de, sep = ': '))

# [1] "at0: Schleuder- oder Selbstunfall"           "at5: Überqueren der Fahrbahn"                "at3: Abbiegeunfall"
# [4] "at2: Auffahrunfall"                          "at7: Parkierunfall"                          "at1: Überholunfall oder Fahrstreifenwechsel"
# [7] "at8: Fussgängerunfall"                       "at4: Einbiegeunfall"                         "at9: Tierunfall"
# [10] "at00: Andere"                                "at6: Frontalkollision"

# "rt432: Hauptstrasse" "rt433: Nebenstrasse" "rt430: Autobahn"     "rt439: andere"       "rt431: Autostrasse"  "rt434: Nebenanlage"
# unique(paste(rawAccident$RoadType, rawAccident$RoadType_de, sep = ': '))

# TODO AccidentInvolvingPedestrian, AccidentInvolvingBicycle, AccidentInvolvingMotorcycle


# Hypothese / Untersuchungsgegenstand: welches sind die Einflussfaktoren von tödlichen Unfällen

accidents <- accidentFreq %>% select(-YearMonth, -TotalAccidents, -Unfall_mit_Sachschad, -Unfall_mit_Leichtver, -Unfall_mit_Schwerver)

# Interzeptmodell als Ausgangmodell
nullModel <- lm(Unfall_mit_Getöteten ~ 1, data = accidents)
fullModel <- lm(Unfall_mit_Getöteten ~ ., data = accidents)

# summary(fullModel)
N <- NROW(accidents)

# Step:  AIC=105.46
# Unfall_mit_Getöteten ~ Abbiegeunf + aw405 + Fussgänger + Tierunfall + aw406 + Autobahn + Frontalkol + aw407
forwardModelAIC <- step(nullModel,
                        direction = "forward",
                        scope = formula(fullModel), k = 2)

# Step:  AIC=105.46
# Unfall_mit_Getöteten ~ aw405 + aw406 + aw407 + Abbiegeunf + Frontalkol + Fussgänger + Tierunfall + Autobahn
backwardModelAIC <- step(fullModel,
                         direction = "backward",
                         scope = formula(fullModel), k = 2)

# Step:  AIC=105.46
# Unfall_mit_Getöteten ~ aw405 + aw406 + aw407 + Abbiegeunf + Frontalkol + Fussgänger + Tierunfall + Autobahn
bothModelAIC <- step(fullModel,
                     direction = "both",
                     data = accidents, k = 2)

# Step:  AIC=121.72
# Unfall_mit_Getöteten ~ Abbiegeunf + aw405
forwardModelBIC <- step(nullModel,
                        direction = "forward",
                        scope = formula(fullModel), k = log(N))
# Step:  AIC=121.72
# Unfall_mit_Getöteten ~ aw405 + Abbiegeunf
backwardModelBIC <- step(fullModel,
                         direction = "backward",
                         scope = formula(fullModel), k = log(N), )

# Step:  AIC=121.72
# Unfall_mit_Getöteten ~ aw405 + Abbiegeunf
bothModelBIC <- step(fullModel,
                     direction = "both",
                     data = accidents, k = log(N), trace = 0)

paste(AIC(forwardModelAIC), AIC(backwardModelAIC), AIC(bothModelAIC), sep = ", ")
paste(attr(bothModelAIC$terms,"term"), sep = ", ")

AIC(forwardModelAIC) + paste0(attr(bothModelAIC$terms,"term"))
paste0(AIC(forwardModelAIC), AIC(backwardModelAIC), AIC(bothModelAIC))
paste0(attr(bothModelAIC$terms,"term"))
names(summary(bothModelAIC))
names(bothModelAIC)
attr(,"term.labels")
attr(bothModelAIC$terms,"term")
finalModel <- lm(Unfall_mit_Getöteten ~
                   aw405 +
                     aw406 +
                     aw407 +
                     Abbiegeun +
                     Frontalko +
                     Fussgänge +
                     Tierunfal +
                     Autobahn, data = accidents)

BIC(finalModel)
BIC(bothModelAIC)
summary(finalModel)
layout(matrix(1:4, 2, 2))
plot(finalModel)
jarque.test(finalModel$residuals)


finalModelSmall <- lm(Unfall_mit_Getöteten ~ aw405 + Abbiegeunf, data = accidents)
summary(finalModelSmall)
layout(matrix(1:4, 2, 2))
plot(finalModelSmall)
jarque.test(finalModelSmall$residuals)

# Weitergehende Analysen
# TODO man koennte die Einfluss-Faktor-Kategorien (Wochentage, Stunde, Unfalltyp, Strasse) auch einzeln beobachten


# ### das gleiche mit den Rohdaten
# deadlyAccidents <- accidentData %>%
#   mutate(DeadlyAccident = AccidentSeverityCategory_de == 'Unfall_mit_Getöteten') %>%
#   select(-AccidentSeverityCategory_de, -AccidentMonth, -AccidentUID, -MunicipalityCode, -AccidentYear, -YearMonth, -AccidentHour)
#
# N <- nrow(deadlyAccidents)
# nullModelRaw <- lm(DeadlyAccident ~ 1, data = deadlyAccidents)
# fullModelRaw <- lm(DeadlyAccident ~ ., data = deadlyAccidents)
#
# # Step:  AIC=105.46
# # Unfall_mit_Getöteten ~ Abbiegeunf + aw405 + Fussgänger + Tierunfall + aw406 + Autobahn + Frontalkol + aw407
# forwardModelAIC <- step(nullModelRaw,
#                         direction = "forward",
#                         scope = formula(fullModelRaw), k = 2)
#
# # Step:  AIC=105.46
# # Unfall_mit_Getöteten ~ aw405 + aw406 + aw407 + Abbiegeunf + Frontalkol + Fussgänger + Tierunfall + Autobahn
# backwardModelAIC <- step(fullModelRaw,
#                          direction = "backward",
#                          scope = formula(fullModelRaw), k = 2)
#
# # Step:  AIC=105.46
# # Unfall_mit_Getöteten ~ aw405 + aw406 + aw407 + Abbiegeunf + Frontalkol + Fussgänger + Tierunfall + Autobahn
# bothModelAIC <- step(fullModelRaw,
#                      direction = "both",
#                      data = deadlyAccidents, k = 2)
#
# # Step:  AIC=121.72
# # Unfall_mit_Getöteten ~ Abbiegeunf + aw405
# forwardModelBIC <- step(nullModelRaw,
#                         direction = "forward",
#                         scope = formula(fullModelRaw), k = log(N))
#
# # Step:  AIC=121.72
# # Unfall_mit_Getöteten ~ aw405 + Abbiegeunf
# backwardModelCIC <- step(fullModelRaw,
#                          direction = "backward",
#                          scope = formula(fullModelRaw), k = log(N))
# # Step:  AIC=121.72
# # Unfall_mit_Getöteten ~ aw405 + Abbiegeunf
# bothModelBIC <- step(fullModelRaw,
#                      direction = "both",
#                      data = deadlyAccidents, k = log(N))

# ***

# Bei allen dreien (egal ob AIC oder BIC):
# DeadlyAccident ~ AccidentType_de

X <- model.matrix(fullModel)[, 2:ncol]

# sowas ist nicht nötig
ncol <- ncol(model.matrix(fullModel))
fullModelData <- cbind(as.data.frame(X), DeadlyAccidentBool = deadlyAccidents$DeadlyAccident)
fullModelLm <- lm(DeadlyAccidentBool ~ ., fullModelData)
summary(fullModelLm)

# Das beste Modell suchen mit regsubset... work very much in progress
subsets <- regsubsets(X, deadlyAccidents$DeadlyAccident,
                      nvmax = ncol - 1, nbest = 3
)
s.subsets <- summary(subsets,
                     nested = FALSE)
s.subsets$rsq
s.subsets$adjr2
s.subsets$bic
s.subsets$which

coef(subsets, 1:30)
vcov(subsets, 5)

?regsubsets
# rawAccident %>% filter(AccidentSeverityCategory == 'as1') %>% nrow()