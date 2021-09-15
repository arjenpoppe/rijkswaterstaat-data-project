



wind.direction.subset <- subset(data_feb_2018, Omschrijving == "Meteo Gemiddelde Windrichting" & sourcefolder == "12 WGR")
wind.strength.subset <- subset(data_feb_2018, Omschrijving == "Meteo Gemiddelde Windkracht" & sourcefolder == "12 WGR")
temperature.subset <- subset(data_feb_2018, Omschrijving == "Meteo Actuele Temperatuur" & sourcefolder == "12 WGR")


library(ggplot2)
harMet.daily <- read.csv(
  file="D:/Downloads/NEONDSMetTimeSeries/NEON-DS-Met-Time-Series/HARV/FisherTower-Met/hf001-06-daily-m.csv",
  stringsAsFactors = FALSE
)

