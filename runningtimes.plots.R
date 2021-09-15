library(ggplot2)

#Runningtime table creation
#--------------------------------------------------------------------------------------------------------

movementData <- subset(data, Omschrijving == "Brug Beweging Actief")
movementData$Tijd <- as.POSIXct(movementData$Tijd, format="%d-%m-%Y %H:%M:%OS")
movementData <- movementData[order(movementData$Tijd),]

formattedData <- data.frame(assetId=character(),
                            openingStart=character(),
                            open=character(),
                            closingStart=character(),
                            closed=character(),
                            stringsAsFactors = FALSE)

op <- options(digits.secs = 3)

formattedData$openingStart <- as.POSIXct(formattedData$openingStart , format="%d-%m-%Y %H:%M:%OS")
formattedData$open <- as.POSIXct(formattedData$open , format="%d-%m-%Y %H:%M:%OS")
formattedData$closingStart <- as.POSIXct(formattedData$closingStart , format="%d-%m-%Y %H:%M:%OS")
formattedData$closed <- as.POSIXct(formattedData$closed , format="%d-%m-%Y %H:%M:%OS")


for(i in 1:dim(movementData)[1]){
  if(i %% 4 == 0){
    formattedData[i/4,1] <- movementData[i-3,1]
    formattedData[i/4,2] <- movementData[i-3,5]
    formattedData[i/4,3] <- movementData[i-2,5]
    formattedData[i/4,4] <- movementData[i-1,5]
    formattedData[i/4,5] <- movementData[i,5]
  }
}

bridgeMovementData <- formattedData

opened <- c()
closed <- c()
total.time <- c()


for (i in 1:dim(bridgeMovementData)[1]){
  opened[i] <- bridgeMovementData[i,3] - bridgeMovementData[i,2]
  closed[i] <- bridgeMovementData[i,5] - bridgeMovementData[i,4]
  total.time[i] <- bridgeMovementData[i,5] - bridgeMovementData[i,2]
}

bridgeMovementData[,6] <- opened
bridgeMovementData[,7] <- closed
bridgeMovementData[,8] <- total.time


#All data selected (skewed plot)
ggplot(bridgeMovementData, aes(assetId, V8, fill = assetId)) +
  geom_boxplot() +
  scale_y_continuous(breaks = round(seq(min(counts$value), 200, by = 5),1)) +
  ylab("gesloten - open - gesloten (in minuten)")

#Manual selected data
ggplot(backup, aes(assetId, V8, fill = assetId)) +
  geom_boxplot() +
  scale_y_continuous(breaks = round(seq(min(counts$value), 75, by = 5),1)) +
  ylab("gesloten - open - gesloten (in minuten)")



#Total running hours per object
#--------------------------------------------------------------------------------------------------------
runninghours.data <- subset(data_apr_2018, Omschrijving == "Object DraaiUren Float")
asset.names <- assets$assetId
asset.hours <- c()

for(i in 1:length(asset.names)){
  hours.per.asset <- subset(runninghours.data, sourcefolder == asset.names[i])
  hours.per.asset$Waarde <- as.numeric(hours.per.asset$Waarde)
  hours.per.asset <- hours.per.asset[order(hours.per.asset$Waarde, decreasing = T),]
  asset.hours[i] <- hours.per.asset$Waarde[1]
}

runninghours <- data.frame(asset.names, asset.hours)

ggplot(runninghours) +
  geom_bar(aes(runninghours$asset.names, runninghours$asset.hours), stat = 'identity')




