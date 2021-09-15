open.data.feb <- subset(data_feb_2018, Omschrijving == "Algemeen Commando Brug Openen" & Waarde == "TRUE")[,1]
open.data.mrt <- subset(data_mrt_2018, Omschrijving == "Algemeen Commando Brug Openen" & Waarde == "TRUE")[,1]

count.feb <- table(open.data.feb)
count.mrt <- table(open.data.mrt)

bridges <- assets[-c(1),]
bridges$opened_feb <- count.feb
bridges$opened_mrt <- count.mrt