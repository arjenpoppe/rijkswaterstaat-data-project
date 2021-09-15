library(ggplot2)
library(reshape)
library(scales)

#Used dataset
#------------------------------------------------------------------------------------
month <- "apr"
filename <- paste("_", month, sep = "")
klasse.dataset <- data_apr_2018

#Used subsets
#------------------------------------------------------------------------------------
nct.data <- subset(klasse.dataset, !is.na(klasse.dataset$Klasse) & sourcefolder == "00 BCT")[,7]
klasse.data <- subset(klasse.dataset, !is.na(klasse.dataset$Klasse) & sourcefolder != "00 BCT" & !is.na(klasse.dataset$sourcefolder))[,c(1,7)]

#Datapreparation
#------------------------------------------------------------------------------------
#Dataprep for malfunction.count and malfunction.distribution
klasse.count <- data.frame(unique(klasse.data[,1]))

klasse.table <- table(klasse.data)
column.names <- c("assetId")

for(i in 1:length(colnames(klasse.table))){
  asset.occurrence <- length(rownames(klasse.table))
  index.end <- i * asset.occurrence
  index.start <- index.end - (asset.occurrence - 1)
  klasse.count[column.names[i]] <- klasse.table[index.start:index.end] 
  column.names[i + 1] <- paste("Klasse", colnames(klasse.table)[i], sep = "")
}

colnames(klasse.count) <- column.names
counts <- melt(klasse.count, id.vars = "assetId")
breaks <- c("Klasse1","Klasse2","Klasse3","Klasse4","Klasse5","Klasse6","Klasse7")
labels <- c("urgent", "niet urgent", "proces en bediening", "onderhoud", "onderhoud bericht", "attentie", "bewaking")
counts$variable_releveled <- factor(counts$variable, levels = c("Klasse7","Klasse6","Klasse5","Klasse4","Klasse3","Klasse2","Klasse1"))

#Dataprep for malfunction.nct
malfunction.data <- data.frame(table(nct.data)) 
total <- sum(malfunction.data[1:4,2])
percentage <- c()

for (i in 1:dim(malfunction.data)[1]){
  percentage[i] <- (malfunction.data[i,2] / total) * 100
}

malfunction.data[,3] <- percentage
colnames(malfunction.data) <- c("Klasse", "aantal", "percentage")

#Plotting the plots
#------------------------------------------------------------------------------------
malfunction.distribution <- ggplot() + 
  geom_bar(data = counts, aes(x = assetId, y = value, fill = variable_releveled),stat = "identity", position = position_fill()) +
  xlab("assets") +
  ylab("verdeling van storingen") +
  ggtitle("Verdeling van storingen per object") +
  scale_y_continuous(labels = percent_format()) +
  guides(fill=guide_legend(title = NULL, reverse = TRUE)) +
  scale_fill_discrete(breaks=breaks, labels=labels)
  
malfunction.count <- ggplot(counts, aes(x = assetId, y = value, fill = variable_releveled)) + 
  geom_bar(stat = "identity") +
  xlab("assets") +
  ylab("aantal storingen") +
  ggtitle("Aantal storingen per object") +
  guides(fill=guide_legend(title = NULL, reverse = TRUE)) +
  scale_y_continuous(breaks = round(seq(min(counts$value), 10000, by = if(max(counts$value) > 1000) 100 else 50),1)) +
  scale_fill_discrete(breaks = breaks, labels=labels)

malfunction.nct <- ggplot(malfunction.data, aes(x="", y=percentage, fill=Klasse))+
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = aantal), size=3, position = position_stack(vjust = 0.5))  +
  coord_polar("y", start=0) +
  ylab("") +
  xlab("")

#Export the plots to a png file and sort them in the proper folder
#------------------------------------------------------------------------------------
main.dir <- "C:\\Users\\arjen\\OneDrive\\HBO-ICT\\Minor\\plots"
sub.dir <- month
output.dir <- paste(main.dir, "\\", sub.dir, sep = "")

dir.create(file.path(main.dir, sub.dir))
setwd(file.path(main.dir, sub.dir))

ggsave(paste("malfunction_distribution", filename, ".png", sep = ""), plot = malfunction.distribution, path = output.dir)
ggsave(paste("malfunction_count", filename, ".png", sep = ""), plot = malfunction.count, path = output.dir)
ggsave(paste("malfunction_nct", filename, ".png", sep = ""), plot = malfunction.nct, path = output.dir)


