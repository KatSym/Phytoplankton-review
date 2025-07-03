setwd("/home/sarah/Downloads")
ordination<-read.csv("ordination_data.csv")

#loading libraries
library(vegan)
library(ggordiplots)

#running ordination
str(ordination)
rownames(ordination)<-ordination[,1]
#running ordination
#without disturbance
pca1<-rda(ordination[complete.cases(ordination) ,-c(1,8,11)], scale = TRUE) #no NAs or chararcters
#without system
pca2<-rda(ordination[complete.cases(ordination) ,-c(1:2,8)], scale = TRUE)
pdf("OrdinationPlots.pdf")
biplot(pca1, display = c("sites", "species"), type = c("text","points"))
vegan::ordihull(pca1,
         group = as.factor(ordination[complete.cases(ordination),]$disturbance), col = c(1:3))
legend("topright", col = c(1,2,3), lty = 1, legend = c("Pulse", "Press", "Both"))
biplot(pca2, display = c("sites", "species"), type = c("text","points"))
vegan::ordihull(pca2,
         group = as.factor(ordination[complete.cases(ordination),]$system), col = c(2,2,2,2,2,2,1,1,1,1,1))
legend("topleft", col = c(rep("black", 6), rep("red", 5)) , lty = 1, legend = c("Shallow Lake", "Pond", "Lake", "Deep Lake", "Alpine Lake", "Wetland", "Brackish", "Estuary", "Lagoon", "Coastal", "OffShore")) 
dev.off()

#showing hulls for systems

