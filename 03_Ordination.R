setwd("/home/sarah/Downloads")
ordination<-read.csv("ordination_data.csv")

#loading libraries
library(vegan)
library(ggordiplots)

#running ordination
str(ordination)
rownames(ordination)<-ordination[,1]
#running ordination
pca1<-rda(ordination[complete.cases(ordination) ,-c(1,8)], scale = TRUE) #no NAs or chararcters
biplot(pca1, display = c("sites", "species"), type = c("text","points"))
vegan::ordihull(pca1,
         group = as.factor(ordination[complete.cases(ordination),]$disturbance), col = c(1:3))
legend("topright", col = c(1,2,3), lty = 1, legend = c("Pulse", "Press", "Both"))
