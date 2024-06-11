rm(list = ls())
setwd("C:/Users/micah/OneDrive/Documents/R/Wine clustering")

data1 <- read.csv("~/R/Wine clustering/wine-clustering.csv")

dim(data1)

#check for missing data
sum(is.na(data1))

####Principal component analysis ####

pr.out <- prcomp(data1, scale=TRUE)
summary(pr.out)

library(ggbiplot)
ggbiplot(pr.out,obs.scale = 1, var.scale = 1,
         circle = TRUE,
         var.axes = TRUE) + 
  theme_minimal() +
  ggtitle('PCA Biplot of Wine Dataset (Standardized)')

#Alternative 
#options(repr.plot.width = 8, repr.plot.height = 6)  #Adjust plot size
#biplot(pr.out,scale = 0, col = c("blue", "red"),cex=0.8)

pr.var <- pr.out$sdev^2 #find variance from standard deviation
pve <- pr.var/sum(pr.var) #compute the proportion of variance explained

#scree plots
plot(pve, xlab="Principal components",ylab="Proportion of variance explained",
     ylim=c(0,1),main = "Proportion of variance explained with each component", cex.main = 0.9, type="b")

#Extract the first 3 Principal components
Newdata <- as.data.frame(pr.out$x[,1:3])
head(Newdata)
dim(Newdata)

#CLUSTERING 

#Hierarchical clustering 
data.dist=dist(Newdata) #for Euclidean distance calculations
plot(hclust(data.dist),main="Complete Linkage",xlab ="",sub="",ylab ="")
abline(h=8,col="red") #therefore we can consider 3 clusters

#K-means Clustering 
set.seed(2)
km.out <- kmeans(Newdata,3,nstart =20)
km.clusters <-  km.out$cluster

# Add cluster assignments to the biplot
clusters <- as.factor(km.clusters)
# Create a PCA biplot with cluster colors
library(ggplot2)
ggplot(Newdata, aes(x = PC1, y = PC2, color = clusters)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("blue", "green", "red")) +  # Customize cluster colors
  labs(x = "PC1", y = "PC2", color = "Cluster") +
  ggtitle("Clustered Principal components")

##alternatively using ggplot 
plot(pr.out$x[,1:2],col=(km.clusters+1),pch=20,cex=2,main="PC1 and PC2 Biplot")
plot(pr.out$x[,c(1,3)],col=(km.clusters+1),pch=20,cex=2, main="PC1 and PC3 Biplot")

#extra cool plot 
ggbiplot(pr.out,obs.scale = 1, var.scale = 1,
         groups = clusters, ellipse = TRUE, 
         circle = TRUE, var.axes = TRUE) + 
  theme_minimal() +
  theme(legend.position = 'bottom') +
  ggtitle('PCA Biplot of Wine Dataset (Standardized)')



