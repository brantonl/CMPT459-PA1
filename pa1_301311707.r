#programming assignment 1
#Name: Branton Li
#student No: 301311707

library("stats")
library("cluster")
library("clusteval")
library("ggplot2")

##Task 1
wine <- read.csv("wine.csv")
wine.data = wine
wine.data$Wine <- NULL
#print(wine.data)

##Task 2
#Z-score
for(i in 1:nrow(wine.data)){
  wine.data$Alcohol[i] <- (wine.data$Alcohol[i] - mean(wine.data$Alcohol))/sd(wine.data$Alcohol)
  wine.data$Malic.acid[i] <- (wine.data$Malic.acid[i] - mean(wine.data$Malic.acid))/sd(wine.data$Malic.acid)
  wine.data$Ash[i] <- (wine.data$Ash[i] - mean(wine.data$Ash))/sd(wine.data$Ash)
  wine.data$Acl[i] <- (wine.data$Acl[i] - mean(wine.data$Acl))/sd(wine.data$Acl)
  wine.data$Mg[i] <- (wine.data$Mg[i] - mean(wine.data$Mg))/sd(wine.data$Mg)
  wine.data$Phenols[i] <- (wine.data$Phenols[i] - mean(wine.data$Phenols))/sd(wine.data$Phenols)
  wine.data$Flavanoids[i] <- (wine.data$Flavanoids[i] - mean(wine.data$Flavanoids))/sd(wine.data$Flavanoids)
  wine.data$Nonflavanoid.phenols[i] <- (wine.data$Nonflavanoid.phenols[i] - mean(wine.data$Nonflavanoid.phenols))/sd(wine.data$Nonflavanoid.phenols)
  wine.data$Proanth[i] <- (wine.data$Proanth[i] - mean(wine.data$Proanth))/sd(wine.data$Proanth)
  wine.data$Color.int[i] <- (wine.data$Color.int[i] - mean(wine.data$Color.int))/sd(wine.data$Color.int)
  wine.data$Hue[i] <- (wine.data$Hue[i] - mean(wine.data$Hue))/sd(wine.data$Hue)
  wine.data$OD[i] <- (wine.data$OD[i] - mean(wine.data$OD))/sd(wine.data$OD)
  wine.data$Proline[i] <- (wine.data$Proline[i] - mean(wine.data$Proline))/sd(wine.data$Proline)
}
#m <- apply(wine.data,2,mean)
#s <- apply(wine.data,2,sd)
#wine.data <- scale(wine.data,m,s)

##Task 3
k.wine <- kmeans(wine.data, centers = 4)
distance <- dist(wine.data)
plot(silhouette(k.wine$cluster, distance))
#average silhouette width: 0.22

##Task 4
plot(wine.data$Hue, wine.data$Phenols, main="Scatter Plot of Alcohol vs Proline"
     ,xlab="Alcohol", ylab="Proline")

##Task 5 
k.wine <- kmeans(wine.data, centers =2)
distance <- dist(wine.data)
plot(silhouette(k.wine$cluster, distance))
#silhouette score with diff centers [-1,1]
#2: 0.52
#3: 0.45
#4: 0.22
#5: 0.22
#6: 0.24
#7: 0.2
#8: 0.19
#9: 0.19

##Task 6
#complete linkage
hc <- hclust(distance)
plot(hc, label=wine$Wine, hang = -1)

#average
ha <- hclust(distance, method="average")
plot(ha, label=wine$Wine, hang = -1)

#single
hs <- hclust(distance, method="single")
plot(hs, label=wine$Wine, hang = -1)

##Task 7
new.hc <- cutree(hc,2)
new.ha <- cutree(ha,2)
new.hs <- cutree(hs,2)

##Task 8

