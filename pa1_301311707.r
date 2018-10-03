#programming assignment 1
#Name: Branton Li
#student No: 301311707

library("stats")
library("cluster")
library("clusteval")
library("ggplot2")

##Task 1
wine <- read.csv(file = "wine.csv")
wine.data <- wine
wine.data$Wine <- NULL

##Task 2
#Z-score
m <- apply(wine.data,2,mean)
s <- apply(wine.data,2,sd)
wine.data <- scale(wine.data,m,s)

##Task 3
distance <- dist(wine.data)
sil_score <- function(center_num){
  k.wine <- kmeans(wine.data, centers = center_num)
  score <- silhouette(k.wine$cluster, distance)
  mean_score <- mean(score[,3])
  return(mean_score)
}
ss_4 <- sil_score(4)
print(ss_4)

##Task 4
k.wine <- kmeans(wine.data, centers = 4)
cluster <- k.wine$cluster

#Alcohol vs Flavanoids
#Mg vs OD
#Phenols vs Flavanoids
#Flavanoids vs Proline
#OD vs Proline

scat <- ggplot(data = wine, aes(x=OD, y= Proline, color=cluster)) + geom_point()
plot(scat)

##Task 5 
ss_list = c(2:10)
cluster_range = c(2:10)
for(i in 2:10){
  ss_list[i-1] <- sil_score(i)
}
print(ss_list)

plot(cluster_range, ss_list, main="Silhouette score vs # of Clusters", xlab="# of clusters"
     , ylab="silhouette score")

##Task 6
#complete linkage
hc <- hclust(distance)
plot(hc, label=wine$Wine, hang = -1, main="Complete-link")

#average
ha <- hclust(distance, method="average")
plot(ha, label=wine$Wine, hang = -1, main="Average-link")

#single
hs <- hclust(distance, method="single")
plot(hs, label=wine$Wine, hang = -1, main="Single-link")

##Task 7
new.hc <- cutree(hc,3)
new.ha <- cutree(ha,3)
new.hs <- cutree(hs,3)

##Task 8
k.best <- kmeans(wine.data, centers = 3)

sim_k <- cluster_similarity(wine$Wine, k.best$cluster, similarity = "rand")
print(sim_k)

sim_c <- cluster_similarity(wine$Wine, new.hc, similarity = "rand")
print(sim_c)

sim_a <- cluster_similarity(wine$Wine, new.ha, similarity = "rand")
print(sim_a)

sim_s <- cluster_similarity(wine$Wine, new.hs, similarity = "rand")
print(sim_s)
