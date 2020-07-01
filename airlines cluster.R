#Hierarcheal Clustering
install.packages("readr")
library(readr)
mydata <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\3 - Clustering\\EastWestAirlines.csv")
View(mydata)
sum(is.na(mydata))

summary(mydata)
head(mydata)
str(mydata)

#Scatter plot
pairs(mydata)
plot(mydata$Murder~ mydata$Assault, data = mydata)
with(mydata,text(mydata$Murder ~ mydata$Assault, labels=mydata$X,pos=1))

#Normalize data
normalized_data <- scale(mydata[,-c(1, 12)])
head(normalized_data)
View(normalized_data)

#Distance Matrix
distance <- dist(normalized_data, , method = "euclidean")
distance

#Hierarcheal Clustering Using Complete Linkage
mydata_hclust <- hclust(distance, method = "complete")
plot(mydata_hclust)
plot(mydata_hclust, hang = 1)
plot(mydata_hclust,labels=mydata$X,main='Default from hclust')

#Hierarcheal 
#Clustering Using Ward Linkage
mydata_hclust_ward <- hclust(distance, method = "ward.D")
plot(mydata_hclust_ward, hang = 1)
rect.hclust(mydata_hclust_ward, k=3, border = "red")

#Cluster Membership
member <- cutree(mydata_hclust_ward, k=3)
membership <- as.matrix(member)
table(membership)

#Characterising Cluster
aggregate(normalized_data, list(member), mean)
aggregate(mydata[,-c(1,1)], list(member), mean)

#final Data
final <- cbind(mydata, membership)
View(final)

##---------------------------------------------------------------------------------------------------------

#K Means Cluster
head(normalized_data)

#nrow(normalized_data)-1 = number of rowsd
wss = (nrow(normalized_data)-1)*sum(apply(normalized_data, 2, var))
for (i in 2:8) {
  wss[i] = sum(kmeans(normalized_data, centers = i)$withinss)
  print(wss[i])
}
plot(1:8, wss, type = "b", xlab="Number of Clusters", ylab="Within group sum of squares")
title(sub = "K-Means Clustering Screw-Plot")

#From the elbow curve, k= 3 or 5
fit1 <- kmeans(normalized_data, 3)
fit1$cluster
fit1$centers
fit2 <- kmeans(normalized_data, 5)

#Cluster Plot
install.packages("ggfortify")
install.packages("factoextra")
library(ggfortify)
library(factoextra)
autoplot(fit2,normalized_data,frame=T)
fviz_silhouette(fit1)

#Final View
table(fit1$cluster)
table(fit2$cluster)
final2<- data.frame(mydata, fit1$cluster)
View(final2)





