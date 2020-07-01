#Hierarcheal Clustering
install.packages("readr")
library(readr)

crime_data <- read.csv("C:\\Users\\91755\\Desktop\\Assignment\\3 - Clustering\\crime_data.csv")
View(crime_data)
head(crime_data)

sum(is.na(crime_data))

str(crime_data)
summary(crime_data)
pairs(crime_data)

#Graphical View
plot(crime_data$Murder~crime_data$Assault, data = crime_data)
with(crime_data, text(crime_data$Murder~crime_data$Assault, labels=crime_data$X, pos=1))

#Normalized Data
normalized_data <- scale(crime_data[,-c(1)])
table(normalized_data)
head(normalized_data)
View(normalized_data)

#Distance Matrix
distance <- dist(normalized_data, method = "euclidean")
distance

#Hierarcheal Clustering Using Complete Linkage
mydata_complete <- hclust(distance, method = "complete")

plot(mydata_complete)
plot(mydata_hclust, hang=1)
plot(mydata.hclust,labels=mydata$X,main='Default from hclust')

#Hierarcheal Clustering Using Ward Linkage
mydata_ward <- hclust(distance, method = "ward.D")
plot(mydata_hclust_ward, hang = 1)
rect.hclust(mydata_hclust_ward, k=3, border = "red")

#Cluster Membership
member <- cutree(mydata_complete, k=3)
membership <- as.matrix(member)
table(membership)

#Characterizing clusters 
aggregate(normalized_data,list(member), mean)
aggregate(crime_data[,c(1)], list(member), mean)

#Final Data
final <- data.frame(crime_data, member)
head(final)
View(final)
