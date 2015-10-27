#Q.4
#installing random forest package
install.packages("randomForest")
#calling libraries
library(randomForest)
library(class)
library(mda)
#Q4a
#Random Forest
ranfor <- read.table("Bankrott.dat", header = TRUE)
#converting to factor
ranfor$Aktiv <- as.factor(as.character(ranfor$Aktiv))
ban.rf <- randomForest(Aktiv~., data=ranfor, importance=TRUE)
#Importance
importance(ban.rf)
#Confusion Matrix
ban.rf$confusion
ban.rf
#Q4c
heircluster <- read.table("hierCluster.csv", sep=";", dec=",")
#Summary of dataset
summary(heircluster)
par(mfrow=c(1,1))
#Creating a box plot
boxplot(heircluster, horizontal=TRUE)
plot(heircluster)
par(mfrow=c(2,5))
#Creating Histogram
hist(heircluster$V1, xlab = 'V1')
hist(heircluster$V2, xlab = 'V2')
hist(heircluster$V3, xlab = 'V3')
hist(heircluster$V4, xlab = 'V4')
hist(heircluster$V5, xlab = 'V5')
hist(heircluster$V6, xlab = 'V6')
hist(heircluster$V7, xlab = 'V7')
hist(heircluster$V8, xlab = 'V8')
hist(heircluster$V9, xlab = 'V9')
hist(heircluster$V10, xlab = 'V10')
#Cluster Layout Plots
layout(matrix(c(1,1,2,3), 2, 2, byrow = TRUE))
plot(heircluster$V1~heircluster$V8, main="Plot Full", xlab = 'V8', ylab = 'V1')
plot(heircluster$V2~heircluster$V4, main="Plot Upper Half", xlab = 'V4', ylab = 'V2')
plot(heircluster$V5~heircluster$V8, main="Plot Lower Half", xlab = 'V8', ylab = 'V5')
#Q4d
s_link <-hclust(dist(heircluster), method = "single")
a_link <-hclust(dist(heircluster), method = "average")
c_link <-hclust(dist(heircluster), method = "complete")
par(mfrow=c(1,3))

plot(s_link, main="Single")
plot(a_link, main="Average")
plot(c_link, main="Complete")
memb <- cutree(s_link, k=4)
par(mfrow=c(1,1))
plot(heircluster, col=memb)
plot(heircluster$V2~heircluster$V8, col=memb, xlab= 'V8', ylab = 'V2')