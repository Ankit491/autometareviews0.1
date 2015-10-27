#Question4
#Reading Data
data <- read.table("Bankrott.dat", header = TRUE)
#Changing data type of the class label column from numeric to factor
data$Aktiv <- as.factor(as.character(data$Aktiv))
#Scatter plot - Q4(a)
plot (data[,1:4],col=data[,5])
#Taking CFpVs and WspVk as active parrameters for scatter plot
plot(data$CFpVs~data$WspVk, col = data[,5], xlab = 'WspVk', ylab = 'CFpVs')
abline(1.1,-0.6)

#Nearest neighbour - Q4b
#Loading appropriate libraries
library(class)
library(mda) 
#For k=1 Nearest neighbour classification
k1nn <- knn(train=data[,-5], test=data[,-5], cl=data$Aktiv, k=1)
#Confusion Matrix
confk1 <- confusion(object = k1nn, true = data$Aktiv)
attr(confk1, "error")

#For k=3 Nearest neighbour classification
k3nn <- knn(train=data[,-5], test=data[,-5], cl=data$Aktiv, k=3)
#Confusion Matrix
confk3 <- confusion(object = k3nn, true = data$Aktiv)
attr(confk3, "error")

#For k=5 Nearest neighbour classification
k5nn <- knn(train=data[,-5], test=data[,-5], cl=data$Aktiv, k=5)
#Confusion Matrix
confk5 <- confusion(object = k5nn, true = data$Aktiv)
attr(confk5, "error")

# Classification error rate - Q4c
cerror = NULL;
for (i in seq(1,29,by=2)) 
{
  temp_e <- knn.cv(train=data[,-5], cl=data$Aktiv, k=i)
  confusion_i <- confusion(object=temp_e, true = data$Aktiv)
  cerror[i] <- attr(confusion_i, "error")
}
#Plot the classification error rates
cerror
plot(cerror, xlab = 'k' , ylab = 'error')

#Decision Tree - Q4d
#loading appropriate library
library(rpart)
#Decision Tree Gini Index
dt_gini <- rpart(Aktiv~., data=data, method="class", parms=list(split='gini'),control=rpart.control(minsplit=1, minbucket=1, cp=0))
plot(dt_gini,uniform=TRUE, compress=TRUE, margin = 0.1)
text(dt_gini, split=TRUE,use.n=TRUE, all=TRUE)
#Decision Tree Entropy
dt_ent <- rpart(Aktiv~., data=data, method="class", parms=list(split='information'),control=rpart.control(minsplit=1, minbucket=1, cp=0))
plot(dt_ent,uniform=TRUE, compress=TRUE, margin = 0.1)
text(dt_ent, split=TRUE,use.n=TRUE, all=TRUE)

#Error rate of completely expanded, un-pruned decision tree(using Gini-Index) - Q4f
#Resubstitution
resub <- predict(dt_gini, newdata=data[,-5], type="class")
resub_temp <- confusion(object=resub, true=data$Aktiv)
attr(resub_temp,"error")

#Leave-one-out
result = NULL;
for (i in 1:46)
{
  if (i == 46)    
  { train <- data[1:45,]; }
  else
    if(i < 46)
    { train <- data[c(0:(i-1),(i+1):46),]; }  
  dt_gini <- rpart(Aktiv~., data=train, method="class", parms=list(split='gini'),control=rpart.control(minsplit=1, minbucket=1, cp=0));
  result[i] <- as.integer(matrix(predict(dt_gini, newdata=data[i,-5],type = "class"))[1,1]);
}

conf_leave <- confusion(object=result, true=data$Aktiv)
attr(conf_leave, "error")
