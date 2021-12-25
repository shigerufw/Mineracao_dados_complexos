#########################################################
#  MDC010 - Aprendizado Supervisionado I                #
#  Exercício 01 - House Pricing - Descida do Gradiente  #
######################################################### 

#install.packages("gradDescent")
library("gradDescent")

set.seed(40)

dataTrain <- read.csv("housePricing_train_set.csv", header=TRUE, stringsAsFactors=TRUE)
dataVal <- read.csv("housePricing_val_set.csv", header=TRUE, stringsAsFactors=TRUE)

dim(dataTrain)
summary(dataTrain)

dim(dataVal)
summary(dataVal)

any(is.na(dataTrain))
any(is.na(dataVal))

## Removing Categorical
dataTrain[,10] <- NULL
dataVal[,10] <- NULL

summary(dataTrain)
summary(dataVal)

# MinMax normalization
min_features <- apply(dataTrain[,1:8], 2, min)
max_features <- apply(dataTrain[,1:8], 2, max)
diff <- max_features - min_features

dataTrain[,1:8] <- sweep(dataTrain[,1:8], 2, min_features, "-")
dataTrain[,1:8] <- sweep(dataTrain[,1:8], 2, diff, "/")
summary(dataTrain)

dataVal[,1:8] <- sweep(dataVal[,1:8], 2, min_features, "-")
dataVal[,1:8] <- sweep(dataVal[,1:8], 2, diff, "/")
summary(dataVal)

## Training with GD ##
## It's expected that the last column of the training data is the target 
gd <- gradDescentR.learn(dataTrain, learningMethod = "GD",
                         featureScaling=FALSE, seed=42, 
                         control=list(alpha=0.01, maxIter=1000))
gd$model

#gd <- GD(dataTrain, alpha = 0.01, maxIter = 1000, seed = 42)

## In prediction, the number of columns must be the same as in the training. Since
## the last column on training is the target and we do not assume labels on validtaion/test
## we are going to add a columns of 0s

valPred <- predict(gd, cbind(dataVal[,1:8], 0))
valPred <- valPred[,ncol(valPred)]

trainPred <- predict(gd, cbind(dataTrain[,1:8], 0))
trainPred <- trainPred[,ncol(trainPred)]

MAE <- function(preds, labels){
  mae_values <- sum(abs(preds-labels))/length(preds)
  return(mae_values)
}

MSE <- function(preds, labels){
  mse_values <- sum((preds-labels)**2)/length(preds)
  return(mse_values)
}

mae_train_baseline <- MAE(trainPred, dataTrain$median_house_value)
mae_train_baseline

mae_val_baseline <- MAE(valPred, dataVal$median_house_value)
mae_val_baseline

## Changing number of iterations ##
iterations <- c(100, 200, 500, 1000, 5000)
total_mae_train <- c(length(iterations))
total_mae_val <- c(length(iterations))
i <- 1
for(iter in iterations){
  gd <- gradDescentR.learn(dataTrain, learningMethod = "GD",
                           featureScaling=FALSE, seed=42, 
                           control=list(alpha=0.01, maxIter=iter))
  
  valPred <- predict(gd, cbind(dataVal[,1:8], 0))
  valPred <- valPred[,ncol(valPred)]
  
  trainPred <- predict(gd, cbind(dataTrain[,1:8], 0))
  trainPred <- trainPred[,ncol(trainPred)]
  
  mae_train <- MAE(trainPred, dataTrain$median_house_value)
  total_mae_train[i] <- mae_train
  
  mae_val <- MAE(valPred, dataVal$median_house_value)
  total_mae_val[i] <- mae_val
  i <- i + 1

}

plot(total_mae_val, xaxt="n", xlab="Number of iterations", ylab="Error", 
     ylim=c(min(total_mae_val)-5000, max(total_mae_val)+5000), pch="+", col="blue")

axis(1, 1:5,iterations)

points(total_mae_train, pch="*", col="red")
points(rep(mae_val_baseline, length(total_mae_val)), pch="o", col="green")

lines(total_mae_train, col="red", lty=2)
lines(total_mae_val, col="blue", lty=2)
lines(rep(mae_val_baseline, length(total_mae_val)), col="green", lty=2)
legend(1, 70000, legend=c("Train", "Val", "Baseline"), 
                  col=c("red", "blue", "green"), lty=2, cex=0.9)

## Changing learning rate ##
Learning_rates <- c(1e-4, 1e-3, 1e-2, 0.1, 1.0)
total_mae_train_lr <- c(length(Learning_rates))
total_mae_val_lr <- c(length(Learning_rates))
i <- 1
for(lr in Learning_rates){
  gd <- gradDescentR.learn(dataTrain, learningMethod = "GD",
                           featureScaling=FALSE, seed=42, 
                           control=list(alpha=lr, maxIter=1000))
  
  valPred <- predict(gd, cbind(dataVal[,1:8], 0))
  valPred <- valPred[,ncol(valPred)]
  
  trainPred <- predict(gd, cbind(dataTrain[,1:8], 0))
  trainPred <- trainPred[,ncol(trainPred)]
  
  mae_train <- MAE(trainPred, dataTrain$median_house_value)
  total_mae_train_lr[i] <- mae_train
  
  mae_val <- MAE(valPred, dataVal$median_house_value)
  total_mae_val_lr[i] <- mae_val
  i <- i + 1
  
}

plot(total_mae_val_lr, xaxt="n", xlab="Learning Rate values",
     ylab="Error", ylim=c(min(total_mae_val_lr)-5000, max(total_mae_val_lr)+5000), 
     pch="+", col="blue")

axis(1, 1:5, Learning_rates)

points(total_mae_train_lr, pch="*", col="red")
points(rep(mae_val_baseline, length(total_mae_val)), pch="o", col="green")

lines(total_mae_train_lr, col="red", lty=2)
lines(total_mae_val_lr, col="blue", lty=2)
lines(rep(mae_val_baseline, length(total_mae_val)), col="green", lty=2)
legend(1, 70000, legend=c("Train", "Val", "Baseline"), 
       col=c("red", "blue", "green"), lty=2, cex=0.8)

