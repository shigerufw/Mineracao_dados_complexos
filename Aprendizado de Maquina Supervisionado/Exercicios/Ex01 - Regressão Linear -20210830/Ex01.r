#############################################
#  MDC010 - Aprendizado Supervisionado I    #
#  Exercício 01 - House Pricing             #
############################################# 

set.seed(40)

dataTrain <- read.csv("housePricing_train_set.csv", header=TRUE, stringsAsFactors=TRUE)
dataVal <- read.csv("housePricing_val_set.csv", header=TRUE, stringsAsFactors=TRUE)

dim(dataTrain)
summary(dataTrain)

dim(dataVal)
summary(dataVal)

any(is.na(dataTrain))
any(is.na(dataVal))

merge(dataTrain, dataVal)

# Transforming to One-Hot-Encoding
dataTrain$less1Hocean <- as.numeric(dataTrain$ocean_proximity == "<1H OCEAN")
dataTrain$inland <- as.numeric(dataTrain$ocean_proximity == "INLAND")
dataTrain$island <- as.numeric(dataTrain$ocean_proximity == "ISLAND")
dataTrain$nearby <- as.numeric(dataTrain$ocean_proximity == "NEAR BAY")
dataTrain$nearocean <- as.numeric(dataTrain$ocean_proximity == "NEAR OCEAN")
dataTrain$ocean_proximity <- NULL

dataVal$less1Hocean <- as.numeric(dataVal$ocean_proximity == "<1H OCEAN")
dataVal$inland <- as.numeric(dataVal$ocean_proximity == "INLAND")
dataVal$island <- as.numeric(dataVal$ocean_proximity == "ISLAND")
dataVal$nearby <- as.numeric(dataVal$ocean_proximity == "NEAR BAY")
dataVal$nearocean <- as.numeric(dataVal$ocean_proximity == "NEAR OCEAN")
dataVal$ocean_proximity <- NULL

summary(dataTrain)
cor(dataTrain[,1:8])

# MinMax normalization
min_features <- apply(dataTrain[,1:8], 2, min)
min_features

max_features <- apply(dataTrain[,1:8], 2, max)
max_features

diff <- max_features - min_features
diff

dataTrain[,1:8] <- sweep(dataTrain[,1:8], 2, min_features, "-")
dataTrain[,1:8] <- sweep(dataTrain[,1:8], 2, diff, "/")
summary(dataTrain)

dataVal[,1:8] <- sweep(dataVal[,1:8], 2, min_features, "-")
dataVal[,1:8] <- sweep(dataVal[,1:8], 2, diff, "/")
summary(dataVal)

# Z-Norm normalization
#mean_features <- apply(dataTrain[,1:8], 2, mean)
#mean_features

#sd_features <- apply(dataTrain[,1:8], 2, sd)
#sd_features

#dataTrain[,1:8] <- sweep(dataTrain[,1:8], 2, mean_features, "-")
#dataTrain[,1:8] <- sweep(dataTrain[,1:8], 2, sd_features, "/")
#summary(dataTrain)

#dataVal[,1:8] <- sweep(dataVal[,1:8], 2, mean_features, "-")
#dataVal[,1:8] <- sweep(dataVal[,1:8], 2, sd_features, "/")
#summary(dataVal)

#plot(dataTrain[,"housing_median_age"], ylab = "value", ylim=c(0,1))
#points(dataTrain[,"total_rooms"], pch="*", col="blue")
#points(dataTrain[,"median_income"], pch="+", col="green")


## Baseline ##
## O comando abaixo encontrará os melhores valores de theta 
## na seguinte expressão: 
## meadian_house_value = theta0 + theta1*longitude + theta2*latitude + 
##                              theta3*house_median_age + theta4*total_rooms + 
##                                theta5*total_bedrooms + theta6*population + 
##                            theta7*households + theta8*median_income
##
## O modelo, por padrão, utiliza equações normais. Em modelos mais complexos e 
## com bases de dados muito grandes, ele automaticamente utiliza descida do gradiente.
baseline <- lm(formula=median_house_value ~ longitude + latitude + housing_median_age 
                                            + total_rooms + total_bedrooms + population 
                                            + households + median_income, data=dataTrain)

summary(baseline)

valPred <- predict(baseline, dataVal)
trainPred <- predict(baseline, dataTrain)

###################################
####   Define MAE function     ####
MAE <- function(preds, labels){
  mae_values <- sum(abs(preds-labels))/length(preds)
  return(mae_values)
}

####################################
####   Define MSE function     ####
MSE <- function(preds, labels){
  mse_values <- sum((preds-labels)**2)/length(preds)
  return(mse_values)
}

###################################
#### Define R-squared function ####
R2 <- function(pred, true){
  rss <- sum((pred - true) ^ 2)
  tss <- sum((true - mean(true)) ^ 2)
  r2 <- 1 - rss/tss
  return(r2)
}

mae_train_baseline <- MAE(trainPred, dataTrain$median_house_value)
mae_train_baseline

mae_val_baseline <- MAE(valPred, dataVal$median_house_value)
mae_val_baseline

mse_train_baseline <- MSE(trainPred, dataTrain$median_house_value)
mse_train_baseline

mse_val_baseline <- MSE(valPred, dataVal$median_house_value)
mse_val_baseline

r2_train_baseline <- R2(trainPred, dataTrain$median_house_value)
r2_train_baseline

r2_val_baseline <- R2(valPred, dataVal$median_house_value)
r2_val_baseline


## Combining features ### 
f01 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income 
               + (longitude + latitude + housing_median_age + total_rooms
              + total_bedrooms + population + households + median_income)^2)

f02 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
              + total_bedrooms + population + households + median_income 
              + (longitude + latitude + housing_median_age + total_rooms
                 + total_bedrooms + population + households + median_income)^3)

f03 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
              + total_bedrooms + population + households + median_income 
              + (longitude + latitude + housing_median_age + total_rooms
                 + total_bedrooms + population + households + median_income)^4)

modelsNoCategorical <- c(f01, f02, f03)
total_mae_train_noCat <- c(length(modelsNoCategorical))
total_mae_val_noCat <- c(length(modelsNoCategorical))

i <- 1
for(f in modelsNoCategorical){
  
  model <- lm(formula=f, data=dataTrain)
  
  valPred <- predict(model, dataVal)
  trainPred <- predict(model, dataTrain)
  
  mae_train <- MAE(trainPred, dataTrain$median_house_value)
  total_mae_train_noCat[i] <- mae_train
  
  mae_val <- MAE(valPred, dataVal$median_house_value)
  total_mae_val_noCat[i] <- mae_val
  i <- i + 1
 
}
summary(model)
plot(total_mae_val_noCat, xlab="Complexity", ylab="Error", 
     ylim=c(43000, 52000), pch="+", col="blue",  xaxt="n")
axis(1, at=1:length(modelsNoCategorical), labels=seq(from = 1, to = 3, by = 1), las=1)
points(total_mae_train_noCat, pch="*", col="red")
points(rep(mae_val_baseline, length(total_mae_val_noCat)), pch="o", col="green")

lines(total_mae_train_noCat, col="red", lty=2)
lines(total_mae_val_noCat, col="blue", lty=2)
lines(rep(mae_val_baseline, length(total_mae_val_noCat)), col="green", lty=2)
legend(1, 45000, legend=c("Train", "Validation", "Baseline"), 
       col=c("red","blue", "green"), lty=2, cex=0.8)

#### Polynomials  ####
f01 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income, data=dataTrain)

f02 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
          + total_bedrooms + population + households + median_income + I(longitude^2) 
          + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
          + I(total_bedrooms^2) + I(population^2) + I(households^2) 
          + I(median_income^2), data=dataTrain)

f03 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + I(longitude^2) 
               + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
               + I(total_bedrooms^2) + I(population^2) + I(households^2) 
               + I(median_income^2) + I(longitude^3) 
               + I(latitude^3) + I(housing_median_age^3) + I(total_rooms^3)
               + I(total_bedrooms^3) + I(population^3) + I(households^3) 
               + I(median_income^3), data=dataTrain)

f04 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + I(longitude^2) 
               + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
               + I(total_bedrooms^2) + I(population^2) + I(households^2) 
               + I(median_income^2) + I(longitude^3) 
               + I(latitude^3) + I(housing_median_age^3) + I(total_rooms^3)
               + I(total_bedrooms^3) + I(population^3) + I(households^3) 
               + I(median_income^3) + I(longitude^4) 
               + I(latitude^4) + I(housing_median_age^4) + I(total_rooms^4)
               + I(total_bedrooms^4) + I(population^4) + I(households^4) 
               + I(median_income^4), data=dataTrain)

f05 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + I(longitude^2) 
               + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
               + I(total_bedrooms^2) + I(population^2) + I(households^2) 
               + I(median_income^2) + I(longitude^3) 
               + I(latitude^3) + I(housing_median_age^3) + I(total_rooms^3)
               + I(total_bedrooms^3) + I(population^3) + I(households^3) 
               + I(median_income^3) + I(longitude^4) 
               + I(latitude^4) + I(housing_median_age^4) + I(total_rooms^4)
               + I(total_bedrooms^4) + I(population^4) + I(households^4) 
               + I(median_income^4) + I(longitude^5) 
               + I(latitude^5) + I(housing_median_age^5) + I(total_rooms^5)
               + I(total_bedrooms^5) + I(population^5) + I(households^5) 
               + I(median_income^5), data=dataTrain)

f06 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + I(longitude^2) 
               + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
               + I(total_bedrooms^2) + I(population^2) + I(households^2) 
               + I(median_income^2) + I(longitude^3) 
               + I(latitude^3) + I(housing_median_age^3) + I(total_rooms^3)
               + I(total_bedrooms^3) + I(population^3) + I(households^3) 
               + I(median_income^3) + I(longitude^4) 
               + I(latitude^4) + I(housing_median_age^4) + I(total_rooms^4)
               + I(total_bedrooms^4) + I(population^4) + I(households^4) 
               + I(median_income^4) + I(longitude^5) 
               + I(latitude^5) + I(housing_median_age^5) + I(total_rooms^5)
               + I(total_bedrooms^5) + I(population^5) + I(households^5) 
               + I(median_income^5) + I(longitude^6) 
               + I(latitude^6) + I(housing_median_age^6) + I(total_rooms^6)
               + I(total_bedrooms^6) + I(population^6) + I(households^6) 
               + I(median_income^6), data=dataTrain)

f07 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + I(longitude^2) 
               + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
               + I(total_bedrooms^2) + I(population^2) + I(households^2) 
               + I(median_income^2) + I(longitude^3) 
               + I(latitude^3) + I(housing_median_age^3) + I(total_rooms^3)
               + I(total_bedrooms^3) + I(population^3) + I(households^3) 
               + I(median_income^3) + I(longitude^4) 
               + I(latitude^4) + I(housing_median_age^4) + I(total_rooms^4)
               + I(total_bedrooms^4) + I(population^4) + I(households^4) 
               + I(median_income^4) + I(longitude^5) 
               + I(latitude^5) + I(housing_median_age^5) + I(total_rooms^5)
               + I(total_bedrooms^5) + I(population^5) + I(households^5) 
               + I(median_income^5) + I(longitude^6) 
               + I(latitude^6) + I(housing_median_age^6) + I(total_rooms^6)
               + I(total_bedrooms^6) + I(population^6) + I(households^6) 
               + I(median_income^6) + I(longitude^7) 
               + I(latitude^7) + I(housing_median_age^7) + I(total_rooms^7)
               + I(total_bedrooms^7) + I(population^7) + I(households^7) 
               + I(median_income^7), data=dataTrain)

f08 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + I(longitude^2) 
               + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
               + I(total_bedrooms^2) + I(population^2) + I(households^2) 
               + I(median_income^2) + I(longitude^3) 
               + I(latitude^3) + I(housing_median_age^3) + I(total_rooms^3)
               + I(total_bedrooms^3) + I(population^3) + I(households^3) 
               + I(median_income^3) + I(longitude^4) 
               + I(latitude^4) + I(housing_median_age^4) + I(total_rooms^4)
               + I(total_bedrooms^4) + I(population^4) + I(households^4) 
               + I(median_income^4) + I(longitude^5) 
               + I(latitude^5) + I(housing_median_age^5) + I(total_rooms^5)
               + I(total_bedrooms^5) + I(population^5) + I(households^5) 
               + I(median_income^5) + I(longitude^6) 
               + I(latitude^6) + I(housing_median_age^6) + I(total_rooms^6)
               + I(total_bedrooms^6) + I(population^6) + I(households^6) 
               + I(median_income^6) + I(longitude^7) 
               + I(latitude^7) + I(housing_median_age^7) + I(total_rooms^7)
               + I(total_bedrooms^7) + I(population^7) + I(households^7) 
               + I(median_income^7) + I(longitude^8) 
               + I(latitude^8) + I(housing_median_age^8) + I(total_rooms^8)
               + I(total_bedrooms^8) + I(population^8) + I(households^8) 
               + I(median_income^8), data=dataTrain)

f09 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + I(longitude^2) 
               + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
               + I(total_bedrooms^2) + I(population^2) + I(households^2) 
               + I(median_income^2) + I(longitude^3) 
               + I(latitude^3) + I(housing_median_age^3) + I(total_rooms^3)
               + I(total_bedrooms^3) + I(population^3) + I(households^3) 
               + I(median_income^3) + I(longitude^4) 
               + I(latitude^4) + I(housing_median_age^4) + I(total_rooms^4)
               + I(total_bedrooms^4) + I(population^4) + I(households^4) 
               + I(median_income^4) + I(longitude^5) 
               + I(latitude^5) + I(housing_median_age^5) + I(total_rooms^5)
               + I(total_bedrooms^5) + I(population^5) + I(households^5) 
               + I(median_income^5) + I(longitude^6) 
               + I(latitude^6) + I(housing_median_age^6) + I(total_rooms^6)
               + I(total_bedrooms^6) + I(population^6) + I(households^6) 
               + I(median_income^6) + I(longitude^7) 
               + I(latitude^7) + I(housing_median_age^7) + I(total_rooms^7)
               + I(total_bedrooms^7) + I(population^7) + I(households^7) 
               + I(median_income^7) + I(longitude^8) 
               + I(latitude^8) + I(housing_median_age^8) + I(total_rooms^8)
               + I(total_bedrooms^8) + I(population^8) + I(households^8) 
               + I(median_income^8) + I(longitude^9) 
               + I(latitude^9) + I(housing_median_age^9) + I(total_rooms^9)
               + I(total_bedrooms^9) + I(population^9) + I(households^9) 
               + I(median_income^9), data=dataTrain)

f10 <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + I(longitude^2) 
               + I(latitude^2) + I(housing_median_age^2) + I(total_rooms^2)
               + I(total_bedrooms^2) + I(population^2) + I(households^2) 
               + I(median_income^2) + I(longitude^3) 
               + I(latitude^3) + I(housing_median_age^3) + I(total_rooms^3)
               + I(total_bedrooms^3) + I(population^3) + I(households^3) 
               + I(median_income^3) + I(longitude^4) 
               + I(latitude^4) + I(housing_median_age^4) + I(total_rooms^4)
               + I(total_bedrooms^4) + I(population^4) + I(households^4) 
               + I(median_income^4) + I(longitude^5) 
               + I(latitude^5) + I(housing_median_age^5) + I(total_rooms^5)
               + I(total_bedrooms^5) + I(population^5) + I(households^5) 
               + I(median_income^5) + I(longitude^6) 
               + I(latitude^6) + I(housing_median_age^6) + I(total_rooms^6)
               + I(total_bedrooms^6) + I(population^6) + I(households^6) 
               + I(median_income^6) + I(longitude^7) 
               + I(latitude^7) + I(housing_median_age^7) + I(total_rooms^7)
               + I(total_bedrooms^7) + I(population^7) + I(households^7) 
               + I(median_income^7) + I(longitude^8) 
               + I(latitude^8) + I(housing_median_age^8) + I(total_rooms^8)
               + I(total_bedrooms^8) + I(population^8) + I(households^8) 
               + I(median_income^8) + I(longitude^9) 
               + I(latitude^9) + I(housing_median_age^9) + I(total_rooms^9)
               + I(total_bedrooms^9) + I(population^9) + I(households^9) 
               + I(median_income^9) + I(longitude^10) 
               + I(latitude^10) + I(housing_median_age^10) + I(total_rooms^10)
               + I(total_bedrooms^10) + I(population^10) + I(households^10) 
               + I(median_income^10), data=dataTrain)





formulas <- list(f01, f02, f03, f04, f05, f06, f07, f08, f09, f10)
total_mae_train_poly <- c(length(formulas))
total_mae_val_poly <- c(length(formulas))
i <- 1
for(i in 1:10){
  model <- lm(formula=formulas[[i]], data=dataTrain)
  
  valPred <- predict(model, dataVal)
  trainPred <- predict(model, dataTrain)
  
  mae_train <- MAE(trainPred, dataTrain$median_house_value)
  total_mae_train_poly[i] <- mae_train
  
  mae_val <- MAE(valPred, dataVal$median_house_value)
  total_mae_val_poly[i] <- mae_val
  i <- i + 1
  
}

summary(model)
plot(total_mae_val_poly, xlab="Complexity", ylab="Error", 
     ylim=c(44000, max(total_mae_val_poly)+5000), pch="+", col="blue")

points(total_mae_train_poly, pch="*", col="red")
points(rep(mae_val_baseline, length(total_mae_val_poly)), pch="o", col="green")

lines(total_mae_train_poly, col="red", lty=2)
lines(total_mae_val_poly, col="blue", lty=2)
lines(rep(mae_val_baseline, length(total_mae_val_poly)), col="green", lty=2)
legend(1, 46000, legend=c("Train", "Validation", "Baseline"), 
       col=c("red","blue", "green"), lty=2, cex=0.7)


#### Considering categorical ####
baseline <- lm(formula=median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + population + households + median_income + less1Hocean
               + inland + island + nearby + nearocean, data=dataTrain)

summary(baseline)

valPred <- predict(baseline, dataVal)
trainPred <- predict(baseline, dataTrain)

mae_train_baseline_cat <- MAE(trainPred, dataTrain$median_house_value)
mae_val_baseline_cat <- MAE(valPred, dataVal$median_house_value)


f01_cat <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + households + median_income + population + 
               + (longitude + latitude + housing_median_age + total_rooms
                  + total_bedrooms + households + median_income + population)^2 + less1Hocean + inland
                + island + nearby + nearocean)

f02_cat <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + households + median_income + population + 
               + (longitude + latitude + housing_median_age + total_rooms
                  + total_bedrooms + households + median_income + population)^3 +less1Hocean + inland
               + island + nearby + nearocean)

f03_cat <- formula(median_house_value ~ longitude + latitude + housing_median_age + total_rooms
               + total_bedrooms + households + median_income 
               + (longitude + latitude + housing_median_age + total_rooms
                  + total_bedrooms + households + median_income + population)^4 + less1Hocean + inland
               + island + nearby + nearocean)
#summary(model01)

modelsCategorical <- c(f01_cat, f02_cat, f03_cat)
total_mae_train_cat <- c(length(modelsCategorical))
total_mae_val_cat <- c(length(modelsCategorical))

i <- 1
for(f in modelsCategorical){
  
  model <- lm(formula=f, data=dataTrain)
  
  valPred <- predict(model, dataVal)
  trainPred <- predict(model, dataTrain)
  
  mae_train <- MAE(trainPred, dataTrain$median_house_value)
  total_mae_train_cat[i] <- mae_train
  
  mae_val <- MAE(valPred, dataVal$median_house_value)
  total_mae_val_cat[i] <- mae_val
  i <- i + 1
  
}

# Plotting non-categorical features results
plot(total_mae_val_noCat, xlab="Complexity", ylab="Error", 
     ylim=c(40000, 55000), pch="+", col="blue")

points(total_mae_train_noCat, pch="+", col="red")
points(rep(mae_val_baseline, length(total_mae_val_noCat)), pch="+", col="green")

lines(total_mae_train_noCat, col="red", lty=1)
lines(total_mae_val_noCat, col="blue", lty=1)
lines(rep(mae_val_baseline, length(total_mae_val_noCat)), col="green", lty=1)


# Plotting categorical features results
points(total_mae_val_cat, pch="o", col="blue")
points(total_mae_train_cat, pch="o", col="red")
points(rep(mae_val_baseline_cat, length(total_mae_val_cat)), pch="o", col="green")

lines(total_mae_train_cat, col="red", lty=2)
lines(total_mae_val_cat, col="blue", lty=2)
lines(rep(mae_val_baseline_cat, length(total_mae_val_cat)), col="green", lty=2)
legend(1, 43000, legend=c("Train no Categorical", 
                          "Validation no Categorical",
                          "Train with Categorical", 
                          "Validation with categorical"), 
              col=c("red","blue","red","blue"), lty=c(1,1,2,2), cex=0.7)


#### Performance on TEST SET ####
#### Best Model - combination 4 by 4 with categorical ####

## Getting min value on valiation set
min(total_mae_val_noCat)
min(total_mae_val_poly)
min(total_mae_val_cat)


total_mae_val_cat # third model of the categorical models

dataTest <- read.csv("housePricing_test_set.csv", header=TRUE, stringsAsFactors=TRUE)
any(is.na(dataTest))

dataTest$less1Hocean <- as.numeric(dataTest$ocean_proximity == "<1H OCEAN")
dataTest$inland <- as.numeric(dataTest$ocean_proximity == "INLAND")
dataTest$island <- as.numeric(dataTest$ocean_proximity == "ISLAND")
dataTest$nearby <- as.numeric(dataTest$ocean_proximity == "NEAR BAY")
dataTest$nearocean <- as.numeric(dataTest$ocean_proximity == "NEAR OCEAN")
dataTest$ocean_proximity <- NULL

dataTest[,1:8] <- sweep(dataTest[,1:8], 2, min_features, "-")
dataTest[,1:8] <- sweep(dataTest[,1:8], 2, diff, "/")

## Retrain the best model ##
best_model <- lm(formula=median_house_value ~ longitude + latitude + housing_median_age + total_rooms
                   + total_bedrooms + households + median_income 
                   + (longitude + latitude + housing_median_age + total_rooms
                      + total_bedrooms + households + median_income + population)^4 + less1Hocean + inland
                   + island + nearby + nearocean, data=dataTrain)



############################
testPred <- predict(best_model, dataTest)
mae_test <- MAE(testPred, dataTest$median_house_value)
mae_test


