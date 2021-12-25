##########################################
# MDC010 - Aprendizado Supervisionado 01 #
# Exercício 02 - Regressão Linear        #
##########################################

source("cleaning.R")
set.seed(17)

data <- read.csv("Financial Distress.csv")
summary(data)

data$x80 <- NULL
data$Company <- NULL
data$Time <- NULL

summary(data)

####### Pre-processing on the database #######

# Selecting features that do have outliers to be removed
selectedFeatures <- c("x1", "x7", "x8", "x12", "x15", "x16", "x17", "x19",
                      "x22", "x25", "x27", "x31", "x32", "x34", "x35",
                      "x38", "x39", "x42", "x43", "x44", "x47", "x48",
                      "x52", "x54", "x57", "x59", "x81")

# Get the new data without outlier features
data <- data[, -match(selectedFeatures, colnames(data))]

# Remove the first and the last 12 values from extreme values 
# for each feature
data <- removeOutliers(data, 12)

dim(data)
data <- unique(data)
dim(data)

any(is.na(data))
summary(data)


# Training/Validation/Test division
randomTrainValIndexes <- sample(1:nrow(data), size=0.8*nrow(data))
trainValSet <- data[randomTrainValIndexes,]
testSet <- data[-randomTrainValIndexes,]

randomTrainIndexes <- sample(1:nrow(trainValSet), size=0.8*nrow(trainValSet))
trainSet <- trainValSet[randomTrainIndexes,]
valSet <- trainValSet[-randomTrainIndexes,]

merge(trainSet, valSet)
merge(trainSet, testSet)
merge(valSet, testSet)

dim(trainSet)
dim(valSet)
dim(testSet)

summary(trainSet)
summary(valSet)
summary(testSet)

## Normalizing
# MinMax normalization
min_features <- apply(trainSet[,2:ncol(trainSet)], 2, min)
min_features

max_features <- apply(trainSet[,2:ncol(trainSet)], 2, max)
max_features

diff <- max_features - min_features
diff

trainSet[,2:ncol(trainSet)] <- sweep(trainSet[,2:ncol(trainSet)], 2, min_features, "-")
trainSet[,2:ncol(trainSet)] <- sweep(trainSet[,2:ncol(trainSet)], 2, diff, "/")
summary(trainSet)

valSet[,2:ncol(valSet)] <- sweep(valSet[,2:ncol(valSet)], 2, min_features, "-")
valSet[,2:ncol(valSet)] <- sweep(valSet[,2:ncol(valSet)], 2, diff, "/")
summary(valSet)

testSet[,2:ncol(testSet)] <- sweep(testSet[,2:ncol(testSet)], 2, min_features, "-")
testSet[,2:ncol(testSet)] <- sweep(testSet[,2:ncol(testSet)], 2, diff, "/")
summary(testSet)

###
#mean_features <- apply(trainSet[,2:ncol(trainSet)], 2, mean)
#mean_features

#sd_features <- apply(trainSet[,2:ncol(trainSet)], 2, sd)
#sd_features

#trainSet[,2:ncol(trainSet)] <- sweep(trainSet[,2:ncol(trainSet)], 2, mean_features, "-")
#trainSet[,2:ncol(trainSet)] <- sweep(trainSet[,2:ncol(trainSet)], 2, sd_features, "/")
#summary(trainSet)

#valSet[,2:ncol(valSet)] <- sweep(valSet[,2:ncol(valSet)], 2, mean_features, "-")
#valSet[,2:ncol(valSet)] <- sweep(valSet[,2:ncol(valSet)], 2, sd_features, "/")
#summary(valSet)

#testSet[,2:ncol(testSet)] <- sweep(testSet[,2:ncol(testSet)], 2, mean_features, "-")
#testSet[,2:ncol(testSet)] <- sweep(testSet[,2:ncol(testSet)], 2, sd_features, "/")
#summary(testSet)

#### Defining helper formula
getHypothesis <- function(feature_names, degree){
    
    # Switch here to name of the target variable 
    hypothesis_string <- "hypothesis <- formula(Financial.Distress ~ "
    for(d in 1:degree){
        for(i in 1:length(feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       "I(", feature_names[i], "^", d, ") + ",
                                       sep = "")
        }
    }
    hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
    hypothesis_string <- paste(hypothesis_string, ")")
    hypothesis <- eval(parse(text=hypothesis_string))
    return(hypothesis)
}

feature_names <- colnames(trainSet)[2:ncol(trainSet)]
feature_names

hypothesis <- getHypothesis(feature_names, 1)
hypothesis

## Baseline ##
baseline <- lm(formula=hypothesis, data=trainSet)

valPred <- predict(baseline, valSet)
trainPred <- predict(baseline, trainSet)
testPred <- predict(baseline, testSet)


MAE <- function(preds, labels){
    mae_values <- sum(abs(preds-labels))/length(preds)
    return(mae_values)
}

MSE <- function(preds, labels){
    mse_values <- sum((preds-labels)**2)/length(preds)
    return(mse_values)
}

mae_train_baseline <- MAE(trainPred, trainSet$Financial.Distress)
mae_train_baseline

mae_val_baseline <- MAE(valPred, valSet$Financial.Distress)
mae_val_baseline

mae_test_baseline <- MAE(testPred, testSet$Financial.Distress)
mae_test_baseline

##### Combining features #####
cor(trainSet)
f01 <- formula(Financial.Distress ~ .)

f02 <- formula(Financial.Distress ~ . + (x2+x4)^2 + (x13+x21+x26)^2 
                                      + (x20+x29+x30)^2)

f03 <- formula(Financial.Distress ~ . + (x2+x4)^3 + (x13+x21+x26)^3 
                                      + (x20+x29+x30)^3)

f04 <- formula(Financial.Distress ~ . + (x2+x4)^4 + (x13+x21+x26)^4 
                                      + (x20+x29+x30)^4)

modelsNoCategorical <- c(f01, f02, f03, f04)
total_mae_train_noCat <- c(length(modelsNoCategorical))
total_mae_val_noCat <- c(length(modelsNoCategorical))

i <- 1
for(f in modelsNoCategorical){
    
    model <- lm(formula=f, data=trainSet)
    
    valPred <- predict(model, valSet)
    trainPred <- predict(model, trainSet)
    
    mae_train <- MAE(trainPred, trainSet$Financial.Distress)
    total_mae_train_noCat[i] <- mae_train
    
    mae_val <- MAE(valPred, valSet$Financial.Distress)
    total_mae_val_noCat[i] <- mae_val
    i <- i + 1
    
}

total_mae_train_noCat
plot(total_mae_train_noCat, xlab="Complexity", ylab="Error", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(total_mae_train_noCat, total_mae_val_noCat, mae_val_baseline)),
            max(c(total_mae_train_noCat, total_mae_val_noCat, mae_val_baseline))))


axis(1, at=1:length(modelsNoCategorical), 
     labels=seq(from = 1, to = length(modelsNoCategorical), by = 1), las=1)
points(total_mae_val_noCat, pch="*", col="blue")
points(rep(mae_val_baseline, length(total_mae_val_noCat)), pch="o", col="green")

lines(total_mae_train_noCat, col="red", lty=2)
lines(total_mae_val_noCat, col="blue", lty=2)
lines(rep(mae_val_baseline, length(total_mae_val_noCat)), col="green", lty=2)
total_mae_val_noCat
legend(1, 0.6234, legend=c("Train", "Validation", "Baseline"), 
       col=c("red","blue", "green"), lty=2, cex=0.7)

## Training again and testing
best_formula <- modelsNoCategorical[[which.min(total_mae_val_noCat)]]
best_formula
model <- lm(formula=best_formula, data=trainSet)

testPred <- predict(model, testSet)
mae_test <- MAE(testPred, testSet$Financial.Distress)
mae_test

## Polynomial Analysis
total_mae_train_noCat <- c()
total_mae_val_noCat <- c()


for(i in 1:5){
   
    hypothesis <- getHypothesis(feature_names, i)
    
    ## Baseline ##
    model_poly <- lm(formula=hypothesis, data=trainSet)
    
    valPred <- predict(model_poly, valSet)
    trainPred <- predict(model_poly, trainSet)
    
    mae_train <- MAE(trainPred, trainSet$Financial.Distress)
    total_mae_train_noCat[i] <- mae_train
    
    mae_val <- MAE(valPred, valSet$Financial.Distress)
    total_mae_val_noCat[i] <- mae_val
}

total_mae_train_noCat
plot(total_mae_train_noCat, xlab="Complexity", ylab="Error", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(total_mae_train_noCat, total_mae_val_noCat, mae_val_baseline)),
            max(c(total_mae_train_noCat, total_mae_val_noCat, mae_val_baseline))))


axis(1, at=1:5, labels=seq(from = 1, to = 5, by = 1), las=1)
points(total_mae_val_noCat, pch="*", col="blue")
points(rep(mae_val_baseline, length(total_mae_val_noCat)), pch="o", col="green")

lines(total_mae_train_noCat, col="red", lty=2)
lines(total_mae_val_noCat, col="blue", lty=2)
lines(rep(mae_val_baseline, length(total_mae_val_noCat)), col="green", lty=2)
total_mae_val_noCat

legend(1, 0.595, legend=c("Train", "Validation", "Baseline"), 
       col=c("red","blue", "green"), lty=2, cex=0.7)

#### Testing #### 
i<- which.min(total_mae_val_noCat)
i

hypothesis <- getHypothesis(feature_names, i)
best_model <- lm(formula=hypothesis, data=trainSet)

testPred <- predict(best_model, testSet)
mae_test <- MAE(testPred, testSet$Financial.Distress)
mae_test



