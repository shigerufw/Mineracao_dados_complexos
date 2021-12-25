##########################################
# MDC010 - Aprendizado Supervisionado 01 #
# Exerc�cio 03 - Regress�o Log�stica     #
##########################################

#install.packages("glmnet")
#install.packages("caret")
#install.packages("pROC")
source("support_functions.R")

set.seed(13)


library(glmnet)
library(caret)
library(pROC)

### Carregando as bases de dados ### 
trainSet <- read.csv("cholesterol_training_set.csv")
valSet <- read.csv("cholesterol_validation_set.csv")
testSet <- read.csv("cholesterol_test_set.csv")

summary(trainSet)
summary(valSet)
summary(testSet)

dim(trainSet)
dim(valSet)
dim(testSet)

merge(trainSet, valSet)
merge(trainSet, testSet)
merge(valSet, testSet)


trainSet$class <- as.factor(trainSet$class)
valSet$class <- as.factor(valSet$class)
testSet$class <- as.factor(testSet$class)

### Verifica Frequencia de cada uma das classes ###
table(trainSet$class)
table(valSet$class)
table(testSet$class)

## Normalizacao Z-norma 
mean_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, mean)
mean_features

sd_features <- apply(trainSet[,1:(ncol(trainSet)-1)], 2, sd)
sd_features

trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, mean_features, "-")
trainSet[,1:(ncol(trainSet)-1)] <- sweep(trainSet[,1:(ncol(trainSet)-1)], 2, sd_features, "/")
summary(trainSet)

valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, mean_features, "-")
valSet[,1:(ncol(valSet)-1)] <- sweep(valSet[,1:(ncol(valSet)-1)], 2, sd_features, "/")
summary(valSet)

testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, mean_features, "-")
testSet[,1:(ncol(testSet)-1)] <- sweep(testSet[,1:(ncol(testSet)-1)], 2, sd_features, "/")
summary(testSet)

############ Training Models ############
feature_names <- colnames(trainSet)[1:(ncol(trainSet)-1)]
feature_names

hypothesis <- getHypothesis(feature_names, 1)
hypothesis


help(glmnet)
x_train <- model.matrix(hypothesis, trainSet)
x_train
y_train <- trainSet$class
y_train


model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE,
                maxit = 1e+05, alpha=0, lambda = 1e-2)

### Verificando os thetas aprendidos ###
model$beta
model$a0 # valor do theta0 (intercept)

trainPred <- predict(model, newx = x_train, type="response")
trainPred

#converting to class
trainClassPred <- trainPred

#### THRESHOLD ####
# Threshold = 0.5 
trainClassPred[trainPred >= 0.5] <- 1
trainClassPred[trainPred < 0.5] <- 0
trainClassPred

#### Balanced Loss
lossN = getLoss(trainSet$class[trainSet$class == 0], trainPred[trainSet$class == 0])
lossP = getLoss(trainSet$class[trainSet$class == 1], trainPred[trainSet$class == 1])
lossN
lossP
(lossN+lossP)/2


cm <- confusionMatrix(data = as.factor(trainClassPred), 
                      reference = as.factor(trainSet$class), 
                      positive='1')


cm$table # Devemos transpo-la para deixar os labels nas linhas 
         # e as predicoes nas colunas. Bem como devemos deixa-la
         # em valores relativos de forma que as linhas somem 100%
         # A funcao "calculaMatrizConfusaoRelativa" ja realiza
         # ambos os procedimentos. 

# SEMPRE construam e reportem a matriz de confusao relativa!
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

### Predicao no conjunto de validacao ###
x_val <- model.matrix(hypothesis, valSet)
y_val <- valSet$class
valPred <- predict(model, newx = x_val, type="response")

#valPred

#converting to class
valClassPred <- valPred


#### THRESHOLD ####
# Threshold = 0.5 
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0
#valClassPred

# threshold = 0.9
#valClassPred[valPred >= 0.9] <- 1
#valClassPred[valPred < 0.9] <- 0


##### Let's see how well we did
#Loss 
lossN = getLoss(valSet$class[valSet$class == 0], valPred[valSet$class == 0])
lossP = getLoss(valSet$class[valSet$class == 1], valPred[valSet$class == 1])
lossN
lossP
loss_baseline <- (lossN+lossP)/2
loss_baseline

cm <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valSet$class), 
                      positive='1')


cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal_baseline <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_baseline

# ROC Curve for baseline
ROC <- roc(valSet$class, valPred[,1], direction="<")
ROC

plot(ROC, col="blue", lwd=2, main="ROC")

############# Polynomial analysis ###########
loss_train <- c()
loss_val <- c()

acc_train <- c()
acc_val <- c()

feature_names <- colnames(trainSet)[1:(ncol(trainSet)-1)]

## Polynomial Analysis
### Be careful! Higher polynomial degrees might not converge!
for(i in 1:3){  
    
    print(i)
    hypothesis <- getHypothesis(feature_names, i)
    
    # Applying hypothesis and training the model
    x_train <- model.matrix(hypothesis, trainSet)
    y_train <- trainSet$class
    model <- glmnet(x_train, y_train,  family="binomial", 
                    standardize = FALSE, maxit = 1e+05, 
                    alpha=0, lambda = 1e-2)
    
    trainPred <- predict(model, newx = x_train, type="response")
    
    #converting to class
    trainClassPred <- trainPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    trainClassPred[trainPred >= 0.5] <- 1
    trainClassPred[trainPred < 0.5] <- 0
    #trainClassPred
    
    lossN = getLoss(trainSet$class[trainSet$class == 0], trainPred[trainSet$class == 0])
    lossP = getLoss(trainSet$class[trainSet$class == 1], trainPred[trainSet$class == 1])
    mean_loss_train <- (lossN+lossP)/2
    
    cm <- confusionMatrix(data = as.factor(trainClassPred), 
                          reference = as.factor(trainSet$class), 
                          positive='1')
    
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
 
    # Validation
    x_val <- model.matrix(hypothesis, valSet)
    y_val <- valSet$class
    valPred <- predict(model, newx = x_val, type="response")
    
    #converting to class
    valClassPred <- valPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    valClassPred[valPred >= 0.5] <- 1
    valClassPred[valPred < 0.5] <- 0
    
    ##### Let's see how well we did
    #Loss 
    lossN = getLoss(valSet$class[valSet$class == 0], valPred[valSet$class == 0])
    lossP = getLoss(valSet$class[valSet$class == 1], valPred[valSet$class == 1])
    mean_loss_val <- (lossN+lossP)/2
    mean_loss_val
    
    cm <- confusionMatrix(data = as.factor(valClassPred), 
                          reference = as.factor(valSet$class), 
                          positive='1')
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
    
    loss_train[i] <- mean_loss_train
    loss_val[i] <- mean_loss_val
    
    acc_train[i] <- acc_bal_train 
    acc_val[i] <- acc_bal_val 
}

############# Plotting Loss ############
plot(loss_train, xlab="Complexity", ylab="Loss", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(loss_train, loss_val)),
            max(c(loss_train, loss_val))))


axis(1, at=1:3, labels=seq(from = 1, to = 3, by = 1), las=1)
points(loss_val, pch="*", col="blue")
points(rep(loss_baseline, length(loss_val)), pch="o", col="green")

lines(loss_train, col="red", lty=2)
lines(loss_val, col="blue", lty=2)
lines(rep(loss_baseline, length(loss_val)), col="green", lty=2)
legend(2.3, 0.465, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)

############ Ploting Acc Balanced ############
plot(acc_train, xlab="Complexity", ylab="Balanced Accuracy", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(acc_train, acc_val)),
            max(c(acc_train, acc_val))))

axis(1, at=1:3, labels=seq(from = 1, to = 3, by = 1), las=1)
points(acc_val, pch="*", col="blue")
points(rep(acc_baseline, length(acc_val)), pch="o", col="green")

lines(acc_train, col="red", lty=2)
lines(acc_val, col="blue", lty=2)
lines(rep(acc_baseline, length(acc_val)), col="green", lty=2)
legend(1.0, 0.81, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)

#### Testing #### 
i<- which.max(acc_val)
i

hypothesis <- getHypothesis(feature_names, i)

x_train <- model.matrix(hypothesis, trainSet)
y_train <- trainSet$class
model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE, 
                alpha=0, maxit = 1e+05, trace.it=1, lambda = 1e-2)

trainPred <- predict(model, newx = x_train, type="response")

x_test <- model.matrix(hypothesis, testSet)
y_test <- testSet$class
testPred <- predict(model, newx = x_test, type="response")

#converting to class
testClassPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

##### Let's see how good the model performs
#Loss 
lossN = getLoss(testSet$class[testSet$class == 0], testPred[testSet$class == 0])
lossP = getLoss(testSet$class[testSet$class == 1], testPred[testSet$class == 1])
mean_loss_test <- (lossN+lossP)/2
mean_loss_test 


cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_test_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_test_bal


############ Combining Features ###########
cor(trainSet[1:(ncol(trainSet)-1)])

f01 <- formula(class ~ .)

f02 <- formula(class ~ . + (LBXTR+LBDHDD+LBXGLT+LBXAPB)^2)

f03 <- formula(class ~ . + (LBXTR+LBDHDD+LBXGLT+LBXAPB)^3)

f04 <- formula(class ~ . + (LBXTR+LBDHDD+LBXGLT+LBXAPB)^4)


formulas <- c(f01, f02, f03, f04)

loss_train <- c()
loss_val <- c()

acc_train <- c()
acc_val <- c()

i <- 1
for(f in formulas){  
    
    
    # Applying hypothesis and training the model
    x_train <- model.matrix(f, trainSet)
    y_train <- trainSet$class
    model <- glmnet(x_train, y_train,  family="binomial", 
                    standardize = FALSE, maxit = 1e+05, 
                    alpha=0, lambda = 1e-2)
    
    trainPred <- predict(model, newx = x_train, type="response")
    
    #converting to class
    trainClassPred <- trainPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    trainClassPred[trainPred >= 0.5] <- 1
    trainClassPred[trainPred < 0.5] <- 0
    #trainClassPred
    
    lossN = getLoss(trainSet$class[trainSet$class == 0], trainPred[trainSet$class == 0])
    lossP = getLoss(trainSet$class[trainSet$class == 1], trainPred[trainSet$class == 1])
    mean_loss_train <- (lossN+lossP)/2
    print(mean_loss_train)
    
    cm <- confusionMatrix(data = as.factor(trainClassPred), 
                          reference = as.factor(trainSet$class), 
                          positive='1')
    
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
    
    
    # Validation
    x_val <- model.matrix(f, valSet)
    y_val <- valSet$class
    valPred <- predict(model, newx = x_val, type="response")
    
    #converting to class
    valClassPred <- valPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    valClassPred[valPred >= 0.5] <- 1
    valClassPred[valPred < 0.5] <- 0
    
    ##### Let's see how well we did
    #Loss 
    lossN = getLoss(valSet$class[valSet$class == 0], valPred[valSet$class == 0])
    lossP = getLoss(valSet$class[valSet$class == 1], valPred[valSet$class == 1])
    mean_loss_val <- (lossN+lossP)/2
    
    
    cm <- confusionMatrix(data = as.factor(valClassPred), 
                          reference = as.factor(valSet$class), 
                          positive='1')
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
    
    loss_train[i] <- mean_loss_train
    loss_val[i] <- mean_loss_val
    
    acc_train[i] <- acc_bal_train 
    acc_val[i] <- acc_bal_val 
    
    i <- i + 1
}

############# Plotting Loss ############
plot(loss_train, xlab="Complexity", ylab="Loss", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(loss_train, loss_val)),
            max(c(loss_train, loss_val))))


axis(1, at=1:4, labels=seq(from = 1, to = 4, by = 1), las=1)
points(loss_val, pch="*", col="blue")
points(rep(loss_baseline, length(loss_val)), pch="o", col="green")

lines(loss_train, col="red", lty=2)
lines(loss_val, col="blue", lty=2)
lines(rep(loss_baseline, length(loss_val)), col="green", lty=2)
legend(3.0, 0.48, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)

############ Ploting Acc Balanced ############
plot(acc_train, xlab="Complexity", ylab="Balanced Accuracy", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(acc_train, acc_val)),
            max(c(acc_train, acc_val))))

axis(1, at=1:4, labels=seq(from = 1, to = 4, by = 1), las=1)
points(acc_val, pch="*", col="blue")
points(rep(acc_baseline, length(acc_val)), pch="o", col="green")

lines(acc_train, col="red", lty=2)
lines(acc_val, col="blue", lty=2)
lines(rep(acc_baseline, length(acc_val)), col="green", lty=2)
legend(1.0, 0.835, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=0.7)


#### Testing #### 
i<- which.max(acc_val)

f <- formulas[[i]]
x_train <- model.matrix(f, trainSet)
y_train <- trainSet$class
model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE, 
                alpha=0, maxit = 1e+05, trace.it=1, lambda = 1e-2)


x_test <- model.matrix(f, testSet)
y_test <- testSet$class
testPred <- predict(model, newx = x_test, type="response")

#converting to class
testClassPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

##### Let's see how good the model performs
#Loss 
lossN = getLoss(testSet$class[testSet$class == 0], testPred[testSet$class == 0])
lossP = getLoss(testSet$class[testSet$class == 1], testPred[testSet$class == 1])
mean_loss_test <- (lossN+lossP)/2
mean_loss_test


cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
acc_bal_test <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_test




