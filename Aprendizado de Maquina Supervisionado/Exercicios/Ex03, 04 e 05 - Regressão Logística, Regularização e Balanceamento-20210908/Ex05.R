##########################################
# MDC010 - Aprendizado Supervisionado 01 #
# Exercício 05 - Balanceamento           #
##########################################


library(glmnet)
library(caret)

source("support_functions.R")
source("DMwR.R")

set.seed(12)

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

# Z-norm normalization
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
hypothesis <- getHypothesis(feature_names, 1)

x_train <- model.matrix(hypothesis, trainSet)
y_train <- trainSet$class

model <-  glmnet(x_train, y_train,  family="binomial", maxit=1e+5,
                 standardize = FALSE, alpha=0, lambda = 1e-6)

trainPred <- predict(model, newx = x_train, type="response")

#converting to class
trainClassPred <- trainPred

#### THRESHOLD ####
# Threshold = 0.5 
trainClassPred[trainPred >= 0.5] <- 1
trainClassPred[trainPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(trainClassPred), 
                      reference = as.factor(trainSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
acc_bal_train_baseline <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_train_baseline

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

cm <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valSet$class), 
                      positive='1')


cm_relative <- calculaMatrizConfusaoRelativa(cm)
acc_bal_baseline <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_baseline


####### Balanceamento por ponderacao da funcao de erro #######

classes_frequency = table(trainSet$class)
classes_frequency

relative_classes_frequency = classes_frequency/sum(classes_frequency)
relative_classes_frequency

w_positive = 1 - relative_classes_frequency[2]
w_negative = 1 - relative_classes_frequency[1]

w_positive
w_negative

# Inicializando com zeros o vetor de pesos
weights <- rep(0.0, dim(trainSet)[1])

# Associando o peso dos positivos (w_positive) aos respectivos exemplos
weights[trainSet$class == 1] = w_positive 

# Associando o peso dos negatives (w_negative) aos respectivos exemplos
weights[trainSet$class == 0] = w_negative 

x_train <- model.matrix(hypothesis, trainSet)
y_train <- trainSet$class

logRegModel_weighting <- glmnet(x_train, y_train,  family="binomial",
                                   weights = weights,
                                   standardize = FALSE, alpha=0, lambda = 1e-6)


x_val <- model.matrix(hypothesis, valSet)
y_val <- valSet$class
valPred <- predict(logRegModel_weighting, newx = x_val, type="response")

#converting to class
valClassPred <- valPred

#### THRESHOLD ####
# Threshold = 0.5 
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
acc_bal_weights <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_weights


####### Balanceamento por Oversampling #######
positiveData <- trainSet[trainSet$class == 1,]
negativeData <- trainSet[trainSet$class == 0,]

dim(positiveData)
dim(negativeData)

# increasing in 2x
selectedIndex <- sample(1:nrow(positiveData), 2*nrow(positiveData), replace=TRUE)
oversampledPosData <- positiveData[selectedIndex,]
dim(oversampledPosData)

newTrainData <- rbind(oversampledPosData, negativeData)
dim(newTrainData)
table(newTrainData$class)


x_train <- model.matrix(hypothesis, newTrainData)
y_train <- newTrainData$class

logRegModel_oversampling <- glmnet(x_train, y_train,  family="binomial", 
                                   standardize = FALSE, alpha=0, lambda = 1e-6)


x_val <- model.matrix(hypothesis, valSet)
y_val <- valSet$class
valPred <- predict(logRegModel_oversampling, newx = x_val, type="response")

#converting to class
valClassPred <- valPred

#### THRESHOLD ####
# Threshold = 0.5 
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
acc_bal_oversampling <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_oversampling


####### Balanceamento por Undersampling #######
positiveData <- trainSet[trainSet$class == 1,]
negativeData <- trainSet[trainSet$class == 0,]

dim(positiveData)
dim(negativeData)

selectedIndex <- sample(1:nrow(negativeData), 1.2*nrow(positiveData), replace=FALSE)
undersampledNegData <- negativeData[selectedIndex,]
dim(undersampledNegData)
dim(positiveData)

newTrainData <- rbind(positiveData, undersampledNegData)
dim(newTrainData)
table(newTrainData$class)

x_train <- model.matrix(hypothesis, newTrainData)
y_train <- newTrainData$class

logRegModel_undersampling <- glmnet(x_train, y_train,  family="binomial", 
                                    standardize = FALSE, alpha=0, lambda = 1e-6)


x_val <- model.matrix(hypothesis, valSet)
y_val <- valSet$class
valPred <- predict(logRegModel_undersampling, newx = x_val, type="response")

#converting to class
valClassPred <- valPred

#### THRESHOLD ####
# Threshold = 0.5 
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
acc_bal_undersampling <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_undersampling


##################################
############ SMOTE ###############
##################################
#help(SMOTE)
# Be carefull!!!! SMOTE only works with factor as labels
trainSet$Class <- as.factor(trainSet$class) # Categorization
newTrainData <- SMOTE(hypothesis, trainSet, 
                      perc.over = 100,  
                      perc.under = 200, 
                      k=3)
#per.over/100 is the number of new cases (smoted cases) generated 
# for each rare case

# perc.under/100 is the number of "normal" cases that are randomly 
# selected for each smoted case

## So, for each of the five training samples we generate more 5, following 
## the criteria of k = 3 neighbors. Then we have 10 training positive examples.
## For each generated positive sample, in other words, for each of the five 
## generated samples, we select two samples from negative data. Then we have
## 5*2 = 10 sampled negative data. 
## In total we have 10 (positive) + 10 (negative) = 20 (total examples)

dim(newTrainData)
table(newTrainData$class)

x_train <- model.matrix(hypothesis, newTrainData)
y_train <- newTrainData$class

logRegModel_SMOTE <- glmnet(x_train, y_train,  family="binomial", 
                            standardize = FALSE, alpha=0, lambda=1e-6)


x_val <- model.matrix(hypothesis, valSet)
y_val <- valSet$class
valPred <- predict(logRegModel_SMOTE, newx = x_val, type="response")

#converting to class
valClassPred <- valPred

#### THRESHOLD ####
# Threshold = 0.5 
valClassPred[valPred >= 0.5] <- 1
valClassPred[valPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(valClassPred), 
                      reference = as.factor(valSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
acc_bal_smote <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_smote

##### Getting best balancing technique #####
acc_bal_baseline*100
acc_bal_weights*100
acc_bal_oversampling*100
acc_bal_undersampling*100
acc_bal_smote*100


x_test <- model.matrix(hypothesis, testSet)
y_test <- testSet$class

## logRegModel here comes from undersampling! Since it gave the best
## performance on validation set after balancing
testPred <- predict(logRegModel_weighting, newx = x_test, type="response")

#converting to class
testClassPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testClassPred[testPred >= 0.5] <- 1
testClassPred[testPred < 0.5] <- 0

cm <- confusionMatrix(data = as.factor(testClassPred), 
                      reference = as.factor(testSet$class), 
                      positive='1')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
acc_bal_weighting_test <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_weighting_test

