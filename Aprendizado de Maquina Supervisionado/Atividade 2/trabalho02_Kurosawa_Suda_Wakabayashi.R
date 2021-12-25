#----------------------------------------------------------------#
# INF-0615 Aprendizado de Maquina Supervisionado I       
#                       
# Trabalho Avaliativo 1 
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes dp grupo:  
# - Daniel Noriaki Kurosawa                                        
# - Eric Uyemura Suda                                       
# - Fernando Shigeru Wakabayashi                                        
# 
#----------------------------------------------------------------#

setwd("/Users/nkuros/Documents/mineiracao_dados_complexos/Aprendizado de Maquina Supervisionado/Atividade 2/")


rm(list=ls())
graphics.off()

calculaMatrizConfusaoRelativa <- function(cm){
    
    # Aplicamos a transposi��o para garantir que a referencia
    # fique nas linhas e a predicao nas colunas
    cm_absolute = t(cm$table)
    
    # SEMPRE construam e reportem a matriz de confusao relativa!
    cm_relative = cm_absolute
    
    cm_relative[1,1] = round(cm_absolute[1,1]/sum(cm_absolute[1,]), digits=2)
    cm_relative[1,2] = round(cm_absolute[1,2]/sum(cm_absolute[1,]), digits=2)
    cm_relative[2,1] = round(cm_absolute[2,1]/sum(cm_absolute[2,]), digits=2)
    cm_relative[2,2] = round(cm_absolute[2,2]/sum(cm_absolute[2,]), digits=2)
    
    return(cm_relative)  
}

# Escreve a funcao de hipotese dada as features continuas e o 
# respectivo grau polinomial
getHypothesis <- function(feature_names, degree){
    
    hypothesis_string <- "hypothesis <- formula(target ~ "
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


getLoss <- function(y_true, y_pred){
    y_true <- as.numeric(y_true) - 1
    
    totalLoss <- 0
    eps <- 1e-9
    # Recall: length(y_true) == length(y_pred)
    # loss = (1-y)*log2(1 - p + eps)) + y*log(p + eps)
    # eps is used for numerical stability, it is very close to 0.
    # Supose we have y = 1 and p = 1 (perfect prediction), the loss (without eps)
    # would be 0*log2(0) + 1*log(1). It would result in NaN
    # because of 0*log2(0). With eps: 0*log2(1e-9) + 1*log(1 + 1e-9) 
    for(i in 1:length(y_true)){
        loss <- -1*((1 - y_true[i])*log2(1 - y_pred[i] + eps) + y_true[i]*log2(y_pred[i] + eps))
        totalLoss <- totalLoss + loss
    }
    totalLoss <- totalLoss/(length(y_true))
    return(totalLoss)
}


TNR_TPR <- function(cm){
    TNR <- cm$table[1,1]/(cm$table[1,1] + cm$table[2,1])
    
    TPR <- cm$table[2,2]/(cm$table[1,2] + cm$table[2,2])
    
    return(c(TNR,TPR ))
}



# Comandos que leem os conjuntos de treino e de validacao

train_set <- read.csv("./proteins_training_set.csv", stringsAsFactors=TRUE)
val_set <- read.csv("./proteins_validation_set.csv", stringsAsFactors=TRUE)
test_set <- read.csv("./proteins_test_set.csv", stringsAsFactors=TRUE)
SARS_set <- read.csv("./SARS_test_set.csv", stringsAsFactors=TRUE)

# Desenvolvam o trabalho a partir daqui, apos executarem os comandos a cima

# install.packages(ggplot2)
# install.packages(dplyr)
# install.packages(reshape2)
# install.packages(glmnet)
# install.packages(caret)
# install.packages(pROC)


library(ggplot2)
library(dplyr)
library(reshape2)
library(glmnet)
library(caret)
library(pROC)
#### Tarefas

#---
# 1.
#---
# Validacao se os conjuntos sao disjuntos
## Ok, todos os conjuntos sao disjuntos
merge(train_set, val_set)
merge(train_set, test_set)
merge(test_set, val_set)

# Validacao de volumetria das bases

#---------------------------------
# Verificacao do dataset de treino
#---------------------------------
summary(train_set)
dim(train_set)
colnames(train_set)
table(train_set$target)
count(train_set[train_set$target == 0,])
count(train_set[train_set$target == 1,])


#------------------------------------
# Verificacao do dataset de validacao
#------------------------------------
summary(val_set)
dim(val_set)
colnames(val_set)
table(val_set$target)
count(val_set[val_set$target == 0,])
count(val_set[val_set$target == 1,])

#--------------------------------
# Verificacao do dataset de teste
#--------------------------------
summary(test_set)
dim(test_set)
colnames(test_set)
table(test_set$target)
count(test_set[test_set$target == 0,])
count(test_set[test_set$target == 1,])


#--------------------------------
# Verificacao do dataset de SARS
#--------------------------------
summary(SARS_set)
dim(SARS_set)
colnames(SARS_set)
table(SARS_set$target)
count(SARS_set[SARS_set$target == 0,])
count(SARS_set[SARS_set$target == 1,])




#########
# Visualizacao da distribuição das variaveis
melt_train_set <- train_set
melt_train_set <- melt(melt_train_set)

p <- ggplot(data=melt_train_set, aes(x=value))+
    stat_density()+
    facet_wrap(~variable, scales='free'); p


#########
# Devido a distribuicao nao normal dos dados iremos aplicar uma normalizacao min-max
# exceto para as variaveis de data e a variavel target
min_features <- apply(train_set[,1:(ncol(train_set)-1)], 2, min); min_features
max_features <- apply(train_set[,1:(ncol(train_set)-1)], 2, max); max_features

diff <- max_features - min_features; diff

train_set[,1:(ncol(train_set)-1)] <- sweep(train_set[,1:(ncol(train_set)-1)], 2, min_features, "-")
train_set[,1:(ncol(train_set)-1)] <- sweep(train_set[,1:(ncol(train_set)-1)], 2, diff, "/")
summary(train_set)

val_set[,1:(ncol(val_set)-1)] <- sweep(val_set[,1:(ncol(val_set)-1)], 2, min_features, "-")
val_set[,1:(ncol(val_set)-1)] <- sweep(val_set[,1:(ncol(val_set)-1)], 2, diff, "/")
summary(val_set)

test_set[,1:(ncol(test_set)-1)] <- sweep(test_set[,1:(ncol(test_set)-1)], 2, min_features, "-")
test_set[,1:(ncol(test_set)-1)] <- sweep(test_set[,1:(ncol(test_set)-1)], 2, diff, "/")
summary(test_set)

SARS_set[,1:(ncol(SARS_set)-1)] <- sweep(SARS_set[,1:(ncol(SARS_set)-1)], 2, min_features, "-")
SARS_set[,1:(ncol(SARS_set)-1)] <- sweep(SARS_set[,1:(ncol(SARS_set)-1)], 2, diff, "/")
summary(SARS_set)
#--------------------------------------------
# Transformando target em variável categorica
#--------------------------------------------
train_set$target <- as.factor(train_set$target)
val_set$target <- as.factor(val_set$target)
test_set$target <- as.factor(test_set$target)
SARS_set$target <- as.factor(SARS_set$target)

#------------------------------------------------------------------------------------------------
# Visualizacao dos dados apos normalizacao min max (Visualmente nao alterou muito a distribuicao)
#------------------------------------------------------------------------------------------------
melt_train_set <- val_set
melt_train_set <- melt(melt_train_set)

ggplot(data=melt_train_set, aes(x=value))+
    stat_density()+
    facet_wrap(~variable, scales='free')


#---------
# Baseline
#---------

# ITERACAO 1 - Validacao Balancemento
# Treinando o Modelo - targetificação #
feature_names <- colnames(train_set)[1:(ncol(train_set)-1)]
feature_names

# Gerando uma função com features a primeira potência para regressão logística
hypothesis <- getHypothesis(feature_names, 1)
hypothesis

x_train <- model.matrix(hypothesis, train_set)
y_train <- train_set$target

model <- glmnet(x_train,
                y_train, 
                family="binomial", #descrição para regressão logística
                standardize = FALSE,
                maxit = 1e+05,
                alpha=0, #tipo de regularização L2, 1 = l1
                lambda = 1e-6
                )

### Verificando os thetas aprendidos ###
model$beta
model$a0 # valor do theta0 (intercept)

#Previsões
trainPred <- predict(model, newx = x_train, type="response")
head(trainPred)

#Convertendo para targets
traintargetPred <- trainPred

#Modificando para Limiar 0.5 , retorno de 0 ou 1
threshold <- 0.5
traintargetPred[trainPred >= threshold] <- 1
traintargetPred[trainPred < threshold] <- 0

#traintargetPred

#Valor de loss
lossN = getLoss(train_set$target[train_set$target == 0], trainPred[train_set$target == 0])
lossP = getLoss(train_set$target[train_set$target == 1], trainPred[train_set$target == 1])
lossN
lossP
(lossN+lossP)/2


#Matriz de confusão 

cm <- confusionMatrix(data = as.factor(traintargetPred), 
                      reference = as.factor(train_set$target), 
                      positive='1')
cm$table

#TNR e TPR
TNR_TPR(cm)

#Matriz de confusão 
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

#acuracia
acc_baseline_train <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_baseline_train


### Predicao no conjunto de validacao ###
x_val <- model.matrix(hypothesis, val_set)
y_val <- val_set$target
valPred <- predict(model, newx = x_val, type="response")


#converting to target
valtargetPred <- valPred


#### THRESHOLD ####
# Threshold = 0.5 
valtargetPred[valPred >= 0.5] <- 1
valtargetPred[valPred < 0.5] <- 0

# Matriz transposta relativa
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

# Acuracia Balanceada de validacao
acc_baseline_val <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_baseline_val



##### Let's see how well we did
#Loss 
lossN = getLoss(val_set$target[val_set$target == 0], valPred[val_set$target == 0])
lossP = getLoss(val_set$target[val_set$target == 1], valPred[val_set$target == 1])
lossN
lossP
loss_baseline <- (lossN+lossP)/2
loss_baseline

cm <- confusionMatrix(data = as.factor(valtargetPred), 
                      reference = as.factor(val_set$target), 
                      positive='1')
# TNR e TPR
TNR_TPR(cm)

# CM relativa
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

# Acuracia baseline
acc_baseline <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_baseline

# ROC Curve for baseline
ROC <- roc(val_set$target, valPred[,1], direction="<")
ROC

plot(ROC, col="blue", lwd=2, main="ROC")

##### BASELINE COM TESTES #######

x_test <- model.matrix(hypothesis,test_set )
y_test <- test_set$target
testPred <- predict(model, newx = x_test, type="response")


#converting to target
testtargetPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testtargetPred[testPred >= 0.5] <- 1
testtargetPred[testPred < 0.5] <- 0

#testtargetPred

cm <- confusionMatrix(data = as.factor(testtargetPred), 
                      reference = as.factor(test_set$target), 
                      positive='1')

#calcula TNR TPR
TNR_TPR(cm)

#matriz transposta relativa
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

#
acc_baseline_test <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_baseline_test


#-----------------------------------------------
# Balanceamento por ponderacao da funcao de erro
#-----------------------------------------------

target_frequency = table(train_set$target)
target_frequency

relative_target_frequency = target_frequency/sum(target_frequency)
relative_target_frequency

w_positive = 1 - relative_target_frequency[2]
w_negative = 1 - relative_target_frequency[1]

w_positive
w_negative

# Inicializando com zeros o vetor de pesos
weights <- rep(0.0, dim(train_set)[1])

# Associando o peso dos positivos (w_positive) aos respectivos exemplos
weights[train_set$target == 1] = w_positive 

# Associando o peso dos negatives (w_negative) aos respectivos exemplos
weights[train_set$target == 0] = w_negative 

# Modelo Baseline com os Pesos utilizados
x_train <- model.matrix(hypothesis, train_set)
y_train <- train_set$target

logRegModel_weighting <- glmnet(x_train, y_train,  family="binomial",
                                   weights = weights,
                                   standardize = FALSE, alpha=0, lambda = 1e-6)


x_val <- model.matrix(hypothesis, val_set)
y_val <- val_set$target
valPred <- predict(logRegModel_weighting, newx = x_val, type="response")

#observar e comparar com o valbaseline
valtargetPred <- valPred

#### THRESHOLD ####
# Threshold = 0.5 
valtargetPred[valPred >= 0.5] <- 1
valtargetPred[valPred < 0.5] <- 0
#matriz transposta relativa
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

# Acurácia Baseline validação
acc_baseline <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_baseline



##### Let's see how well we did
#Loss 
lossN = getLoss(val_set$target[val_set$target == 0], valPred[val_set$target == 0])
lossP = getLoss(val_set$target[val_set$target == 1], valPred[val_set$target == 1])
lossN
lossP
loss_baseline <- (lossN+lossP)/2
loss_baseline

cm <- confusionMatrix(data = as.factor(valtargetPred), 
                      reference = as.factor(val_set$target), 
                      positive='1')


cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_baseline <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_baseline

# ROC Curve for baseline
ROC <- roc(val_set$target, valPred[,1], direction="<")
ROC

plot(ROC, col="blue", lwd=2, main="ROC")



############# Analise Polinomial ###########
loss_train <- c()
loss_val <- c()

acc_train <- c()
acc_val <- c()

feature_names <- colnames(train_set)[1:(ncol(train_set)-1)]

## Polynomial Analysis
### Be careful! Higher polynomial degrees might not converge!
for(i in 1:10){  
    
    print(i)
    hypothesis <- getHypothesis(feature_names, i)
    
    # Applying hypothesis and training the model
    x_train <- model.matrix(hypothesis, train_set)
    y_train <- train_set$target
    model <- glmnet(x_train, y_train,  family="binomial", 
                    standardize = FALSE, maxit = 1e+05, 
                    weights = weights,
                    alpha=0, lambda = 1e-6)
    
    trainPred <- predict(model, newx = x_train, type="response")
    
    #converting to target
    traintargetPred <- trainPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    traintargetPred[trainPred >= 0.5] <- 1
    traintargetPred[trainPred < 0.5] <- 0
    #traintargetPred
    
    lossN = getLoss(train_set$target[train_set$target == 0], trainPred[train_set$target == 0])
    lossP = getLoss(train_set$target[train_set$target == 1], trainPred[train_set$target == 1])
    mean_loss_train <- (lossN+lossP)/2
    
    cm <- confusionMatrix(data = as.factor(traintargetPred), 
                          reference = as.factor(train_set$target), 
                          positive='1')
    
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
 
    # Validation
    x_val <- model.matrix(hypothesis, val_set)
    y_val <- val_set$target
    valPred <- predict(model, newx = x_val, type="response")
    
    #converting to target
    valtargetPred <- valPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    valtargetPred[valPred >= 0.5] <- 1
    valtargetPred[valPred < 0.5] <- 0
    
    ##### Let's see how well we did
    #Loss 
    lossN = getLoss(val_set$target[val_set$target == 0], valPred[val_set$target == 0])
    lossP = getLoss(val_set$target[val_set$target == 1], valPred[val_set$target == 1])
    mean_loss_val <- (lossN+lossP)/2
    mean_loss_val
    
    cm <- confusionMatrix(data = as.factor(valtargetPred), 
                          reference = as.factor(val_set$target), 
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
     ylim=c(min(c(loss_train, loss_val,loss_baseline)),
            max(c(loss_train, loss_val,loss_baseline))))


axis(1, at=1:10, labels=seq(from = 1, to = 10, by = 1), las=1)
points(loss_val, pch="*", col="blue")
points(rep(loss_baseline, length(loss_val)), pch="o", col="green")

lines(loss_train, col="red", lty=2)
lines(loss_val, col="blue", lty=2)
lines(rep(loss_baseline, length(loss_val)), col="green", lty=2)
legend(3.5, 0.950, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=1.5)

############ Ploting Acc Balanced ############
plot(acc_train, xlab="Complexity", ylab="Balanced Accuracy", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(acc_train, acc_val,acc_baseline)),
            max(c(acc_train, acc_val,acc_baseline))))

axis(1, at=1:10, labels=seq(from = 1, to = 10, by = 1), las=1)
points(acc_val, pch="*", col="blue")
points(rep(acc_baseline, length(acc_val)), pch="o", col="green")

lines(acc_train, col="red", lty=2)
lines(acc_val, col="blue", lty=2)
lines(rep(acc_baseline, length(acc_val)), col="green", lty=2)
legend(7, 0.635, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=1.5)



#### Melhor Modelo Exponencial #### 
i<- which.max(acc_val)
i

hypothesis <- getHypothesis(feature_names, i)

x_train <- model.matrix(hypothesis, train_set)
y_train <- train_set$target
model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE, 
                alpha=0, maxit = 1e+05, trace.it=1, lambda = 1e-6, weights = weights)

trainPred <- predict(model, newx = x_train, type="response")

#### Melhor modelo com dataset de Validacao ####

x_val <- model.matrix(hypothesis, val_set)
y_val <- val_set$target
valPred <- predict(model, newx = x_val, type="response")

#convertendo para target
valtargetPred <- valPred

#### THRESHOLD ####
# Threshold = 0.5 
valtargetPred[valPred >= 0.5] <- 1
valtargetPred[valPred < 0.5] <- 0

##### Verificando a performance do melhor
#Loss 
lossN = getLoss(val_set$target[val_set$target == 0], valPred[val_set$target == 0])
lossP = getLoss(val_set$target[val_set$target == 1], valPred[val_set$target == 1])
mean_loss_val <- (lossN+lossP)/2
mean_loss_val 

# Matriz de confusão para do dataset de validação com o melhor modelo exponencial
cm <- confusionMatrix(data = as.factor(valtargetPred), 
                      reference = as.factor(val_set$target), 
                      positive='1')
#TNR e TPR
TNR_TPR(cm)

# Matriz de confusão relativa - validação
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

# Acurácia Balanceada - validação
acc_val_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_val_bal

#### Melhor modelo com dataset de Teste #### 

trainPred <- predict(model, newx = x_train, type="response")

x_test <- model.matrix(hypothesis, test_set)
y_test <- test_set$target
testPred <- predict(model, newx = x_test, type="response")

#convertendo para target
testtargetPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testtargetPred[testPred >= 0.5] <- 1
testtargetPred[testPred < 0.5] <- 0

##### Let's see how  the model performs
#Loss 
lossN = getLoss(test_set$target[test_set$target == 0], testPred[test_set$target == 0])
lossP = getLoss(test_set$target[test_set$target == 1], testPred[test_set$target == 1])
mean_loss_test <- (lossN+lossP)/2
mean_loss_test 

# Matriz de confusão - teste
cm <- confusionMatrix(data = as.factor(testtargetPred), 
                      reference = as.factor(test_set$target), 
                      positive='1')
#TNR e TPR
TNR_TPR(cm)

# Matriz de confusão Relativa - teste
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

# Acurácia balanceada - teste
acc_test_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_test_bal


############ Combining Features ###########
cor(train_set[1:(ncol(train_set)-1)])


teste <- getHypothesis(feature_names, 2)

f01 <- formula(target ~ .)

f02 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar+parker+isoelectric_point+aromaticity+hydrophobicity+stability)^2)

f03 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar+parker+isoelectric_point+aromaticity+hydrophobicity+stability)^3)

f04 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar+parker+isoelectric_point+aromaticity+hydrophobicity+stability)^4)

f05 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar+parker+isoelectric_point+aromaticity+hydrophobicity+stability)^5)

f06 <- formula(target ~ . + (start_position+end_position+chou_fasman+emini+kolaskar_tongaonkar+parker+isoelectric_point+aromaticity+hydrophobicity+stability)^6)



formulas <- c(f01, f02, f03, f04, f05, f06)

loss_train <- c()
loss_val <- c()

acc_train <- c()
acc_val <- c()

i <- 1
for(f in formulas){  
    
    
    # Applying hypothesis and training the model
    x_train <- model.matrix(f, train_set)
    y_train <- train_set$target
    model <- glmnet(x_train, y_train,  family="binomial", 
                    standardize = FALSE, maxit = 1e+05, weights = weights,
                    alpha=0, lambda = 1e-6)
    
    trainPred <- predict(model, newx = x_train, type="response")
    
    #converting to target
    traintargetPred <- trainPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    traintargetPred[trainPred >= 0.5] <- 1
    traintargetPred[trainPred < 0.5] <- 0
    #traintargetPred
    
    lossN = getLoss(train_set$target[train_set$target == 0], trainPred[train_set$target == 0])
    lossP = getLoss(train_set$target[train_set$target == 1], trainPred[train_set$target == 1])
    mean_loss_train <- (lossN+lossP)/2
    print(mean_loss_train)
    
    cm <- confusionMatrix(data = as.factor(traintargetPred), 
                          reference = as.factor(train_set$target), 
                          positive='1')
    
    
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
    
    
    # Validation
    x_val <- model.matrix(f, val_set)
    y_val <- val_set$target
    valPred <- predict(model, newx = x_val, type="response")
    
    #converting to target
    valtargetPred <- valPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    valtargetPred[valPred >= 0.5] <- 1
    valtargetPred[valPred < 0.5] <- 0
    
    ##### Let's see how well we did
    #Loss 
    lossN = getLoss(val_set$target[val_set$target == 0], valPred[val_set$target == 0])
    lossP = getLoss(val_set$target[val_set$target == 1], valPred[val_set$target == 1])
    mean_loss_val <- (lossN+lossP)/2
    
    
    cm <- confusionMatrix(data = as.factor(valtargetPred), 
                          reference = as.factor(val_set$target), 
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
     ylim=c(min(c(loss_train, loss_val,loss_baseline)),
            max(c(loss_train, loss_val,loss_baseline))))


axis(1, at=1:6, labels=seq(from = 1, to = 6, by = 1), las=1)
points(loss_val, pch="*", col="blue")
points(rep(loss_baseline, length(loss_val)), pch="o", col="green")

lines(loss_train, col="red", lty=2)
lines(loss_val, col="blue", lty=2)
lines(rep(loss_baseline, length(loss_val)), col="green", lty=2)
legend(4.0, 0.94, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=1.5)

############ Ploting Acc Balanced ############
plot(acc_train, xlab="Complexity", ylab="Balanced Accuracy", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(acc_train, acc_val,acc_baseline)),
            max(c(acc_train, acc_val,acc_baseline))))

axis(1, at=1:6, labels=seq(from = 1, to = 6, by = 1), las=1)
points(acc_val, pch="*", col="blue")
points(rep(acc_baseline, length(acc_val)), pch="o", col="green")

lines(acc_train, col="red", lty=2)
lines(acc_val, col="blue", lty=2)
lines(rep(acc_baseline, length(acc_val)), col="green", lty=2)
legend(4.0, 0.64, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=1.5)


#### Melhor modelo Combinacao de Features #### 
i<- which.max(acc_val)
i

f <- formulas[[i]]
x_train <- model.matrix(f, train_set)
y_train <- train_set$target
model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE, weights = weights,
                alpha=0, maxit = 1e+05, trace.it=1, lambda = 1e-6)

##Validando Modelo####
x_val <- model.matrix(f, val_set)
y_val <- val_set$target

valPred <- predict(model, newx = x_val, type="response")

#convertendo para target
valtargetPred <- valPred

#### THRESHOLD ####
# Threshold = 0.5 
valtargetPred[valPred >= 0.5] <- 1
valtargetPred[valPred < 0.5] <- 0

##### Verificando a performance do melhor modelo
#Loss 
lossN = getLoss(val_set$target[val_set$target == 0], valPred[val_set$target == 0])
lossP = getLoss(val_set$target[val_set$target == 1], valPred[val_set$target == 1])
mean_loss_val <- (lossN+lossP)/2
mean_loss_val 
#
cm <- confusionMatrix(data = as.factor(valtargetPred), 
                      reference = as.factor(val_set$target), 
                      positive='1')
#TNR e TPR
TNR_TPR(cm)

# Matriz de confusão Relativa - Validação
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

# Acurácia balanceada - Validação
acc_val_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_val_bal

#### Melhor modelo com dataset de Teste #### 

trainPred <- predict(model, newx = x_train, type="response")

x_test <- model.matrix(f, test_set)
y_test <- test_set$target
testPred <- predict(model, newx = x_test, type="response")

#converting to target
testtargetPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testtargetPred[testPred >= 0.5] <- 1
testtargetPred[testPred < 0.5] <- 0

##### Let's see how good the model performs
#Loss 
lossN = getLoss(test_set$target[test_set$target == 0], testPred[test_set$target == 0])
lossP = getLoss(test_set$target[test_set$target == 1], testPred[test_set$target == 1])
mean_loss_test <- (lossN+lossP)/2
mean_loss_test

# Matriz de confusão para o dataset de teste com o melhor modelo
cm <- confusionMatrix(data = as.factor(testtargetPred), 
                      reference = as.factor(test_set$target), 
                      positive='1')

# TNR e TPR
TNR_TPR(cm)

# Matriz de confusão relativa - Teste
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

#Acuracia Balanceada para o dataset de Teste
acc_bal_test <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_test


#----------------------------------------------
# Teste de Regularizacao com diferentes Lambdas
#----------------------------------------------
loss_train <- c()
loss_val <- c()

acc_train <- c()
acc_val <- c()

lambda_values <- c(1.0, 0.1, 1e-2, 1e-3, 1e-4, 1e-5, 1e-6, 1e-7 , 1e-8, 1e-9, 1e-10)

i <- 1
for(l in lambda_values){
    
    print(l)
    # Applying hypothesis and training the model
    x_train <- model.matrix(f, train_set)
    y_train <- train_set$target
    model <- glmnet(x_train, y_train,  family="binomial", 
                    standardize = FALSE, maxit = 1e+05, 
                    weights = weights,
                    alpha=0, lambda = l)
    
    trainPred <- predict(model, newx = x_train, type="response")
    
    #converting to target
    traintargetPred <- trainPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    traintargetPred[trainPred >= 0.5] <- 1
    traintargetPred[trainPred < 0.5] <- 0
    #traintargetPred
    
    lossN = getLoss(train_set$target[train_set$target == 0], trainPred[train_set$target == 0])
    lossP = getLoss(train_set$target[train_set$target == 1], trainPred[train_set$target == 1])
    mean_loss_train <- (lossN+lossP)/2
    
    # Matriz de Confusão
    cm <- confusionMatrix(data = as.factor(traintargetPred), 
                          reference = as.factor(train_set$target), 
                          positive='1')
    
    # Matriz de confusão balanceada
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    
    # Acuracia balanceada
    acc_bal_train <- (cm_relative[1,1] + cm_relative[2,2])/2
    
    
    # Validation
    x_val <- model.matrix(f, val_set)
    y_val <- val_set$target
    valPred <- predict(model, newx = x_val, type="response")
    
    #converting to target
    valtargetPred <- valPred
    
    #### THRESHOLD ####
    # Threshold = 0.5 
    valtargetPred[valPred >= 0.5] <- 1
    valtargetPred[valPred < 0.5] <- 0
    
    ##### Let's see how well we did
    #Loss 
    lossN = getLoss(val_set$target[val_set$target == 0], valPred[val_set$target == 0])
    lossP = getLoss(val_set$target[val_set$target == 1], valPred[val_set$target == 1])
    mean_loss_val <- (lossN+lossP)/2
    
    # Matriz de confusão
    cm <- confusionMatrix(data = as.factor(valtargetPred), 
                          reference = as.factor(val_set$target), 
                          positive='1')
    # Matriz de confusão relativa
    cm_relative <- calculaMatrizConfusaoRelativa(cm)
    
    # Acuracia Balanceada
    acc_bal_val <- (cm_relative[1,1] + cm_relative[2,2])/2
  
    # Loss para treino e validação
    loss_train[i] <- mean_loss_train
    loss_val[i] <- mean_loss_val
    
    # Acuracia para treino e teste
    acc_train[i] <- acc_bal_train 
    acc_val[i] <-acc_bal_val 
    i <- i + 1
    
}

############################################
# Pay attention on warnings! They might or #
# not prejudice your model performance!    #
############################################

############# Plotting Loss ############
plot(loss_train, xlab="Regularization factor (lambda)", ylab="Loss", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(loss_train, loss_val,loss_baseline)),
            max(c(loss_train, loss_val,loss_baseline))))


axis(1, at=1:length(lambda_values), labels=lambda_values, 
     cex.axis=0.5, las=2)

points(loss_val, pch="*", col="blue")
points(rep(loss_baseline, length(loss_val)), pch="o", col="green")

lines(loss_train, col="red", lty=2)
lines(loss_val, col="blue", lty=2)
lines(rep(loss_baseline, length(loss_val)), col="green", lty=2)
legend(2, 0.9, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=1.5)

############ Ploting Acc Balanced ############
plot(acc_train, xlab="Regularization factor (lambda)", ylab="Acc Balanced", 
     pch="+", col="red",  xaxt="n", 
     ylim=c(min(c(acc_train, acc_val,acc_baseline)),
            max(c(acc_train, acc_val,acc_baseline))))

axis(1, at=1:length(lambda_values), labels=lambda_values, 
     cex.axis=0.5, las=2)
points(acc_val, pch="*", col="blue")
points(rep(acc_baseline, length(acc_val)), pch="o", col="green")

lines(acc_train, col="red", lty=2)
lines(acc_val, col="blue", lty=2)
lines(rep(acc_baseline, length(acc_val)), col="green", lty=2)
legend(1, 0.65, legend=c("Train", "Validation", "Baseline"),
       col=c("red","blue","green"), lty=2, cex=1.5)


#### Encontrando melhor lambda baseado na acurácia balanceada e loss na validação ####

i<- 7

best_lambda <- lambda_values[i]
best_lambda

x_train <- model.matrix(hypothesis, train_set)
y_train <- train_set$target
model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE, 
                alpha=0, maxit = 1e+05, trace.it=1, lambda = best_lambda)

trainPred <- predict(model, newx = x_train, type="response")

x_test <- model.matrix(hypothesis, test_set)
y_test <- test_set$target
testPred <- predict(model, newx = x_test, type="response")

#converting to target
testtargetPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testtargetPred[testPred >= 0.5] <- 1
testtargetPred[testPred < 0.5] <- 0

##### Let's see how well the model performs
#Loss 
lossN = getLoss(test_set$target[test_set$target == 0], testPred[test_set$target == 0])
lossP = getLoss(test_set$target[test_set$target == 1], testPred[test_set$target == 1])
mean_loss_test <- (lossN+lossP)/2
mean_loss_test

# Matriz de confusão - Teste
cm <- confusionMatrix(data = as.factor(testtargetPred), 
                      reference = as.factor(test_set$target), 
                      positive='1')
#TNR e TPR
TNR_TPR(cm)

# Matriz de Confusão Relativa - Teste
cm_relative <- calculaMatrizConfusaoRelativa(cm)
acc_bal_test <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_test


#### Melhor modelo Combinacao de Features#### 
i <- 5
f <- formulas[[i]]

x_train <- model.matrix(f, train_set)
y_train <- train_set$target
model <- glmnet(x_train, y_train,  family="binomial", standardize = FALSE, weights = weights,
                alpha=0, maxit = 1e+05, trace.it=1, lambda = 1e-6)

#### Melhor modelo com test set SARS #### 
x_test <- model.matrix(f, SARS_set)
y_test <- SARS_set$target
testPred <- predict(model, newx = x_test, type="response")

#converting to target
testtargetPred <- testPred

#### THRESHOLD ####
# Threshold = 0.5 
testtargetPred[testPred >= 0.5] <- 1
testtargetPred[testPred < 0.5] <- 0

##### Let's see how well the model performs
#Loss 
lossN = getLoss(SARS_set$target[SARS_set$target == 0], testPred[SARS_set$target == 0])
lossP = getLoss(SARS_set$target[SARS_set$target == 1], testPred[SARS_set$target == 1])
mean_loss_test <- (lossN+lossP)/2
mean_loss_test

# Matriz de confusão - SARS
cm <- confusionMatrix(data = as.factor(testtargetPred), 
                      reference = as.factor(SARS_set$target), 
                      positive='1')
                      
# TNR e TPR
TNR_TPR(cm)
# Matriz de confusão relativa  - SARS
cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative
# Acurácia balanceada - SARS
acc_bal_test <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal_test