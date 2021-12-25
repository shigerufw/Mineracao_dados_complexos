#----------------------------------------------------------------#
# INF-0615 Aprendizado de Maquina Supervisionado I       
#                       
####### C�digo de apoio ao Trabalho 03 da disciplina INF-0615 #######
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes dp grupo:  
# - Daniel Noriaki Kurosawa                                        
# - Eric Uyemura Suda                                       
# - Fernando Shigeru Wakabayashi                                        
# 
#----------------------------------------------------------------#

#------------------------------------------------------------------
#0 - Op��es iniciais
#------------------------------------------------------------------

# setwd("/Users/nkuros/Documents/mineiracao_dados_complexos/Aprendizado de Maquina Supervisionado/")
# rm(list = ls())
set.seed(42)

#------------------------------------------------------------------
#1 - Importa Bibliotecas 
#------------------------------------------------------------------
# install.packages('rpart') 
# install.packages('rpart.plot') 
# install.packages('caret') 
# install.packages('ramify') 
# install.packages('ggplot2') 
# install.packages('dplyr') 
# install.packages('hrbrthemes') 
# install.packages('rattle')
# install.packages('reshape2') 
# install.packages('ggplot2') 
# install.packages('randomForest') 

library(rpart)
library(rpart.plot)
library(caret)
library(ramify)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(rattle)
library(reshape2)
library(ggplot2)
library(randomForest)

#------------------------------------------------------------------
#2 - Fun��es �teis
#------------------------------------------------------------------

# Funcao que calcula a matriz de confusao relativa para 3 classes
calculaMatrizConfusaoRelativa <- function(cm){
    
    # Aplicamos a transposi��o para garantir que a referencia
    # fique nas linhas e a predicao nas colunas
    cm_absolute = t(cm$table)
    
    # SEMPRE construam e reportem a matriz de confusao relativa!
    cm_relative = cm_absolute
    
    cm_relative[1,] = round(cm_absolute[1,]/sum(cm_absolute[1,]), digits=2)
    cm_relative[2,] = round(cm_absolute[2,]/sum(cm_absolute[2,]), digits=2)
    cm_relative[3,] = round(cm_absolute[3,]/sum(cm_absolute[3,]), digits=2)
    
    return(cm_relative)  
}

calcula_acc_balanceada <- function(cm_relative){
    acc_balanceada <- (cm_relative[1,1] + cm_relative[2,2]+ cm_relative[3,3])/3
    return(acc_balanceada)  
}

evaluate_model_ret_acc_relative <- function (model,df_val_or_test){
    
    model_preds <- predict(model, df_val_or_test, type = "class")
    
    cm_model_preds <- confusionMatrix(data = as.factor(model_preds), 
                                      reference = as.factor(df_val_or_test$label), 
                                      positive='forgery')
    cm_model_preds_t = t(cm_model_preds$table)
    print("MATRIZ DE CONFUSAO")
    print(cm_model_preds_t)
    print('___________________________________')
    
    
    cm_model_relative <- calculaMatrizConfusaoRelativa(cm_model_preds)
    print("MATRIZ DE CONFUSAO RELATIVA")
    print(cm_model_relative)
    
    
    acc_relativa_models <- calcula_acc_balanceada(cm_model_relative)
    
    print(acc_relativa_models)
    
    return(acc_relativa_models)
    
}

ret_feature_importances <- function(tree_model){
    importance_per_features <- tree_model$variable.importance
    relative_importance <- tree_model$variable.importance/sum(importance_per_features)
    return(relative_importance)
}


getmode <- function(v) {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
}



#------------------------------------------------------------------
# 3- Leitura da base de treinamento+validacao
#------------------------------------------------------------------
data <- read.csv("train_val_set_patient_status_covid19.csv", stringsAsFactors = T)

#3.1 - verificando os dados 
head(data)

colnames(data)

dim(data)

summary(data)


#3.2 - Removendo duplicatas
data <- unique(data)

summary(data)

dim(data)

#3.3 - Resetando �ndice das colunas
row.names(data) <- NULL

#3.4 - verificando desbalanceamento
table(data$label)

#------------------------------------------------------------------
# 4- Separa��o Treino / Valida��o
#------------------------------------------------------------------

# 4.1 - Separa��o rand�mica 
randomTrainValIndexes <- sample(1:nrow(data), size=0.8*nrow(data))

# 4.2 - Cria��o dos dataframes de Treino e valida��o
dataTrain <- data[randomTrainValIndexes, ]
dim(dataTrain)
dataVal  <- data[-randomTrainValIndexes, ] 
dim(dataVal)
# 4.3 - Verificando propor��o dos labels
#train
table(dataTrain$label)
table(dataTrain$label)/length(dataTrain$label)
#validation
table(dataVal$label)
table(dataVal$label)/length(dataVal$label)


#------------------------------------------------------------------
# 5- Treinando Baseline - �rvore de decis�o
#------------------------------------------------------------------

#5.1 - criando modelo
treeModel_baseline <- rpart(formula=label ~ ., 
                            data=dataTrain,
                            method="class",
                            control=rpart.control(minsplit=2,
                                                  cp=0.0,
                                                  xval = 0),
                            parms= list(split="information"))
#5.2- verificando status
#printcp(treeModel)
#summary(treeModel)
#prp(treeModel)

#5.3 - importancia das vari�veis
importance_per_features <- treeModel_baseline$variable.importance
#importance_per_features
relative_importance <- importance_per_features/sum(importance_per_features)
relative_importance

#5.4 - EDA (opcional) das vari�veis mais apontadas como importantes
table(dataTrain$label)

#5.4.1- date_death_or_discharge
p_date_death_or_discharge <- data %>%
    ggplot( aes(x=data$date_death_or_discharge, fill=label)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080", "503060")) +
    theme_ipsum() +
    labs(fill="")
p_date_death_or_discharge

#5.4.2- longitude
p_longitude <- data %>%
    ggplot( aes(x=data$longitude, fill=label)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080", "503060")) +
    theme_ipsum() +
    labs(fill="")
p_longitude

#5.4.3 - date_admission_hospital
p_date_admission_hospital <- data %>%
    ggplot( aes(x=data$date_admission_hospital, fill=label)) +
    geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity') +
    scale_fill_manual(values=c("#69b3a2", "#404080", "503060")) +
    theme_ipsum() +
    labs(fill="")
p_date_admission_hospital

#5.4.4 - country
table(data$country, data$label)

#5.5 - Avalia��o do Baseline

#5.5.1 - Cria��o das previs�es do modelo
train_pred_baseline <- predict(treeModel_baseline, dataTrain, type = "class")


#5.5.1 - Matriz de confus�o para o dataset de treinamento
cm_baseline <- confusionMatrix(data = as.factor(train_pred_baseline), 
                               reference = as.factor(dataTrain$label), 
                               positive='forgery')
cm_baseline

#5.5.2 - Matriz de confus�o relativa para o dataset de treinamento
cm_baseline_relative <- calculaMatrizConfusaoRelativa(cm_baseline)
cm_baseline_relative

#5.5.3 - Acur�cia Balanceada para o dataset de treinamento
acc_baseline_balanceada <-calcula_acc_balanceada(cm_baseline_relative)
acc_baseline_balanceada

#5.5.1 - Cria��o das previs�es do modelo
train_pred_baseline <- predict(treeModel_baseline, dataVal, type = "class")

#5.5.1 - Matriz de confus�o para o dataset de validação
cm_baseline <- confusionMatrix(data = as.factor(val_pred_baseline), 
                               reference = as.factor(dataVal$label), 
                               positive='forgery')
cm_baseline

#5.5.2 - Matriz de confus�o relativa para o dataset de validação
cm_baseline_relative <- calculaMatrizConfusaoRelativa(cm_baseline)
cm_baseline_relative

#5.5.3 - Acur�cia Balanceada para o dataset de validação
acc_baseline_balanceada <-calcula_acc_balanceada(cm_baseline_relative)
acc_baseline_balanceada



#------------------------------------------------------------------
# 6- Balanceamento
#------------------------------------------------------------------

#6.1 - Balanceamento por pesos 

#6.1.1 - Criação dos pesos

label_frequency = table(dataTrain$label)
label_frequency 
relative_label_frequency =label_frequency/sum(label_frequency)
relative_label_frequency
#6.1.2 - Criação do vetor dos pesos
weights <- rep(0.0, dim(dataTrain)[1])

weight_dead = sum(label_frequency)/(3*label_frequency[1])
weight_onTreatment = sum(label_frequency)/(3*label_frequency[2])
weight_recovered = sum(label_frequency)/(3*label_frequency[3])

weights[dataTrain$label == 'dead'] = weight_dead
weights[dataTrain$label == 'onTreatment'] = weight_onTreatment
weights[dataTrain$label == 'recovered'] = weight_recovered

#6.1.2 - modelo "baseline" com pesos

treeModel_w_baseline <- rpart(formula=label ~ ., 
                              data=dataTrain,
                              method="class",
                              weights = weights,
                              control=rpart.control(minsplit=2,
                                                    cp=0.0,
                                                    xval = 0),
                              parms= list(split="information"))

#6.1.3 - Avaliacao dataset Treino
evaluate_model_ret_acc_relative(treeModel_w_baseline, dataTrain)

#6.1.4 - Avaliacao dataset Validacao
evaluate_model_ret_acc_relative(treeModel_w_baseline, dataVal)

#6.2 - Balanceamento por Undersampling

#6.2.1  - Verificando a menor quantidade dentre as labels
table(dataTrain$label)

dataTrain_dead <- dataTrain[dataTrain$label == 'dead',]
dataTrain_onTreatment <- dataTrain[dataTrain$label == 'onTreatment',]
dataTrain_recovered <-dataTrain[dataTrain$label == 'recovered',]

lowest_samples <- min (nrow(dataTrain_dead), nrow(dataTrain_onTreatment), nrow(dataTrain_recovered) )
lowest_samples
#por praticidade de ajuste
nsamples <- lowest_samples

#6.2.2 - Selecinoando os indices
Idx_dead <- sample(1:nrow(dataTrain_dead), nsamples, replace = FALSE)
Idx_onTreatment <- sample(1:nrow(dataTrain_onTreatment), nsamples, replace = FALSE)
Idx_recovered <- sample(1:nrow(dataTrain_recovered), nsamples, replace = FALSE)

#6.2.3 - Criando o SubDataset
subsetDataTrain <- rbind (dataTrain_dead[Idx_dead,],
                          dataTrain_onTreatment[Idx_onTreatment,],
                          dataTrain_recovered[Idx_recovered,])
table(subsetDataTrain$label)

#6.1.4 - modelo "baseline" pelo subset

treeModel_undersampling_baseline <- rpart(formula=label ~ ., 
                                          data=subsetDataTrain,
                                          method="class",
                                          control=rpart.control(minsplit=2,
                                                                cp=0.0,
                                                                xval = 0),
                                          parms= list(split="information"))

#6.1.5 - Avaliacao dataset de treino
evaluate_model_ret_acc_relative(treeModel_undersampling_baseline, dataTrain)

#6.1.5 - Avaliacao dataset de validacao
evaluate_model_ret_acc_relative(treeModel_undersampling_baseline, dataVal)

#------------------------------------------------------------------
# 7- Varia��o do tamanho das �rvores
#------------------------------------------------------------------

#Cria�ao do Data.Frame para salvar os valores de acuracia
accPerDepth <- data.frame(depth=numeric(15), accTrain=numeric(15), accVal=numeric(15))
summary(accPerDepth)

#for para treinar �rvores com tamanhos vari�dos (s� depth nesse caso)
#utilizando base c/ undersampling
for (maxDepth in 1:30){
    treeModel <- rpart(formula=label ~ .,
                       data=subsetDataTrain,
                       method="class",
                       control=rpart.control(minsplit=2,
                                             cp=0.0, 
                                             maxdepth=maxDepth,
                                             xval = 0),
                       parms= list(split="information"))
    
    # Avaliando no conjunto de treinamento
    train_pred <- predict(treeModel, subsetDataTrain, type="class")
    
    cm_train <- confusionMatrix(data = as.factor(train_pred), 
                                reference = as.factor(subsetDataTrain$label), 
                                positive='forgery')
    
    cm_train_relative <- calculaMatrizConfusaoRelativa(cm_train)
    
    acc_bal_train <- calcula_acc_balanceada(cm_train_relative)
    
    # Avaliando no conjunto de validacao
    val_pred <- predict(treeModel, dataVal, type="class")
    
    cm_val <- confusionMatrix(data = as.factor(val_pred), 
                              reference = as.factor(dataVal$label), 
                              positive='forgery')
    
    cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)
    
    acc_bal_val <- calcula_acc_balanceada(cm_val_relative)
    
    accPerDepth[maxDepth,] = c(maxDepth, 
                               acc_bal_train, 
                               acc_bal_val)
}

accPerDepth <- melt(accPerDepth, id="depth")  # convert to long format
ggplot(data=accPerDepth, aes(x=depth, y=value, colour=variable)) + geom_line() + geom_point()

# Nesse caso esta entre 6 e 7
# Treinando modelo para profundidade igual a 7 de acordo com melhor valor de acc balanceada de validacao
# Avalaicao dos pontos plotados acima
accPerDepth

treeModel_max_depth <- rpart(formula=label ~ .,
                    data=subsetDataTrain,
                    method="class",
                    control=rpart.control(minsplit=2,
                                            cp=0.0, 
                                            maxdepth=7,
                                            xval = 0),
                    parms= list(split="information"))

#------------------------------------------------------------------
# 8- Explora��o do Subconjunto de features
#------------------------------------------------------------------
relative_importance
#8.1 - Subset 1
subset_1 = subsetDataTrain[, c('date_death_or_discharge',
                               'age',
                               'sex',
                               'travel_history_dates',
                               'country',
                               'date_admission_hospital',
                               'label')]
treeModel_subset_1 <- rpart(formula=label ~ .,
                            data=subset_1,
                            method="class",
                            control=rpart.control(minsplit=2,
                                                  cp=0.0, 
                                                  maxdepth=7,
                                                  xval = 0),
                            parms= list(split="information"))


evaluate_model_ret_acc_relative(treeModel_subset_1, dataVal)
ret_feature_importances(treeModel_subset_1)

#8.2 - Subset 2
subset_2 = subsetDataTrain[, c('date_death_or_discharge',
                               'age',
                               'longitude',
                               'latitude',
                               'date_admission_hospital'
                               ,'label')]


treeModel_subset_2 <- rpart(formula=label ~ .,
                            data=subset_2,
                            method="class",
                            control=rpart.control(minsplit=2,
                                                  cp=0.0, 
                                                  maxdepth=7,
                                                  xval = 0),
                            parms= list(split="information"))

evaluate_model_ret_acc_relative(treeModel_subset_2, dataVal)

ret_feature_importances(treeModel_subset_2)

#8.2 - Subset 3

subset_3 = subsetDataTrain[, c('age',
                               'travel_history_dates',
                               'label')] 

treeModel_subset_3 <- rpart(formula=label ~ .,
                     data=subset_3,
                     method="class",
                     control=rpart.control(minsplit=2,
                                           cp=0.0, 
                                           maxdepth=7,
                                           xval = 0),
                     parms= list(split="information"))

evaluate_model_ret_acc_relative(treeModel_subset_3, dataVal)

ret_feature_importances(treeModel_subset_3)

#------------------------------------------------------------------
# 9- Rodando conjunto teste
#------------------------------------------------------------------

# Leitura da base de Teste. Descomentem as linhas abaixo quando o 
# conjunto de teste estiver dispon�vel.

test_set <- read.csv("test_set_patient_status_covid19.csv", stringsAsFactors = T) # Descomentar

# As duas linhas abaixo s�o um trick para corrigir os "levels" na
# coluna country. Ele apenas adiciona 1 exemplo de treino na primeira
# linha do teste e depois retira-o para obter o test_set original. 
# Nao se preocupem, eh apenas para nivelamento interno do R. 
# Certifiquem-se de executar os comandos na seguinte ordem:
# linha 476, linha 485 e linha 486 quando a base de teste estiver disponivel

temporary_test <- rbind(data[1,], test_set)
test_set <- temporary_test[-1,]
dim(test_set)
# 4.3 - Verificando propor��o dos labels
#train
table(test_set$label)
table(test_set$label)/length(test_set$label)

# Teste Baseline (Sem Balanceamento)
evaluate_model_ret_acc_relative(treeModel_baseline, test_set)

# Teste Baseline (Com Balanceamneto por Pesos)
evaluate_model_ret_acc_relative(treeModel_w_baseline, test_set)

# Teste Baseline (Com Undersampling)
evaluate_model_ret_acc_relative(treeModel_undersampling_baseline, test_set)

# Teste variação da profundidade máxima da árvore
evaluate_model_ret_acc_relative(treeModel_max_depth, test_set)

#Teste subconjunto de variáveis
evaluate_model_ret_acc_relative(treeModel_subset_1, test_set)


#------------------------------------------------------------------
# 10 - Florestas aleatórias
#------------------------------------------------------------------

#10.1 - teste com todas as features
nTreeList = c(1, 5, 10, 25, 50, 100, 250, 500, 1000)
accPerNTrees <- data.frame(ntree=numeric(length(nTreeList)), 
                           accTrain=numeric(length(nTreeList)), 
                           accVal=numeric(length(nTreeList)))

for (i in 1:length(nTreeList)){
    
    set.seed(42)
    
    rfModel <- randomForest(formula=label ~ ., 
                            data= subsetDataTrain,
                            ntree=nTreeList[i],
                            mtry=3)
    
    # Avaliando no conjunto de treinamento
    train_pred <- predict(rfModel, subsetDataTrain, type="class")
    
    cm_train <- confusionMatrix(data = as.factor(train_pred), 
                                reference = as.factor(subsetDataTrain$label), 
                                positive='forgery')
    
    cm_train_relative <- calculaMatrizConfusaoRelativa(cm_train)
    acc_bal_train <- calcula_acc_balanceada(cm_train_relative)
    
    # Avaliando no conjunto de validacao
    val_pred <- predict(rfModel, dataVal, type="class")
    cm_val <- confusionMatrix(data = as.factor(val_pred), 
                              reference = as.factor(dataVal$label), 
                              positive='forgery')
    
    cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)
    acc_bal_val <- calcula_acc_balanceada(cm_val_relative)
    
    accPerNTrees[i,] = c(nTreeList[i], 
                         acc_bal_train, 
                         acc_bal_val)
}


accPerNTrees <- melt(accPerNTrees, id="ntree")  # convert to long format
ggplot(data=accPerNTrees, aes(x=ntree, y=value, colour=variable)) + geom_line() + geom_point()


#10.1 teste com features n2

nTreeList = c(1, 5, 10, 25, 50, 100, 250, 500, 1000)
accPerNTrees <- data.frame(ntree=numeric(length(nTreeList)), 
                           accTrain=numeric(length(nTreeList)), 
                           accVal=numeric(length(nTreeList)))


for (i in 1:length(nTreeList)){

    set.seed(42)

    rfModel <- randomForest(formula=label ~ date_death_or_discharge+ age+
                               longitude+ latitude+ date_admission_hospital, 
                            data= subsetDataTrain,
                            ntree=nTreeList[i],
                            mtry=3)
    
    # Avaliando no conjunto de treinamento
    train_pred <- predict(rfModel, subsetDataTrain, type="class")
    
    cm_train <- confusionMatrix(data = as.factor(train_pred), 
                                reference = as.factor(subsetDataTrain$label), 
                                positive='forgery')
    
    cm_train_relative <- calculaMatrizConfusaoRelativa(cm_train)
    acc_bal_train <- calcula_acc_balanceada(cm_train_relative)
    
    # Avaliando no conjunto de validacao
    val_pred <- predict(rfModel, dataVal, type="class")
    cm_val <- confusionMatrix(data = as.factor(val_pred), 
                              reference = as.factor(dataVal$label), 
                              positive='forgery')
    
    cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)
    acc_bal_val <- calcula_acc_balanceada(cm_val_relative)
    
    accPerNTrees[i,] = c(nTreeList[i], 
                         acc_bal_train, 
                         acc_bal_val)
}

accPerNTrees <- melt(accPerNTrees, id="ntree")  # convert to long format
ggplot(data=accPerNTrees, aes(x=ntree, y=value, colour=variable)) + geom_line() + geom_point()


#10.2 Melhorando intervalo de busca

nTreeList = c(1:100)
accPerNTrees <- data.frame(ntree=numeric(length(nTreeList)), 
                           accTrain=numeric(length(nTreeList)), 
                           accVal=numeric(length(nTreeList)))


for (i in 1:length(nTreeList)){

    set.seed(42)

    rfModel <- randomForest(formula=label ~ ., 
                            data= subsetDataTrain,
                            ntree=nTreeList[i],
                            mtry=sqrt(length(colnames(subsetDataTrain))))
    
    # Avaliando no conjunto de treinamento
    train_pred <- predict(rfModel, subsetDataTrain, type="class")
    
    cm_train <- confusionMatrix(data = as.factor(train_pred), 
                                reference = as.factor(subsetDataTrain$label), 
                                positive='forgery')
    
    cm_train_relative <- calculaMatrizConfusaoRelativa(cm_train)
    acc_bal_train <- calcula_acc_balanceada(cm_train_relative)
    
    # Avaliando no conjunto de validacao
    val_pred <- predict(rfModel, dataVal, type="class")
    cm_val <- confusionMatrix(data = as.factor(val_pred), 
                              reference = as.factor(dataVal$label), 
                              positive='forgery')
    
    cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)
    acc_bal_val <- calcula_acc_balanceada(cm_val_relative)
    
    accPerNTrees[i,] = c(nTreeList[i], 
                         acc_bal_train, 
                         acc_bal_val)
}

accPerNTrees <- melt(accPerNTrees, id="ntree")  # convert to long format
ggplot(data=accPerNTrees, aes(x=ntree, y=value, colour=variable)) + geom_line() + geom_point()

#ENCONTRAR MAIS FACIL E TREINAR COM MELHOR MODELO <---- VALIDAR***

max_acc_validation = max(accPerNTrees[accPerNTrees$variable == 'accVal',]['value'])

min_n_tree = min(accPerNTrees[(accPerNTrees['value'] == max_acc_validation) &
             (accPerNTrees['variable'] == 'accVal') ,'ntree'])

#acho que tem alguma pergunta que toca nesse numero minimo
print(min_n_tree)

#10.3 Treinando floresta com melhor numero de arvores
set.seed(42)

treeForest_n3 <- randomForest(formula=label ~ ., 
                            data= subsetDataTrain,
                            ntree=3,
                            mtry=sqrt(length(colnames(subsetDataTrain))))



#Validando
evaluate_model_ret_acc_relative(treeForest_n3, dataVal)
#Rodando no conjunto teste
evaluate_model_ret_acc_relative(treeForest_n3, test_set)


#------------------------------------------------------------------
# 11- EXTRA
#------------------------------------------------------------------

#11.1 Cria matriz com 0 do tamanho do DataVal

getRandomForestResults <- function(ntree, m, trainSet, valSet){
    
  
  # Seleciona os exemplos das classes positivas e negativas
  #dataNeg <- trainSet[trainSet$y == "no",]
  #dataPos <- trainSet[trainSet$y == "yes",] 
    # Seleciona os exemplos das classes
    dataTrain_dead <- dataTrain[dataTrain$label == 'dead',]
    dataTrain_onTreatment <- dataTrain[dataTrain$label == 'onTreatment',]
    dataTrain_recovered <-dataTrain[dataTrain$label == 'recovered',]

  #lowest_samples <- min(dim(dataNeg)[1], dim(dataPos)[1])
    #seleciona o menor dos valores
    lowest_samples <- min (nrow(dataTrain_dead), nrow(dataTrain_onTreatment), nrow(dataTrain_recovered) )
  
    print("Numero de elementos na classe 'dead':")
    print(dim(dataTrain_dead))
    print("Numero de elementos na classe 'onTreatment':")
    print(dim(dataTrain_onTreatment))
    print("Numero de elementos na classe 'recovered':")
    print(dim(dataTrain_recovered))
    print("Menor desse valores:")
    
    print(lowest_samples)
  
  # Matriz de tamano N x M inicializada com zeros. Em que N Ã© o nÃºmero
  # de exemplos no conjunto de validaÃ§Ã£o e M Ã© o nÃºmero de Ã¡rvores que
  # teremos no Ensemble. Cada coluna terÃ¡ os valores preditos por  
  # cada Ã¡rvore no Ensemble. 
    valPredictedClasses <- matrix(0, nrow = nrow(valSet), ncol = ntree)
    trainPredictedClasses <- matrix(0, nrow = nrow(trainSet), ncol = ntree)
  
  
    for(i in 1:ntree){
    
        nsamples <- round(runif(1, min=0.85, max=1.0)*lowest_samples)
    
    # Seleciona, com reposiÃ§Ã£o (ja que o Bagging faz parte da Random Forest), 
    # os Ã­ndices da classe negativa
    #NoIdx <- sample(1:nrow(dataNeg), nsamples, replace = TRUE)
    #YesIdx <- sample(1:nrow(dataPos), nsamples, replace = TRUE)
        Idx_dead <- sample(1:nrow(dataTrain_dead), nsamples, replace = TRUE)
        Idx_onTreatment <- sample(1:nrow(dataTrain_onTreatment), nsamples, replace = TRUE)
        Idx_recovered <- sample(1:nrow(dataTrain_recovered), nsamples, replace = TRUE)
    
    # Selecionamos aleatoriamente um subconjunto das features
    # originais (desconsiderando o target). JÃ¡ que, cada arvore 
    # na random forest, eh treinada com um subconjunto dos dados
    # tomados com reposicao (duas linha de comando a cima) e um 
    # subconjunto das features.
        featuresIdx <- sample(1:(ncol(trainSet)-1), m, replace = FALSE)
    #aqui se diferencia dos anteriores <<<<<<<<<<<<<
    # Como desconsideramos o target anteriormente,
    # temos que adiciona-lo de volta para o modelo treinar
        featuresIdx <- c(featuresIdx, ncol(trainSet)) 
    
    # Cria-se o conjunto de treino baseado na selecao de exemplos
    # e features das linhas anteriores
        subsetDataTrain <- rbind (dataTrain_dead[Idx_dead,],
                                  dataTrain_onTreatment[Idx_onTreatment,],
                                  dataTrain_recovered[Idx_recovered,])
    
        treeModel <- rpart(formula=label ~ ., 
                           data=subsetDataTrain, method="class",
                           control=rpart.control(minsplit=2, cp=0.0, xval = 0),
                           parms= list(split="information"))
        
        #treino
        trainPreds <- predict(treeModel, trainSet, type = "class")
        trainPredictedClasses[,i] <- trainPreds    
        #validaÃ§Ã£o
        valPreds <- predict(treeModel, valSet, type = "class")
        valPredictedClasses[,i] <- valPreds 

    
    }
    
    df <-data.frame(apply(valPredictedClasses,1, FUN = ,getmode))
    colnames(df) <- 'predicted'
    rownames(df) <- rownames(valSet)
    df[df['predicted'] == 1]  <- 'dead'
    df[df['predicted'] == 2]  <- 'onTreatment'
    df[df['predicted'] == 3]  <- 'recovered'
    
    cm <- confusionMatrix(data = as.factor(df$predicted), 
                          reference = as.factor(valSet$label), 
                          positive='yes')
    
    #TREINO
    accTrainRandomForest <- c(ntree-1)
    #VALIDACAO
    accValRandomForest <- c(ntree-1)
    
    for(i in 2:ntree){
        
        #TREINO        
        df_train <-data.frame(apply(trainPredictedClasses[,1:i],1, FUN = ,getmode))
        colnames(df_train) <- 'predicted'
        rownames(df_train) <- rownames(trainSet)
        df_train[df_train['predicted'] == 1]  <- 'dead'
        df_train[df_train['predicted'] == 2]  <- 'onTreatment'
        df_train[df_train['predicted'] == 3]  <- 'recovered'

        cm_train <- confusionMatrix(data = as.factor(df_train$predicted), 
                              reference = as.factor(trainSet$label), 
                              positive='yes')
        
        cm_train_relative <- calculaMatrizConfusaoRelativa(cm_train)    
        acc_bal_train <- calcula_acc_balanceada(cm_train_relative)       
        accTrainRandomForest[i-1] <- acc_bal_train        
        
        
        
        #VALIDACAO        
        df_val <-data.frame(apply(valPredictedClasses[,1:i],1, FUN = ,getmode))
        colnames(df_val) <- 'predicted'
        rownames(df_val) <- rownames(valSet)
        df_val[df_val['predicted'] == 1]  <- 'dead'
        df_val[df_val['predicted'] == 2]  <- 'onTreatment'
        df_val[df_val['predicted'] == 3]  <- 'recovered'

        cm_val <- confusionMatrix(data = as.factor(df_val$predicted), 
                              reference = as.factor(valSet$label), 
                              positive='yes')
        
        cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)    
        acc_bal_val <- calcula_acc_balanceada(cm_val_relative)       
        accValRandomForest[i-1] <- acc_bal_val
        }      
    
    
    
    
    
    
    plot(2:ntree, accValRandomForest, xlab = "Number of classifiers", 
         ylab = "Balanced Acc", col="blue", type="o",        
         ylim=c(min(accValRandomForest, accTrainRandomForest), 
            max(accValRandomForest,accTrainRandomForest)))

        
        

    
    points(accTrainRandomForest, col="red", pch=".")
    lines(accTrainRandomForest, col="red", lty=1)
    
    legend(20, 0.65, legend=c("AccValRandomForest",
                             "accTrainRandomForest"), 
       col=c("blue","red" ), pch=c("__"), cex=0.7, pt.cex = 1)

    return(accValRandomForest)

    
    
}


#Rodando primeiro modelo , escolhendo m como sqrt
m <- sqrt((ncol(dataTrain)))
#valPredictedClasses <- getRandomForestResults(5, m, dataTrain, dataVal)
df <- getRandomForestResults(75, m, dataTrain, dataVal)

# Variação do número de features
m <- ncol(dataTrain)/2 

df_2 <- getRandomForestResults(75, m, dataTrain, dataVal)


#variação do m
m <- (ncol(dataTrain)*3)/4

df_3 <- getRandomForestResults(75, m, dataTrain, dataVal)

