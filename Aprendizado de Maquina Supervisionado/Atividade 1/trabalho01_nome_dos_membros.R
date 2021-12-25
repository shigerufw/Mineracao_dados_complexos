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


# Funcao de Apoio ao Trabalho 01 de Aprendizado Supervisionado I. 
# Esta fun??o escreve a formula dos modelos polinomiais. 
# Parametros:

# real_feature_names: Um vetor com os nomes dos atributos continuos que voce
#                     quer que seja elevado ao grau desejado.
#  
# categorical_feature_names: Um vetor com os nomes dos atributos categoricos
#                            que voce quer que seja adicionado a hipotese. 
#                            Eles n?o s?o elevados ao grau especificado ja que
#                            sao valores binarios (0 ou 1). Se voce quer uma
#                            hipotese que nao tenha nenhum valor categorico, mas
#                            apenas os reais, basta nao passar nenhum valor 
#                            para este parametro quando chamar a funcao.
#
#
# degree: Grau que voc? deseja que os atributos reais em "real_feature_names"
#         sejam elevados. Ao chamar a funcao, escreva explicitamente
#         o grau desejado. Por exemplo, para grau igual 2, escreva degree=2

# Vejam os exerc?cios 02 e 03 para ver o funcionamento 
# de uma funcao similar a essa.
# setwd("/Users/nkuros/Documents/mineiracao_dados_complexos/Aprendizado de Maquina Supervisionado/Atividade 1/")


rm(list=ls())
graphics.off()

getHypothesis <- function(real_feature_names, categorical_feature_names=F, degree=3){
    
    hypothesis_string <- "hypothesis <- formula(target ~ "
    for(d in 1:degree){
        for(i in 1:length(real_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       "I(", real_feature_names[i], "^", d, ") + ",
                                       sep = "")
        }
    }
    
    if(typeof(categorical_feature_names) != "logical"){
        for(i in 1:length(categorical_feature_names)){
            hypothesis_string <- paste(hypothesis_string, 
                                       categorical_feature_names[i], " + ",
                                       sep = "")
        } 
    }
    
    
    hypothesis_string <- substr(hypothesis_string, 1, nchar(hypothesis_string)-3)
    hypothesis_string <- paste(hypothesis_string, ")")
    hypothesis <- eval(parse(text=hypothesis_string))
    return(hypothesis)
}

# Comandos que leem os conjuntos de treino e de validacao

train_set <- read.csv("./training_set_air_quality.csv", stringsAsFactors=TRUE)
val_set <- read.csv("./validation_set_air_quality.csv", stringsAsFactors=TRUE)
test_set <- read.csv("./test_set_air_quality.csv", stringsAsFactors=TRUE)

# Desenvolvam o trabalho a partir daqui, apos executarem os comandos a cima

# install.packages(ggplot2)
# install.packages(dplyr)
# install.packages(reshape2)

library(ggplot2)
library(dplyr)
library(reshape2)

#### Tarefas

# 1.
########
# Validacao se os conjuntos sao disjuntos
## Ok, todos os conjuntos sao disjuntos
merge(train_set, val_set)
merge(train_set, test_set)
merge(test_set, val_set)

# Validacao de volumetria das bases
summary(train_set)
colnames(train_set)
colnames(train_set)

summary(val_set)
dim(val_set)
colnames(val_set)

summary(test_set)
dim(test_set)
colnames(test_set)

#########
# Verificacao da categoria marcada como (OTHER) na coluna wd
tapply(train_set$No, train_set$wd, length)

#########
# Visualizacao da distribuição das variaveis
melt_train_set <- train_set[, -which(names(train_set) %in% c('No', 'wd', 'year', 'month', 'day'))]
melt_train_set <- melt(melt_train_set)

p <- ggplot(data=melt_train_set, aes(x=value))+
    stat_density()+
    facet_wrap(~variable, scales='free'); p


########
# Visualizacao dos boxplots para retirada de outliers
p <- ggplot(data = melt_train_set[melt_train_set$variable!='target', ], aes(x=variable, y=value)) + 
    geom_boxplot()
p + facet_wrap( ~ variable, scales="free_y"); p


#########
# Problema com a variavel RAIN, quase todos os registros tem valor 0
ggplot(train_set, aes(x=RAIN)) + 
    geom_histogram(color='White', bins=10) +
    stat_bin(aes(label=..count..))

tapply(train_set$No, train_set$RAIN, length)

########
# Funcao para criar Onehot encoding para a variavel de direcao do vento

onehot_features <- function(dataset, feature){
    cats <- unique(dataset[, feature])
    for (cat in cats){
        dataset[cat] <- as.numeric(dataset[,feature]==cat)
    }
    return(dataset)
}
train_set <- onehot_features(train_set, 'wd')
val_set <- onehot_features(val_set, 'wd')
test_set <- onehot_features(test_set, 'wd')

#########
# Retirando No e a última variável de vento  para gerar nosso caso BASELINE
train_set_clean <- train_set[, -which(names(train_set) %in% c('No', 'wd', 'N'))]
val_set_clean <- val_set[, -which(names(val_set) %in% c('No', 'wd', 'N'))]
test_set_clean <- test_set[, -which(names(test_set) %in% c('No', 'wd', 'N'))]

setdiff(colnames(train_set_clean), colnames(val_set_clean))
setdiff(colnames(train_set_clean), colnames(test_set_clean))
setdiff(colnames(val_set_clean), colnames(test_set_clean))

summary(train_set_clean)

#########
# Devido a distribuicao nao normal dos dados iremos aplicar uma normalizacao min-max
# exceto para as variaveis de data e a variavel target
min_features <- apply(train_set_clean[,5:14], 2, min); min_features

max_features <- apply(train_set_clean[,5:14], 2, max); max_features

diff <- max_features - min_features; diff

train_set_clean[,5:14] <- sweep(train_set_clean[,5:14], 2, min_features, "-")
train_set_clean[,5:14] <- sweep(train_set_clean[,5:14], 2, diff, "/")
summary(train_set_clean)

val_set_clean[,5:14] <- sweep(val_set_clean[,5:14], 2, min_features, "-")
val_set_clean[,5:14] <- sweep(val_set_clean[,5:14], 2, diff, "/")

test_set_clean[,5:14] <- sweep(test_set_clean[,5:14], 2, min_features, "-")
test_set_clean[,5:14] <- sweep(test_set_clean[,5:14], 2, diff, "/")


########
# Visualizacao dos dados apos normalizacao min max (Visualmente nao alterou muito a distribuicao)
melt_train_set <- train_set_clean[, -which(names(train_set_clean) %in% c('No', 'wd', 'year', 'month', 'day', 'hour'))]
melt_train_set <- melt(melt_train_set)

ggplot(data=melt_train_set, aes(x=value))+
    stat_density()+
    facet_wrap(~variable, scales='free')


########
# Criacao do modelo Baseline

########
# Para o onehot encoding, utilizar n-1 features de flags
wd_columns <- c("NE","SE","SSE","SSW","NNE","SW","S","WNW","ESE","NNW","NW","W","E","ENE");wd_columns

not_include <- c(wd_columns, 'target');not_include

feature_names <- colnames(train_set_clean[, -which(names(train_set_clean) %in% not_include)]);feature_names


hypothesis <- getHypothesis(feature_names, categorical_feature_names=wd_columns,degree=1);hypothesis

## Baseline ##
baseline <- lm(formula=hypothesis, data=train_set_clean)

valPred <- predict(baseline, val_set_clean)
trainPred <- predict(baseline, train_set_clean)

MAE <- function(preds, labels){
    mae_values <- sum(abs(preds-labels))/length(preds)
    return(mae_values)
}

MSE <- function(preds, labels){
    mse_values <- sum((preds-labels)**2)/length(preds)
    return(mse_values)
}

R2 <- function(pred, true){
    rss <- sum((pred - true) ^ 2)
    tss <- sum((true - mean(true)) ^ 2)
    r2 <- 1 - rss/tss
    return(r2)
}

mae_train_baseline <- MAE(trainPred, train_set_clean$target); mae_train_baseline
mse_train_baseline <- MSE(trainPred, train_set_clean$target); mse_train_baseline
r2_train_baseline <- R2(trainPred, train_set_clean$target);r2_train_baseline


mae_val_baseline <- MAE(valPred, val_set_clean$target);mae_val_baseline
mse_val_baseline <- MSE(valPred, val_set_clean$target);mse_val_baseline
r2_val_baseline <- R2(valPred, val_set_clean$target);r2_val_baseline


# Retirando RAIN

train_set_clean <- train_set_clean[, -which(names(train_set_clean) %in% c('RAIN'))];colnames(train_set_clean)
val_set_clean <- val_set_clean[, -which(names(val_set_clean) %in% c('RAIN'))]
test_set_clean <- test_set_clean[, -which(names(test_set_clean) %in% c('RAIN'))]
feature_names <- colnames(train_set_clean[, -which(names(train_set_clean) %in% not_include)]);feature_names

########
# Criacao de modelos atraves da combinacao de features
h02 <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + 
                   I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + 
                   I(DEWP^1) + I(WSPM^1) + I(WSW^1) + NE + SE + SSE + SSW + 
                   NNE + SW + S + WNW + ESE + NNW + NW + W + E + ENE +
                   (
                       I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + 
                       I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(WSPM^1) + I(WSW^1)
                   )^2
               )


h03 <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + 
                   I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + 
                   I(DEWP^1) + I(WSPM^1) + I(WSW^1) + NE + SE + SSE + SSW + 
                   NNE + SW + S + WNW + ESE + NNW + NW + W + E + ENE +
                   (
                       I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + 
                           I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(WSPM^1) + I(WSW^1)
                   )^3
)

h04 <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + 
                   I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + 
                   I(DEWP^1) + I(WSPM^1) + I(WSW^1) + NE + SE + SSE + SSW + 
                   NNE + SW + S + WNW + ESE + NNW + NW + W + E + ENE +
                   (
                       I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + 
                           I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(WSPM^1) + I(WSW^1)
                   )^5
)

h05 <- formula(target ~ I(year^1) + I(month^1) + I(day^1) + I(hour^1) + I(PM2.5^1) + 
                   I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + I(TEMP^1) + I(PRES^1) + 
                   I(DEWP^1) + I(WSPM^1) + I(WSW^1) + NE + SE + SSE + SSW + 
                   NNE + SW + S + WNW + ESE + NNW + NW + W + E + ENE +
                   (
                       I(PM2.5^1) + I(PM10^1) + I(SO2^1) + I(NO2^1) + I(O3^1) + 
                           I(TEMP^1) + I(PRES^1) + I(DEWP^1) + I(WSPM^1) + I(WSW^1)
                   )^10
)

h06 <- getHypothesis(feature_names, categorical_feature_names=wd_columns,degree=2)
h07 <- getHypothesis(feature_names, categorical_feature_names=wd_columns,degree=3)
h08 <- getHypothesis(feature_names, categorical_feature_names=wd_columns,degree=5)
h09 <- getHypothesis(feature_names, categorical_feature_names=wd_columns,degree=10)


modelsCategorical <- c(h02, h03, h04, h05,h06, h07, h08, h09)
total_mae_train_noCat <- c(length(modelsCategorical))
total_mae_val_noCat <- c(length(modelsCategorical))

total_mse_train_noCat <- c(length(modelsCategorical))
total_mse_val_noCat <- c(length(modelsCategorical))

total_r2_train_noCat <- c(length(modelsCategorical))
total_r2_val_noCat <- c(length(modelsCategorical))


i <- 1
dataTrain <- train_set_clean
dataVal <- val_set_clean


for(f in modelsCategorical){
    
    model <- lm(formula=f, data=dataTrain)
    
    valPred <- predict(model, dataVal)
    trainPred <- predict(model, dataTrain)
    
    mae_train <- MAE(trainPred, dataTrain$target)
    total_mae_train_noCat[i] <- mae_train
    
    mae_val <- MAE(valPred, dataVal$target)
    total_mae_val_noCat[i] <- mae_val

    mse_train <- MSE(trainPred, dataTrain$target)
    total_mse_train_noCat[i] <- mse_train
    
    mse_val <- MSE(valPred, dataVal$target)
    total_mse_val_noCat[i] <- mse_val
    
    r2_train <- R2(trainPred, dataTrain$target)
    total_r2_train_noCat[i] <- r2_train
    
    r2_val <- R2(valPred, dataVal$target)
    total_r2_val_noCat[i] <- r2_val

    
    i <- i + 1
    
}





mae_train_comb <- total_mae_train_noCat[1:4];mae_train_comb
mae_val_comb <- total_mae_val_noCat[1:4];mae_val_comb

mse_train_comb <- total_mse_train_noCat[1:4];mse_train_comb
mse_val_comb <- total_mse_val_noCat[1:4];mse_val_comb

r2_train_comb <- total_r2_train_noCat[1:4];r2_train_comb
r2_val_comb <- total_r2_val_noCat[1:4];r2_val_comb

#
mae_train_poly <- total_mae_train_noCat[5:8];mae_train_poly
mae_val_poly <- total_mae_val_noCat[5:8];mae_val_poly

mse_train_poly <- total_mse_train_noCat[5:8];mse_train_poly
mse_val_poly <- total_mse_val_noCat[5:8];mse_val_poly

r2_train_poly <- total_r2_train_noCat[5:8];r2_train_poly
r2_val_poly <- total_r2_val_noCat[5:8];r2_val_poly


models_comb <- c('h02', 'h03', 'h04', 'h05');models_comb
models_poly <- c('h06', 'h07', 'h08', 'h09');models_poly


########
# Plotando curvas de erro x complexidade MAE
#jpeg("mae.jpeg", quality = 75)
plot(mae_train_comb, xlab="Degree", ylab="Error", 
     ylim=c(280, 400), pch="+", col="orange",  xaxt="n")
points(mae_val_comb, pch="+", col="black")
lines(mae_train_comb, col="orange", lty=1)
lines(mae_val_comb, col="black", lty=1)


points(mae_train_poly, pch="*", col="blue")
points(mae_val_poly, pch="*", col="red")
lines(mae_train_poly, col="blue", lty=2)
lines(mae_val_poly, col="red", lty=2)

points(rep(mae_val_baseline, 4), pch="o", col="green")
lines(rep(mae_val_baseline, 4), col="green", lty=1)

axis(1, at=1:4, labels=c(2, 3, 5, 10), las=1)

legend(400, y=NULL,
       legend=c("Feature Combination Train", "Feature Combination Valid","Polynomials Train", "Polynomials Test","Baseline"), 
       col=c("orange","black","red","blue", "green"), lty=1, cex=0.8)
#dev.off()

####

# Plotando curvas de erro x complexidade MSE
#jpeg("mse.jpeg", quality = 75)
plot(mse_train_comb, xlab="Degree", ylab="Error", 
     ylim=c(200000, 400000),pch="+", col="orange",  xaxt="n")
points(mse_val_comb, pch="+", col="black")
lines(mse_train_comb, col="orange", lty=1)
lines(mse_val_comb, col="black", lty=1)
mse_val_comb

points(mse_train_poly, pch="*", col="blue")
points(mse_val_poly, pch="*", col="red")
lines(mse_train_poly, col="blue", lty=2)
lines(mse_val_poly, col="red", lty=2)

points(rep(mse_val_baseline, 4), pch="o", col="green")
lines(rep(mse_val_baseline, 4), col="green", lty=1)

axis(1, at=1:4, labels=c(2, 3, 5, 10), las=1)

legend(220000, y=NULL,
       legend=c("Feature Combination Train", "Feature Combination Valid","Polynomials Train", "Polynomials Test","Baseline"), 
       col=c("orange","black","red","blue", "green"), lty=1, cex=0.8)

#dev.off()

####

# Plotando curvas de erro x complexidade R2
#jpeg("r2.jpeg", quality = 75)
plot(r2_train_comb, xlab="Degree", ylab="R2", 
     ylim=c(.6, 1), pch="+", col="orange",  xaxt="n")
points(r2_val_comb, pch="+", col="black")
lines(r2_train_comb, col="orange", lty=1)
lines(r2_val_comb, col="black", lty=1)


points(r2_train_poly, pch="*", col="blue")
points(r2_val_poly, pch="*", col="red")
lines(r2_train_poly, col="blue", lty=2)
lines(r2_val_poly, col="red", lty=2)

points(rep(r2_val_baseline, 4), pch="o", col="green")
lines(rep(r2_val_baseline, 4), col="green", lty=1)

axis(1, at=1:4, labels=c(2, 3, 5, 10), las=1)

legend(0.7, y=NULL,
       legend=c("Feature Combination Train", "Feature Combination Valid","Polynomials Train", "Polynomials Test","Baseline"), 
       col=c("orange","black","red","blue", "green"), lty=1, cex=0.8)
#dev.off()


######## A PARTIR DAQUI CALCULOS DE ERRO DO TEST SET ##########
model <- lm(formula=h04, data=dataTest)

testPred <- predict(model, test_set_clean)
mae_test <- MAE(testPred, test_set_clean$target);mae_test

mse_test <- MSE(testPred, test_set_clean$target);mse_test

r2_test <- R2(testPred, test_set_clean$target);r2_test

