#############################################
#  MDC010 - Aprendizado Supervisionado I    #
#  Exercise 07 - Banknote Authentication    #
# �rvores de Decis�o e Random Forest        #  
############################################# 

# Descomente as seguintes linhas e execute-as para instalar
# as bibliotceas. Se j� estiverem instaladas, voc� n�o precisa
# executar estes comandos novamente.
#install.packages("caret", dependencies = TRUE)
#install.packages("reshape2", dependencies = TRUE)
#install.packages("ggplot2")
#install.packages("rpart", dependencies = TRUE)
#install.packages("randomForest", dependencies = TRUE)

library(caret)
library(reshape2)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)


set.seed(12)

# Calcula a matriz de confus�o relativa 
calculaMatrizConfusaoRelativa <- function(cm){
    
    # Aplicamos a transposi��o para garantir que a referencia
    # fique nas linhas e a predicao nas colunas
    cm_absolute = t(cm$table)
    
    # SEMPRE construam e reportem a matriz de confusao relativa!
    cm_relative = cm_absolute
    
    cm_relative[1,] = round(cm_absolute[1,]/sum(cm_absolute[1,]), digits=2)
    cm_relative[2,] = round(cm_absolute[2,]/sum(cm_absolute[2,]), digits=2)
    
    return(cm_relative)  
}

trainSet <- read.csv("banknote_authentication_train.csv", stringsAsFactors = T)
valSet <- read.csv("banknote_authentication_validation.csv", stringsAsFactors = T)
testSet <- read.csv("banknote_authentication_test.csv", stringsAsFactors = T)

merge(trainSet, valSet)
merge(trainSet, testSet)
merge(valSet, testSet)


dim(trainSet)
dim(valSet)
dim(testSet)

summary(trainSet)
summary(valSet)
summary(testSet)

table(trainSet$class)
table(valSet$class)
table(testSet$class)

# Documenta��o para �rvore de decis�o.
help(rpart)

# minsplit = n�mero  m�nimo de exemplos em um n� para que ele gere n�s filhos.
# cp = fator que determina o quanto o erro no conjunto de treinamento deve ser
# diminuido para que a gera��o de filhos (split) seja realizada. 
# xval = n�mero de valida��es cruzadas que ser�o realizadas. Ou seja, 
# xval = 10 significa que a divis�o treinamento/valida��o ser� realizado 10
# vezes, e a m�dia e desvio padr�o dos resultados s�o reportados.

# Se quisermos usar como crit�rio a Entropia + Ganho de Informa��o coloque 
# como par�metro o "information".
treeModel <- rpart(formula=class ~ variance + skewness + curtosis + entropy, 
                   data=trainSet, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, xval = 10),
                   parms= list(split="information"))

# Se quisermos usar o Gini como crit�rio de gera��o de filhos.
#treeModel <- rpart(formula=class ~ variance + skewness + curtosis + entropy,
#                    data=trainSet, method="class",
#                    control=rpart.control(minsplit=2, cp=0.0),
#                    parms= list(split="gini"))

# Mostra a tabela com o ganho de performance (CP).
# CP na linha i, � calculado como:
# CP[i] = (rel_error[i] - rel_error[i+1])/(n_split[i+1] - n_split[i])
#
# Por exemplo,vamos calcular para i = 5. Temos que n_split[5] = 5, 
# n_split[6] = 7, rel_error[5] = 0.0848329 e rel_error[6] = 0.0257069. 
# Logo CP[5] = (0.0848329 - 0.0257069)/(7 - 5) = 0.029563
printcp(treeModel)


summary(treeModel)
####    Explica��o de cada parte das especifica��es de um n�       #####

# Respectivo identificador do n� na �rvore e 
# numero de observa��es que alcan�aram o n�
# Node number 2: 469 observations 

# Redu��o do erro relativo em pontos percentuais
# complexity param = 0.1336761

# Classe majorit�ria dos exemplos que alcan�ou este n�.
# predicted class = forgery

# Fra��o da classe minorit�ria dos exemplos que alcan�aram este n�.
# expected loss = 0.2260128

# Probabilidade de alcan�ar este n� da raiz. A conta � baseada
# no n�mero de exemplos enviados para cada filho. Assim se um n� 
# tem N exemplos e "Nd" exemplos s�o enviados para o filho da direita
# e "Ne" exemplos s�o enviados para o filho da esquerda, ent�o as 
# probabilidades de um exemplo descer pelo filho direito ou pelo esquerdo
# s�o respectivamente: Nd/N e Ne/N. A probabilidade abaixo � resultado
# da multiplica��o de cada probabilidade de cada filho desde o n� raiz
# at� o n� atual.

# P(node) = 0.5440835

# Quantidades absolutas de exemplos para cada classe que alcan�aram 
# este n�. Baseado na informa��o "predicted class", sabemos que 363
# s�o da classe "forgery" e 106 da classe "genuine". 
# class counts:    363    106 

#  Mesma informa��o que "class counts", mas em frequ�ncias. 
# probabilities: 0.774 0.226 

# N�mero de exemplos enviados para cada filho. Assim, a probabilidade
# de um exemplo ir para a direita � 100/469 e de ir para esquerda � 
# 369/469.
# "4" e "5" s�o os �ndices dos filhos da esquerda 
# e da direita respectivamente.
# left son=4 (369 obs) right son=5 (100 obs) 

# Ordem dos atributos de acordo com o ganho que cada um fornece 
# para a �rvore de decis�o. O primeiro deles � utilizado pelo 
# modelo para decidir para qual filho direcionar o exemplo 
# que ter� seu target predito. Nesse caso, "skewness", para este n�,
# � o atributo mais importante a ser observado, seguido respectivamente
# por "variance", "entropy" e "curtosis". O "improve" � calculado tomando
# a f�rmula do "Gain(S,A)" visto em aula e multiplicando pelo n�mero de 
# elementos que alcan�aram o n�. Tamb�m utiliza-se o N�mero de Euler como 
# base do logaritmo. Dessa maneira, calcula-se  a entropia para os exemplos
# que apresentam valor de"skewness" abaixo de 5.0956 e para aqueles que apresentam
# o valor de "skewness" a cima de 5.0956. Com os valores de cada uma destas 
# duas entropias, com a entropia total do n�, e com as respectivas
# quantidades, conseguimos calcular o Ganho. Depois podemos multiplic�-lo 
# pelo n�mero de elementos no n�, e assim obter o "improve". 
# O Passo a passo desse c�lculo � mostrado ao final do exerc�cio 
# utilizando o exemplo visto em aula.

#
#Primary splits:
#    skewness < 5.0956     to the left,  improve=91.49793, (0 missing)
#    variance < -2.80905   to the left,  improve=32.79927, (0 missing)
#    entropy  < -3.26915   to the right, improve=19.79886, (0 missing)
#    curtosis < 8.83885    to the right, improve=19.65944, (0 missing)

# Surrogate variables s�o vari�veis que s�o utilizadas em um c�lculo ou
# em crit�rio de decis�o caso uma outra vari�vel necess�ria n�o esteja
# presente. Neste n� em particular deste exerc�cio, 
# o m�todo assume que o atributo "skweness" (o primeiro mais importante) 
# pode n�o estar ausente, assim ele testa outros
# atributos dispon�veis para simular o mesmo comportamento que esse n�
# teria caso a vari�vel original estivesse presente. Neste caso, o
# atributo "entropy" foi escolhido como segunda op��o com threshold
# de decis�o de -4.4918

#Surrogate splits:
#    entropy  < -4.4918    to the right, agree=0.874, adj=0.41, (0 split)
#    variance < -5.14385   to the right, agree=0.825, adj=0.18, (0 split)
###############################################################

#Plot using prp
prp(treeModel)

#Plot using rpart.plot
rpart.plot(treeModel,
           extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)


### Verificando importancia das vari�veis ###
importance_per_features <- treeModel$variable.importance
importance_per_features

relative_importance <- importance_per_features/sum(importance_per_features)
relative_importance

######### Poda p�s treinamento (POST PRUNE) ########

# Mostra a tabela com CP's novamente
printcp(treeModel)

# Poda a �rvore baseado no CP do menor erro no conjunto de valida��o.
minCP <- treeModel$cptable[which.min(treeModel$cptable[,"xerror"]),"CP"]
minCP

ptree <- prune(treeModel, cp=minCP)
summary(ptree)


# Plota a �rvore de decis�o podada
rpart.plot(ptree, 
           extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)


######### Avalia��o ##########
# Vamos ver a performance da �rvore de Decis�o sem a poda
val_pred <- predict(treeModel, valSet, type="class")
cm <- confusionMatrix(data = as.factor(val_pred), 
                      reference = as.factor(valSet$class), 
                      positive='forgery')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal

# Vamos ver agora a performance da �rvore de decis�o ap�s a poda
# com a primeira escolha.
val_pred <- predict(ptree, valSet, type="class")
cm <- confusionMatrix(data = as.factor(val_pred), 
                      reference = as.factor(valSet$class), 
                      positive='forgery')

cm_relative <- calculaMatrizConfusaoRelativa(cm)
cm_relative

acc_bal <- (cm_relative[1,1] + cm_relative[2,2])/2
acc_bal

########## ACC Vs Depth 
# Vamos ver como as acur�cias de treinamento e de valida��o 
# se comportam conforme variamos o tamanho da �rvore de decis�o. 
accPerDepth <- data.frame(depth=numeric(15), accTrain=numeric(15), accVal=numeric(15))
summary(accPerDepth)
for (maxDepth in 1:15){
    treeModel <- rpart(formula=class ~ variance + skewness + curtosis + entropy, 
                       data=trainSet, method="class",
                       control=rpart.control(minsplit=2, cp=0.0, 
                                             maxdepth=maxDepth, xval = 0),
                       parms= list(split="information"))
    
    # Avaliando no conjunto de treinamento
    train_pred <- predict(treeModel, trainSet, type="class")
    cm_train <- confusionMatrix(data = as.factor(train_pred), 
                          reference = as.factor(trainSet$class), 
                          positive='forgery')
    
    cm_train_relative <- calculaMatrizConfusaoRelativa(cm_train)
    acc_bal_train <- (cm_train_relative[1,1] + cm_train_relative[2,2])/2
    
    # Avaliando no conjunto de validacao
    val_pred <- predict(treeModel, valSet, type="class")
    cm_val <- confusionMatrix(data = as.factor(val_pred), 
                                reference = as.factor(valSet$class), 
                                positive='forgery')
    
    cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)
    acc_bal_val <- (cm_val_relative[1,1] + cm_val_relative[2,2])/2
    
    accPerDepth[maxDepth,] = c(maxDepth, 
                               acc_bal_train, 
                               acc_bal_val)
}

accPerDepth <- melt(accPerDepth, id="depth")  # convert to long format
ggplot(data=accPerDepth, aes(x=depth, y=value, colour=variable)) + geom_line() + geom_point()

###### Taking the best tree to run on test
treeModel <- rpart(formula=class ~ variance + skewness + curtosis + entropy, 
                   data=trainSet, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, 
                                         maxdepth=5, xval = 0),
                   parms= list(split="information"))

test_pred <- predict(treeModel, testSet, type="class")
cm_test <- confusionMatrix(data = as.factor(test_pred), 
                          reference = as.factor(testSet$class), 
                          positive='forgery')

cm_test_relative <- calculaMatrizConfusaoRelativa(cm_test)
cm_test_relative

acc_bal_test <- (cm_test_relative[1,1] + cm_test_relative[2,2])/2
acc_bal_test

############# Floresta Aleat�ria ################
help(randomForest)

# Treina a Floresta Aleat�ria 
# mtry eh o numero de features que cada arvore da floresta 
# apresentara. Ou seja, para cada arvore da floresta, mtry
# features sao aleatoriamente amostradas para treinar esta arvore. 
rfModel <- randomForest(formula=class ~ variance + skewness 
                        + curtosis + entropy, 
                        data= trainSet, ntree=12, mtry=3)


# Plota o erro para cada classe e para o Out-Of-Bag (OOB)
# O OOB � calculado seguindo a seguinte l�gica: uma arvore qualquer
# da floresta eh treinada com amostragem aleatoria e com repeticao dos
# exemplos de treinamento, assim os exemplos de treinamento que
# nao estao nesta amostragem nao foram considerados para treinar esta arvore. em particular. 
# Esses exemplos nao usados no treino sao usados para validar esta
# arvore e calcular sua performance. Repare que, para arvores diferentes,
# ha exemplos de treinamento diferentes e portanto os exemplos utilizados
# nesta validaco tambem sao diferentes. Como nao podemos comparar a 
# performance de cada arvore indivudualmente justamente pelo fato de 
# apresentarem conjuntos de validacao diferentes, toma-se a media da 
# performance para cada arvore sobre o respectivo conjunto de validacao, e
# essa performance eh chamada de Out-Of-Bag Error (OOB Error). 
# IMPORTANTE: OOB nao eh sinonimo de erro no conjunto de validacao! Ele
# eh uma especie de validacao interna da Floresta Aleatoria. Por esse motivo
# deixamos SEMPRE um conjunto de validacao externo, como temos neste 
# exercicio para comparar os modelos!

layout(matrix(c(1,2),nrow=1), width=c(4,1)) 
par(mar=c(5,4,4,0)) # Sem margem no lado direito 
plot(rfModel, log="y")
par(mar=c(5,0,4,2)) # Sem margem no lado esquerdo
plot(c(0,1),type="n", axes=F, xlab="", ylab="")
legend("top", colnames(rfModel$err.rate),col=1:4,cex=0.8,fill=1:4)


sizes = treesize(rfModel)
minSize = min(sizes)
maxSize = max(sizes)
# Verifica o tamanho das �rvores a partir de um histograma
# de distribui��o dos tamanhos.
hist(sizes,
     main="Histogram of Tree Depths",
     xlab="Depth",
     ylab="Frequency",
     xlim=c(minSize,maxSize),
     ylim=c(0,maxSize),
     las=1, 
     breaks=maxSize - minSize,
     xaxt="n")
axis(1, at=seq(minSize, maxSize, by=1), labels=seq(minSize, maxSize, by=1))


# Matriz de Confus�o
val_pred <- predict(rfModel, valSet, type="class")
cm_val <- confusionMatrix(data = as.factor(val_pred), 
                           reference = as.factor(valSet$class), 
                           positive='forgery')

cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)
cm_val_relative

acc_bal_val <- (cm_val_relative[1,1] + cm_val_relative[2,2])/2
acc_bal_val


# Vamos ver agora como as acur�cias de treinamento e de valida��o
# variam conforme aumentamos o n�mero de �rvores na floresta. 
set.seed(42)
nTreeList = c(1, 5, 10, 25, 50, 100, 250, 500, 1000)
accPerNTrees <- data.frame(ntree=numeric(length(nTreeList)), 
                           accTrain=numeric(length(nTreeList)), 
                           accVal=numeric(length(nTreeList)))


for (i in 1:length(nTreeList)){
    rfModel <- randomForest(formula=class ~ variance + skewness 
                            + curtosis + entropy, 
                            data= trainSet, ntree=nTreeList[i], mtry=3)
    
    # Avaliando no conjunto de treinamento
    train_pred <- predict(rfModel, trainSet, type="class")
    cm_train <- confusionMatrix(data = as.factor(train_pred), 
                                reference = as.factor(trainSet$class), 
                                positive='forgery')
    
    cm_train_relative <- calculaMatrizConfusaoRelativa(cm_train)
    acc_bal_train <- (cm_train_relative[1,1] + cm_train_relative[2,2])/2
    
    # Avaliando no conjunto de validacao
    val_pred <- predict(rfModel, valSet, type="class")
    cm_val <- confusionMatrix(data = as.factor(val_pred), 
                              reference = as.factor(valSet$class), 
                              positive='forgery')
    
    cm_val_relative <- calculaMatrizConfusaoRelativa(cm_val)
    acc_bal_val <- (cm_val_relative[1,1] + cm_val_relative[2,2])/2
    
    accPerNTrees[i,] = c(nTreeList[i], 
                         acc_bal_train, 
                         acc_bal_val)
}

accPerNTrees <- melt(accPerNTrees, id="ntree")  # convert to long format
ggplot(data=accPerNTrees, aes(x=ntree, y=value, colour=variable)) + geom_line() + geom_point()

#### Avalia��o da melhor floresta no teste ####
# Treina a Floresta Aleat�ria
rfModel <- randomForest(formula=class ~ variance + skewness 
                        + curtosis + entropy, 
                        data= trainSet, ntree=100)

test_pred <- predict(rfModel, testSet, type="class")
cm_test <- confusionMatrix(data = as.factor(test_pred), 
                           reference = as.factor(testSet$class), 
                           positive='forgery')

cm_test_relative <- calculaMatrizConfusaoRelativa(cm_test)
cm_test_relative

acc_bal_test <- (cm_test_relative[1,1] + cm_test_relative[2,2])/2
acc_bal_test


######## Execu��o do Exemplo da Aula #############
classExample <- data.frame(tempo=c("ensolarado", "ensolarado", "nublado","chover",
                                   "chover", "chover", "nublado", "ensolarado",
                                   "ensolarado", "chover", "ensolarado", "nublado",
                                   "nublado", "chover"), 
                           temperatura=c("calor", "calor", "calor", "moderada", "frio",
                                         "frio", "frio", "moderada", "frio", "moderada",
                                         "moderada", "moderada", "calor", "moderada"),
                           umidade=c("Alta", "Alta", "Alta", "Alta", "Normal", "Normal",
                                     "Normal", "Alta", "Normal", "Normal", "Normal",
                                     "Alta", "Normal", "Alta"),
                           vento=c("Fraco", "Forte", "Fraco", "Fraco", "Fraco", "Forte",
                                   "Forte", "Fraco", "Fraco", "Fraco", "Forte", "Forte",
                                   "Fraco", "Forte"),
                           target=c("nao", "nao", "sim", "sim", "sim", "nao", "sim",
                                    "nao", "sim", "sim", "sim", "sim", "sim", "nao"),
                           stringsAsFactors = TRUE)

dim(classExample)
summary(classExample)

treeModel <- rpart(formula=target ~ tempo + temperatura + umidade + vento, 
                   data=classExample, method="class",
                   control=rpart.control(minsplit=2, cp=0.0, xval = 0),
                   parms= list(split="information"))



printcp(treeModel)

summary(treeModel)
# Plota a �rvore de decis�o podada
rpart.plot(treeModel, 
           extra=104, box.palette="GnBu",
           branch.lty=3, shadow.col="gray", nn=TRUE)

# Node number 1: 14 observations,    complexity param=0.3
# predicted class=sim  expected loss=0.3571429  P(node) =1
# class counts:      5     9
# probabilities: 0.357 0.643 
# left son=2 (10 obs) right son=3 (4 obs)
# Primary splits:
#    tempo       splits as  LLR, improve=2.1931200, (0 missing)
# umidade     splits as  LR,  improve=1.4734210, (0 missing)
# vento       splits as  LR,  improve=0.4670276, (0 missing)
# temperatura splits as  LRR, improve=0.2433601, (0 missing)


# Para o c�lculo do "improve" do atributo "tempo" ,
# segue-se o seguinte algoritmo:
# Toma-se "Chover" e "Ensolarado" em um �nico conjunto. Se o exemplo 
# apresentar no atributo "tempo" os valores "Chover" ou "Ensolarado", 
# a �rvore desce para a esquerda. Por isso as duas primeiras letras ap�s 
# "split as" s�o "LL". Se apresentar o valor "Nublado" vai para a direita, 
# repare o "R" como �ltima letra ("splits as  LLR"). 
# Sabe-se que os dois primeiros L's e o
# �ltimo R referenciam os atributos "Chover", "Ensolarado" e "Nublado" 
# porque estes s�o apresentados em ordem alfab�tica na descri��o do n�.

# Temos N = 14 exemplos no total, sendo 5 com target "nao" e 9 com target
# "sim". Assim, calculando a entropia com f�rmula vista em aula, mas
# trocando a base do logaritmo para o n�mero de Euler, temos:

Entropy_S = -((5/14)*log(5/14) + (9/14)*log(9/14))
Entropy_S 

# Assim sabemos que Entropy_S = 0.6518 
# Vamos utilizar a f�rmula do ganho visto em aula, no entanto considerando
# "Chover" e "Ensolarado" como um �nico n�vel, e "Nublado" como outro n�vel.
# Assim temos "nao" com 5 elementos e "sim" com 5 elementos para "Chover"
# + "Ensolarado", e 0 elementos com "nao"e 4 elementos com "sim" para n�vel
# Nublado. Agora vamos calcular a entropia por n�vel do atributo:


Entropy_chover_ensolarado = -((5/10)*log(5/10) + (5/10)*log(5/10))
Entropy_chover_ensolarado

# Zero substituido por 1e-12 para n�o resultar em NaN
Entropy_nublado = -((0/4)*log(1e-12/4) + (4/4)*log(4/4))
Entropy_nublado

# Assim temos que a Entropia para os n�veis "Chover" + "Ensolarado" 
# � igual a 0.6932, e para "Nublado" � zero. Agora tomando as quantidades
# de exemplos de cada n�vel temos: 10 para "Chover" + "Ensolarado" e 
# 4 para "Nublado". Colocando os valores na f�rmula do Ganho temos:

gain = Entropy_S - (10/14)*Entropy_chover_ensolarado - (4/14)*Entropy_nublado
gain

# Agora basta multiplicar o ganho pelo n�mero total de exemplos que temos:

improve = 14*gain
improve

# Que � exatamente o valor em "improve" mostrado no "Primary Splits". 
# H� casos em que o valor n�o � exatamente o mesmo, com erros na terceira 
# ou quarta casa decimal. Isso ocorre devido � maneira como a biblioteca 
# "rpart" faz o c�lculos e quest�es de arrendodamento e truncamento. 
# De qualquer maneira, esse � o passo a passo que � executado 
# para cada atributo e em cada n� da �rvore gerada. Como visto em aula, 
# toma-se aquele  atributo que gera o maior "improve". 

