########################################
# Trabalho Final - 0611
# Nome(s):
# Daniel Noriaki Kurosawa
# Eric Uyemura Suda
# Fernando Shigeru Wakabayashi
########################################

# ----------------------------------------------
# carregando bibliotecas
# ----------------------------------------------
install.packages("dplyr")
install.packages("ggcorrplot")
install.packages("ggpubr")
install.packages("ggplot2")

library(ggplot2)
library(dplyr)
library(ggcorrplot)
library(ggpubr)

# ----------------------------------------------
# 1) Pré-tratamento de dados
# ----------------------------------------------

# ----------------------
# a)carregando dataframe
# a)colocando nome nas colunas do dataframe
# ----------------------
names <- c("horario", "temp", "vento", "umid", "sensa")
con <- url("https://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
cepagri <- read.table(con, header = FALSE, 
                      fill = TRUE, sep = ";", 
                      col.names = names)
                      

#verificando class
class(cepagri)

summary(cepagri)

summary(cepagri$horario)

class(cepagri$horario)
cepagri$horario <- as.POSIXct(cepagri$horario,
                              format = '%d/%m/%Y-%H:%M', 
                              tz = "America/Sao_Paulo")

class(cepagri$horario)

# menor data do dataframe
min(cepagri$horario)

# maior data do dataframe
max(cepagri$horario)

# ----------------------
# B) Delimitação do Intervalo a ser considerado 
# ----------------------

data_min <- as.POSIXct('2015-01-01',format= '%Y-%m-%d')
data_max <- as.POSIXct('2020-12-31',format= '%Y-%m-%d')

class(data_min)

cepagri <-cepagri[cepagri$horario >= data_min & cepagri$horario <= data_max,]


#RESETANDO o INDEX
row.names(cepagri) <- NULL

# verificando novos limites de data
min(cepagri$horario)
max(cepagri$horario)

#verificação de como ficou o dataframe
summary(cepagri)

unique(cepagri$temp)
# ----------------------
# c) Remoção das colunas Marcadas como erro
# ----------------------

summary(cepagri$temp)

#criando uma copia do dataset para ser limpa
cepagri_cleaned <- cepagri


# CRIANDO DATAFRAME PARA CONTROLE DE QUANTIDADE DE LINHAS DELETADAS NO PRÉ PROCESSAMENTO
etapa = "Dataframe Original" #modificar se necessário para o mais adequado 
tamanho = length(cepagri_cleaned$horario)
num_lin_retiradas = 0
pctg = (length(cepagri$horario)-length(cepagri_cleaned$horario))/length(cepagri$horario)*100
df_controle_pre <- data.frame(etapa,tamanho,num_lin_retiradas,pctg)
colnames(df_controle_pre) <- c("Etapa","Tamanho do Dataframe","N de linhas deletadas","% Deletada")
df_controle_pre


#verificando a classe da temperatura
class(cepagri_cleaned$temp)

#transformando valores para numérico, valores que não são numéricos são retornados como <NA>
cepagri_cleaned$temp <- as.numeric(cepagri_cleaned$temp)

num_lin_retiradas <- length(cepagri_cleaned[is.na(cepagri_cleaned$sensa),]$sensa)

#removendo valores <NA>
cepagri_cleaned <- cepagri_cleaned[!is.na(cepagri_cleaned$sensa),]


#atualizando dataset de controle
etapa = "Remoção - [ERRO] Sensação Térmica" #modificar se necessário para o mais adequado 
tamanho = length(cepagri_cleaned$horario)
n_cols_retiradas = length(cepagri$horario) - length(cepagri_cleaned$horario)
pctg = (length(cepagri$horario)-length(cepagri_cleaned$horario))/length(cepagri$horario)*100
df_controle_etapa <- data.frame(etapa,tamanho,num_lin_retiradas,pctg)
colnames(df_controle_etapa) <- c("Etapa","Tamanho do Dataframe","N de linhas deletadas","% Deletada")
df_controle_pre  <- rbind(df_controle_pre,df_controle_etapa)
df_controle_pre

# verificando a classe do array pós retificação dos dados
class(cepagri_cleaned$temp)

summary(cepagri_cleaned)

# ----------------------
# D) Análise e remoção de outliers
# ----------------------

# Resetando o índice das linhas

row.names(cepagri_cleaned) <- NULL
#definindo uma copia do dataset com colunas de informações temporais
cepagri_posix_lt <- cepagri_cleaned

cepagri_posix_lt$horario <- as.POSIXlt(cepagri_posix_lt$horario)
cepagri_posix_lt$ano <- unclass(cepagri_posix_lt$horario)$year + 1900
cepagri_posix_lt$mes <- unclass(cepagri_posix_lt$horario)$mon + 1
cepagri_posix_lt$dia <- unclass(cepagri_posix_lt$horario)$yday + 1
cepagri_posix_lt$dia_do_mes <- unclass(cepagri_posix_lt$horario)$mday 
cepagri_posix_lt$hora <- unclass(cepagri_posix_lt$horario)$hour
cepagri_posix_lt$min <- unclass(cepagri_posix_lt$horario)$min
cepagri_posix_lt$horario <- as.POSIXct(cepagri_posix_lt$horario,
                              format = '%d/%m/%Y-%H:%M',
                              tz = "America/Sao_Paulo")
summary(cepagri_posix_lt)
cepagri_cleaned <- cepagri_posix_lt
######
# Verificando média, mediana e desvio padrão para cada dia do ano
tapply(cepagri_posix_lt$temp, cepagri_posix_lt$dia, mean)
tapply(cepagri_posix_lt$temp, cepagri_posix_lt$dia, median)
tapply(cepagri_posix_lt$temp, cepagri_posix_lt$dia, sd)

# gráficos para visualização de Outliers **

summary(cepagri_cleaned)


#temperatura - histograma
grap_temp_hist <- (ggplot(cepagri_cleaned, aes(x=temp, y= ..density..)) +
  geom_histogram(color='White', bins=100) +
  geom_density() +
  ggtitle("Histograma - Temperatura por ano"));grap_temp_hist


#umidade - histograma
grap_umid_hist <-(ggplot(cepagri_cleaned, aes(x=umid, y= ..density..)) +
  geom_histogram(color='White', bins=100) +
  ggtitle("Histograma Umidade por ano")+
  geom_density());grap_umid_hist
#possíveis outliers em 0 e 100

#vento - histograma
grap_vento_hist <-(ggplot(cepagri_cleaned, aes(x=vento, y= ..density..)) +
  geom_histogram(color='White', bins=100) +
  ggtitle("Histograma Velocidade do Vento por ano")+
  geom_density());grap_vento_hist
#possíveis outliers acima de 100

grap_sensa_boxplot <- (ggplot(cepagri_cleaned, aes(x = mes, y = sensa, group = mes)) +
  ggtitle("Boxplot Sensação Térmica por ano")+
  scale_x_continuous(
    breaks=c(1:12),
    labels=c("Jan", "Fev", "Mar","Abr","Mai","Jun","Jul","Aug","Sep","Oct","Nov","Dez"))+
  geom_boxplot());grap_sensa_boxplot


# juntando gráficos 
ggarrange(grap_temp_hist, grap_umid_hist, grap_vento_hist , grap_sensa_boxplot, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


#ggsave("outliers.png")

##### Rodar, mas esperar carregar a imagem 
grap_umid_geom_point <- (ggplot(cepagri_cleaned, aes(x = horario, y = umid, group = mes)) + 
  ggtitle("Scatter da Umidade por ano")+
  labs(x = "Anos")+
  geom_point());grap_umid_geom_point


#ggsave("outliers_umid.png")

##### COMENTAR SAIDAS ####

# Comentários:
#### Temperatura - Minimos e maximos estao amplitude(5 , 37,4) graus
#### Vento - Valores Ok (existem noticias que confirmam ventos de ate 143km/h
#### Umidade - possíveis outliers em 0 e 100, para o caso de umidade 0, retiramos os valores
# para o caso de 100% retiramos os valores em uma cópia do dataframe, para usar os dados dos outros sensores

#Analise da umidade
#Tabela para analise do entorno da situação 
#verificando outliers
cepagri_cleaned[cepagri_cleaned$umid == 0.0, ]

cepagri_cleaned[cepagri_cleaned$umid == 100, ]
#encontrando indices para analisar


cepagri_cleaned[(263688-10):(263688+10),]
#TABELA PARA SALVAR PARA ANALISE  TABELA 1 - ANALISE DE OUTLIERS - UMIDADE

#write.csv(cepagri_cleaned[(263688-10):(263688+10),],"./Entorno_valores.csv")

unique(cepagri_cleaned[cepagri_cleaned$umid== 100,6])

#observando que realmente são outlers * 
ggplot(cepagri_cleaned[ cepagri_cleaned$ano ==2015 & cepagri_cleaned$mes== 03 , ], aes(x=umid, y= ..density..)) + geom_histogram(color='White', bins=100) + geom_density()
# observando quer existe uma tendencia dos valores de 100 serem outliers (12% das medidas do mes de mar,2015 sao 100%, esse valor elevado se repetiu em varios meses entre 2015-2017)
length(cepagri_cleaned[ cepagri_cleaned$ano ==2015 & cepagri_cleaned$mes== 03 & cepagri_cleaned$umid== 100, ]$horario)/length(cepagri_posix_lt[ cepagri_posix_lt$ano ==2015 & cepagri_posix_lt$mes== 03, ]$horario)
summary(cepagri_cleaned$umid)



# anulando as linhas com erro
cepagri_cleaned[cepagri_cleaned$umid == 0.0, ] <- NA
cepagri_cleaned[cepagri_cleaned$umid == 100, ] <- NA

num_lin_retiradas <- length(cepagri_cleaned[is.na(cepagri_cleaned$umid),]$umid)
summary(cepagri_cleaned$umid)


# Retirando valores <NA>
cepagri_cleaned <- cepagri_cleaned[!is.na(cepagri_cleaned$umid),]
head(cepagri_cleaned)

#atualizando dataset de controle
etapa = "Remoção - Outliers Umidade" #modificar se necessário para o mais adequado 
tamanho = length(cepagri_cleaned$horario)
pctg = (length(cepagri$horario)-length(cepagri_cleaned$horario))/length(cepagri$horario)*100
df_controle_etapa <- data.frame(etapa,tamanho,num_lin_retiradas, pctg)
colnames(df_controle_etapa) <- c("Etapa","Tamanho do Dataframe","N de linhas deletadas","% Deletada")
df_controle_pre  <- rbind(df_controle_pre,df_controle_etapa)
df_controle_pre
summary(cepagri_cleaned)



# Validacao sensacao termica
# Valores estranhos de 99 graus de sensacao termica
stats <- summary(cepagri_cleaned$sensa)

# Pelo plot podemos observar que ha valores marcados no final do plot que parecem outliers

#Carregar, mas esperar rodar os pontos, imagem pesada *
ggplot(cepagri_cleaned, aes(x = horario, y = sensa, group = mes)) +   geom_point()
#Selecionando os 20 primeiros valores com sensação térmica negativa não nulos
head(cepagri_cleaned[(cepagri_cleaned$sensa < 0) & !(is.na(cepagri_cleaned$sensa)),], 20)

# Verificando minimo valor de sensação térmica
min(cepagri_cleaned$sensa)

#verificando o index do valor mínimo da sensação térmica
cepagri_cleaned[cepagri_cleaned$sensa == -8,]

# Analisando os valores em tono do valor mínimo de sensação térmica
cepagri_cleaned[74568:74588,] #alterar


#Selecionando os 20 primeiros valores com sensação térmica maiores que 90 não nulos
tail(cepagri_cleaned[(cepagri_cleaned$sensa > 90) & !(is.na(cepagri_cleaned$sensa)),], 20)


# Aplicando NA para os valores de sensacao termica = 99.9 

cepagri_cleaned[cepagri_cleaned$sensa == 99.9, 5] <- NA
num_lin_retiradas <- length(cepagri_cleaned[is.na(cepagri_cleaned$sensa),]$sensa)

summary(cepagri_cleaned$sensa)



# Retirando valores <NA>
cepagri_cleaned <- cepagri_cleaned[!is.na(cepagri_cleaned$sensa),]


#atualizando dataset de controle
etapa = "Remoção - Outliers Sensação Térmica = 99.9" #modificar se necessário para o mais adequado 
tamanho = length(cepagri_cleaned$horario)
pctg = (length(cepagri$horario)-length(cepagri_cleaned$horario))/length(cepagri$horario)*100
df_controle_etapa <- data.frame(etapa,tamanho,num_lin_retiradas, pctg)
colnames(df_controle_etapa) <- c("Etapa","Tamanho do Dataframe","N de linhas deletadas","% Deletada")
df_controle_pre  <- rbind(df_controle_pre,df_controle_etapa)
df_controle_pre
summary(cepagri_cleaned)


# Resetando o índice das linhas
row.names(cepagri_cleaned) <- NULL



# ----------------------
# E) Trabalhando com Valores repetidos
# ----------------------

#definindo função para verificar a presenca de valores repetidos 

consecutive <- function(vector, k = 2) {
  n <- length(vector)
  result <- logical(n)
  for (i in k:n)
    if (all(vector[(i-k+1):i] == vector[i]))
      result[(i-k+1):i] <- TRUE
  return(result)
}


# verificando ocorrências repetidas por tempos prologandos para decidir melhor
# estrategia para tratar os dados
length(cepagri_cleaned[consecutive(cepagri_cleaned$temp, 48),]$temp)  # 08 horas
length(cepagri_cleaned[consecutive(cepagri_cleaned$temp, 72),]$temp)  # 12 horas
length(cepagri_cleaned[consecutive(cepagri_cleaned$temp, 144),]$temp)  # 01 dia
length(cepagri_cleaned[consecutive(cepagri_cleaned$temp, 288),]$temp)  # 02 dias
length(cepagri_cleaned[consecutive(cepagri_cleaned$temp, 720),]$temp)  # 05 dias

length(cepagri_cleaned[consecutive(cepagri_cleaned$vento, 48),]$vento)  # 08 horas
length(cepagri_cleaned[consecutive(cepagri_cleaned$vento, 72),]$vento)  # 12 horas
length(cepagri_cleaned[consecutive(cepagri_cleaned$vento, 144),]$vento)  # 01 dia
length(cepagri_cleaned[consecutive(cepagri_cleaned$vento, 288),]$vento)  # 02 dias
length(cepagri_cleaned[consecutive(cepagri_cleaned$vento, 720),]$vento)  # 05 dias

length(cepagri_cleaned[consecutive(cepagri_cleaned$umid, 48),]$umid)  # 08 horas
length(cepagri_cleaned[consecutive(cepagri_cleaned$umid, 72),]$umid)  # 12 horas
length(cepagri_cleaned[consecutive(cepagri_cleaned$umid, 144),]$umid)  # 01 dia
length(cepagri_cleaned[consecutive(cepagri_cleaned$umid, 288),]$umid)  # 02 dias
length(cepagri_cleaned[consecutive(cepagri_cleaned$umid, 720),]$umid)  # 05 dias

length(cepagri_cleaned[consecutive(cepagri_cleaned$sensa, 48),]$sensa)  # 08 horas
length(cepagri_cleaned[consecutive(cepagri_cleaned$sensa, 72),]$sensa)  # 12 horas
length(cepagri_cleaned[consecutive(cepagri_cleaned$sensa, 144),]$sensa)  # 01 dia
length(cepagri_cleaned[consecutive(cepagri_cleaned$sensa, 288),]$sensa)  # 02 dias
length(cepagri_cleaned[consecutive(cepagri_cleaned$sensa, 720),]$temp)  # 05 dias



#removendo dados que se repitam por um dia ou mais em qualquer um dos campos
num_lin_retiradas <- length(cepagri_cleaned[
      (
        consecutive(cepagri_cleaned$temp, 144)|
        consecutive(cepagri_cleaned$vento, 144)|
        consecutive(cepagri_cleaned$umid, 144)|
        consecutive(cepagri_cleaned$sensa, 144)
      ),
    ]$horario)
cepagri_cleaned <- cepagri_cleaned[
      !(
        consecutive(cepagri_cleaned$temp, 144)|
        consecutive(cepagri_cleaned$vento, 144)|
        consecutive(cepagri_cleaned$umid, 144)|
        consecutive(cepagri_cleaned$sensa, 144)
      ),
    ]



#atualizando dataset de controle
etapa = "Remoção - Valores repetidos por mais de um dia" #modificar se necessário para o mais adequado 
tamanho = length(cepagri_cleaned$horario)
pctg = (length(cepagri$horario)-length(cepagri_cleaned$horario))/length(cepagri$horario)*100
df_controle_etapa <- data.frame(etapa,tamanho,num_lin_retiradas, pctg)
colnames(df_controle_etapa) <- c("Etapa","Tamanho do Dataframe","N de linhas deletadas","% Deletada")
df_controle_pre  <- rbind(df_controle_pre,df_controle_etapa)
df_controle_pre
summary(cepagri_cleaned)
#write.csv(df_controle_pre,"./linhas_removidas.csv")

# Resetando o índice das linhas
row.names(cepagri_cleaned) <- NULL



# ----------------------------------------------
# 2) Análise de dados
# ----------------------------------------------

# ----------------------------------------------
# a)Valores mais frequentes
# ----------------------------------------------

ggplot(cepagri_cleaned, aes(x = mes, y = umid, group = mes)) + 
  geom_boxplot()

tail(names(sort(table(cepagri_cleaned$temp))), )
tail(names(sort(table(cepagri_cleaned$sensa))), )
tail(names(sort(table(cepagri_cleaned$vento))), )
tail(names(sort(table(cepagri_cleaned$umid))), )
# ----------------------------------------------
# a) Análise das médias e variações por ano
# ----------------------------------------------
#media
table <- cepagri_cleaned %>% group_by(dia_do_mes,mes) %>% summarise(media = mean(temp))
avg_temp_heatmap <- ggplot(table, aes(x = dia_do_mes, y = mes, fill = media)) +

ggtitle("Heatmap da média de Temperatura por dia") +
geom_tile() +
scale_fill_gradient(low="navy", high="red") +
scale_y_continuous(
  breaks=c(1:12),
  labels=c("Jan", "Fev", "Mar","Abr","Mai","Jun","Jul","Aug","Sep","Oct","Nov","Dez")
); avg_temp_heatmap


#desvio padrao
table <- cepagri_cleaned %>% group_by(dia_do_mes,mes) %>% summarise(sd = sd(temp))
sd_temp_heatmap <-ggplot(table, aes(x=dia_do_mes, y = mes, fill=sd)) +
  ggtitle("Heatmap do desvio padrão de Temperatura por dia") +
geom_tile() +
scale_fill_gradient(low="navy", high="red") +
scale_y_continuous(
  breaks=c(1:12),
  labels=c("Jan", "Fev", "Mar","Abr","Mai","Jun","Jul","Aug","Sep","Oct","Nov","Dez")
); sd_temp_heatmap

#media
table <- cepagri_cleaned %>% group_by(dia_do_mes,mes) %>% summarise(media = mean(umid))
avg_umid_heatmap <- ggplot(table, aes(x = dia_do_mes, y = mes, fill = media)) +
ggtitle("Heatmap da média de umidade por dia") +
geom_tile() +
scale_fill_gradient(low="navy", high="red") +
scale_y_continuous(
  breaks=c(1:12),
  labels=c("Jan", "Fev", "Mar","Abr","Mai","Jun","Jul","Aug","Sep","Oct","Nov","Dez")
); avg_umid_heatmap


#desvio padrao
table <- cepagri_cleaned %>% group_by(dia_do_mes,mes) %>% summarise(sd = sd(umid))
sd_umid_heatmap <-ggplot(table, aes(x=dia_do_mes, y = mes, fill=sd)) +
  ggtitle("Heatmap do desvio padrão de umidade por dia") +
geom_tile() +
scale_fill_gradient(low="navy", high="red") +
scale_y_continuous(
  breaks=c(1:12),
  labels=c("Jan", "Fev", "Mar","Abr","Mai","Jun","Jul","Aug","Sep","Oct","Nov","Dez")
); sd_umid_heatmap

#### avg sensa
table <- cepagri_cleaned %>% group_by(dia_do_mes,mes) %>% summarise(media = mean(sensa))
avg_sensa_heatmap <- ggplot(table, aes(x = dia_do_mes, y = mes, fill = media)) +
ggtitle("Heatmap da média de Sensação por dia") +
geom_tile() +
scale_fill_gradient(low="navy", high="red") +
scale_y_continuous(
  breaks=c(1:12),
  labels=c("Jan", "Fev", "Mar","Abr","Mai","Jun","Jul","Aug","Sep","Oct","Nov","Dez")
); avg_umid_heatmap

#### avg vento
table <- cepagri_cleaned %>% group_by(dia_do_mes,mes) %>% summarise(media = mean(vento))
avg_vent_heatmap <- ggplot(table, aes(x = dia_do_mes, y = mes, fill = media)) +
ggtitle("Heatmap da média de Vento por dia") +
geom_tile() +
scale_fill_gradient(low="navy", high="red") +
scale_y_continuous(
  breaks=c(1:12),
  labels=c("Jan", "Fev", "Mar","Abr","Mai","Jun","Jul","Aug","Sep","Oct","Nov","Dez")
); avg_umid_heatmap
# juntando gráficos 
ggarrange(avg_temp_heatmap, sd_temp_heatmap, avg_umid_heatmap, sd_umid_heatmap,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

##### salvar graficos
ggarrange(avg_temp_heatmap,  avg_umid_heatmap, avg_vent_heatmap, avg_sensa_heatmap,
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)

#ggsave("avg_heatmap.png")

# Analise univariada por histograma após a retirada de outliers
#temperatura - histograma
grap_temp_hist <- (ggplot(cepagri_cleaned, aes(x=temp, y= ..density..)) +
  geom_histogram(color='White', bins=100) +
  geom_density() +
  ggtitle("Histograma - Temperatura por ano"));grap_temp_hist



#umidade - histograma
grap_umid_hist <-(ggplot(cepagri_cleaned, aes(x=umid, y= ..density..)) +
  geom_histogram(color='White', bins=100) +
  ggtitle("Histograma Umidade por ano")+
  geom_density());grap_umid_hist
#possíveis outliers em 0 e 100

#vento - histograma
grap_vento_hist <-(ggplot(cepagri_cleaned, aes(x=vento, y= ..density..)) +
  geom_histogram(color='White', bins=100) +
  ggtitle("Histograma Velocidade do Vento por ano")+
  geom_density());grap_vento_hist
#possíveis outliers acima de 100

grap_sensa_boxplot <- (ggplot(cepagri_cleaned, aes(x = mes, y = sensa, group = mes)) +
  ggtitle("Boxplot Sensação Térmica por ano")+
  scale_x_continuous(
    breaks=c(1:12),
    labels=c("Jan", "Fev", "Mar","Abr","Mai","Jun","Jul","Aug","Sep","Oct","Nov","Dez"))+
  geom_boxplot());grap_sensa_boxplot


# juntando gráficos 
ggarrange(grap_temp_hist, grap_umid_hist, grap_vento_hist , grap_sensa_boxplot, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)


#ggsave("sem_outliers.png")

# ----------------------------------------------
# b) Correlação entre dados
# ----------------------------------------------
# Calculando a matriz de correlação
corr <- cor(cepagri_cleaned[, 2:5]);corr

# Calculando a matrix de p-values
p_mat <- cor_pmat(cepagri_cleaned[, 2:5]);p_mat

ggcorrplot(corr, lab = TRUE)
ggcorrplot(p_mat, lab = TRUE)

cepagri_by_month <- cepagri_cleaned %>% group_by(ano,mes) %>% summarise(temp = mean(temp),vento = mean(vento), umid = mean(umid), sensa = mean(sensa))

cepagri_by_month$ano_mes <- paste(cepagri_by_month$ano, '-',cepagri_by_month$mes)

cepagri_by_month
corr <- cor(cepagri_by_month[, 3:6])
corr
# Calculando a matrix de p-values
p_mat <- cor_pmat(cepagri_by_month[, 3:6])
p_mat
ggcorrplot(corr, lab = TRUE)
ggcorrplot(p_mat, lab = TRUE)

#agrupando os dados por media, vemos que existe correlacao da t


# ----------------------------------------------
# c) Line plot Por ano
# ----------------------------------------------
cepagri_by_month

#PLOTAR GRAFICO DE LINHA DE TEMPERATURA POR ANO

line_plot_temperatura_ano <- (ggplot(cepagri_by_month, aes(x = ano_mes, y = temp,group = 1, colour = factor(ano))) +
  ggtitle("Grafico Temperatura por ano")+
  geom_point()+
  geom_smooth(method = "lm") +
  theme(axis.text.x = element_blank())+
  geom_line());
line_plot_temperatura_ano 
 ggsave("temp_anual.png")

cepagri_sem_2020 <- cepagri_by_month[cepagri_by_month$ano != 2015 & cepagri_by_month$ano != 2016,]
line_plot_temperatura_ano <- (ggplot(cepagri_sem_2020, aes(x = ano_mes, y = temp,group = 1, colour = factor(ano))) +
  ggtitle("Grafico Temperatura por ano")+
  geom_point()+
  geom_smooth(method = "lm") +
  theme(axis.text.x = element_blank())+
  geom_line());
line_plot_temperatura_ano 

length(cepagri_cleaned[cepagri_cleaned$ano == c(2019, 2020),]$temp)

tapply(cepagri_cleaned$temp, cepagri_cleaned$ano, length)

line_plot_vento_ano <- (ggplot(cepagri_by_month, aes(x = ano_mes, y = vento,group = 1, colour = factor(ano))) +
  ggtitle("Grafico Vento por ano")+
  geom_point()+
  geom_line());line_plot_vento_ano
  
line_plot_umid_ano <- (ggplot(cepagri_by_month, aes(x = ano_mes, y = umid,group = 1, colour = factor(ano))) +
  ggtitle("Grafico Umidade por ano")+
  geom_point()+
  geom_line());line_plot_umid_ano
  
line_plot_sensa_ano <- (ggplot(cepagri_by_month, aes(x = ano_mes, y = sensa,group = 1, colour = factor(ano))) +
  ggtitle("Grafico Sensação Térmica por ano")+
    geom_smooth(method = "lm") +
  geom_point()+
  geom_line());line_plot_sensa_ano

# juntando gráficos 
ggarrange(box_plot_temperatura_ano, box_plot_vento_ano, box_plot_umid_ano , box_plot_sensa_ano, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)



#Análise dos boxplots 
cepagri_by_month
box_plot_temperatura_ano <- (ggplot(cepagri_by_month, aes(x = ano, y = temp,group = ano, colour = factor(ano))) +
  ggtitle("Grafico Temperatura por ano")+  
  geom_boxplot() 
);
box_plot_temperatura_ano 


box_plot_vento_ano <- (ggplot(cepagri_by_month, aes(x = ano, y = vento,group = ano, colour = factor(ano))) +
  ggtitle("Grafico Vento por ano")+  
  geom_boxplot());
  
box_plot_vento_ano + geom_smooth(method = "lm") 

box_plot_umid_ano <- (ggplot(cepagri_by_month, aes(x = ano, y = umid,group = ano, colour = factor(ano))) +
  ggtitle("Grafico Umidade por ano")+ 
  geom_boxplot()); box_plot_umid_ano

# ggsave("boxplot_umid_ano.png")

box_plot_sensa_ano <- (ggplot(cepagri_by_month, aes(x = ano, y = sensa,group = ano, colour = factor(ano))) +
  ggtitle("Grafico Sensação Térmica por ano")+
  
  geom_boxplot());box_plot_sensa_ano + geom_smooth(method = "lm") 

# juntando gráficos 
ggarrange(box_plot_temperatura_ano, box_plot_vento_ano, box_plot_umid_ano , box_plot_sensa_ano, 
          labels = c("A", "B", "C", "D"),
          ncol = 2, nrow = 2)
