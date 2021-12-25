# o conjunto de dados dos recordes nacionais masculinos de oito provas
# de pista encontra-se na página:
#
# - http://www.stat.wisc.edu/~rich/JWMULT06dat/T8-6.DAT

# os dados são de 2005 e dizem respeito a 54 países, listados na primeira
# coluna do arquivo. As demais colunas contêm os resultados das seguintes
# provas (unidades): 100 m (s), 200 m (s), 400 m (s), 800 m (min), 1500 m(min),
# 5000 m (min), 10000 m (min) e maratona (min).

dados <- read.table(file="T8-6.DAT")

# dimensão 54 x 9
dim(dados)

# o vetor provas contém os nomes e unidades dos resultados das provas.
provas <- c("100 m (s)", "200 m (s)", "400 m (s)", "800 m (min)",
            "1500 m (min)", "5000 m (min)", "10000 m (min)",
            "Maratona (min)")

# armazena só os países
paises <- dados[, 1]

# excluir os países
dados <- dados[, -1]

# renomeia as variáveis e linhas
colnames(dados) <- provas
rownames(dados) <- paises

# sumário estatístico
summary(dados)

# gráficos boxplot 
boxplot(dados)

boxplot(scale(dados))

# análise de componentes principais
acpcor <- prcomp(dados, scale=TRUE)
summary(acpcor)

# visualiza componentes principais
screeplot(acpcor, type="lines")

# os desvios padrão dos componentes principais estão no componente sdev.
# a soma dos quadrados deles é igual a 8, conforme esperado (p = 8).
sum(acpcor$sdev^2)

[1] 8

# o primeiro componente principal responde por cerca de 84% da variância
# total dos dados padronizados, ao passo que se tomarmos os dois primeiros
# componentes atingimos cerca de 92% davariância total.  O primeiro componente
# principal tem variância 6,70 (acpcor$sdev[1]^2), bem maior do que a média
# das variâncias (igual a 1). Além disso, o gráfico da Fig. 4 indica que o
# número decomponentes a reter é dois. 

plot(1:ncol(dados), acpcor$sdev^2, type="b",
     xlab="Componente", ylab="Variância", 
     pch=20, cex.axis=1.3, cex.lab=1.3)

# verifique os resultados dos comandos screeplot(acpcor) e plot(acpcor)

# os coeficientes dos componentes principais (isto é, os autovetores e1,
# e2,..., ep da matriz decorrelações amostral)  estão armazenados no
# componente rotation (ou seja, acpcor$rotation). Os dois primeiros são
# mostrados abaixo.

acpcor$rotation
                      PC1         PC2          PC3         PC4         PC5
100 m (s)      -0.3323877 -0.52939911 -0.343859303 -0.38074525  0.29967117
200 m (s)      -0.3460511 -0.47039050  0.003786104 -0.21702322 -0.54143422
400 m (s)      -0.3391240 -0.34532929  0.067060507  0.85129980  0.13298631
800 m (min)    -0.3530134  0.08945523  0.782711152 -0.13427911 -0.22728254
1500 m (min)   -0.3659849  0.15365241  0.244270040 -0.23302034  0.65162403
5000 m (min)   -0.3698204  0.29475985 -0.182863147  0.05462441  0.07181636
10000 m (min)  -0.3659489  0.33360619 -0.243980694  0.08706927 -0.06133263
Maratona (min) -0.3542779  0.38656085 -0.334632969 -0.01812115 -0.33789097
                       PC6        PC7          PC8
100 m (s)      -0.36203713  0.3476470 -0.065701445
200 m (s)       0.34859224 -0.4398969  0.060755403
400 m (s)       0.07708385  0.1135553 -0.003469726
800 m (min)    -0.34130845  0.2588830 -0.039274027
1500 m (min)    0.52977961 -0.1470362 -0.039745509
5000 m (min)   -0.35914382 -0.3283202  0.705684585
10000 m (min)  -0.27308617 -0.3511133 -0.697181715
Maratona (min)  0.37516986  0.5941571  0.069316891

# as correlações entre os dois primeiros componentes principais e as variáveis
# são estimadas abaixo.
print(acpcor$sdev[1:2] * t(acpcor$rotation[, 1:2]), digits=3)

biplot(acpcor, xlab="CP1", ylab="CP2", cex.lab=1.5, cex.axis=1.5)

################################################################

# correlation matrix
matcor <- round(cor(dados), 2)
matcor

# plot
library(ggcorrplot)
ggcorrplot(matcor, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method="circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title="Correlograma", 
           ggtheme=theme_bw)

# PCA
# PCA com a matriz de cor
res.pca.cor <- prcomp(dados, scale.unit=T, graph=FALSE, ncp=Inf)
# matriz de covariância
round(cor(dados), 4)
