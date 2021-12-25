# Boston Housing: 506 registros e 14 variáveis (13 atributos e 1 variável de resposta)

# lê dados de entrada

houses <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data",
                     header=F, na.string = "?")

colnames(houses) <- c("CRIM", "ZN", "INDUS","CHAS",
                      "NOX","RM","AGE","DIS","RAD",
                      "TAX","PTRATIO","B","LSTAT","MEDV")

# calcula matriz de correlação (nos 13 primeiros atributos) para verificar se
# algumas variáveis são altamente correlacionadas e se PCA tem potencial para
# reduzir a dimensionalidade dos dados.

round(cor(houses[,-14]), 2)

plot(houses[,-14])

# aplica PCA
pca_houses <- prcomp(houses[,-14], scale=TRUE)

summary(pca_houses)

# matriz de rotação que mostra os pesos usados para criar os novos pontos
pca_houses$rot

# desvio padrão
pca_houses$sdev

#
pca_houses$x
head(pca_houses$x, 3)

# exibe componentes principais
plot(pca_houses)

# exibe componentes principais
plot(pca_houses, type="lines")

biplot(pca_houses)
