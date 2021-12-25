# url <- 'http://www.biz.uiowa.edu/faculty/jledolter/DataMining/protein.csv'
url <- 'http://www.ic.unicamp.br/~helio/datasets/protein.csv'

# le dados
data <- read.csv(url) 

# remove coluna de indices
data$X <- NULL

# mostra resultados
head(data)

labcol <- 1 # number of label column

# variaveis normalizadas, rotulos removidos
data.stand <- scale(subset(data, select = -c(labcol)))

# rotulos
data.lab <- data[,labcol]

# renomeia linhas por rotulos para uma visao mais interessante
row.names(data.stand) <- data.lab

# cria modelo PCA
data.cpca <- princomp(data.stand)
plot(data.cpca)

# aplica PCA aos dados
data.pca <- predict(data.cpca, data)

# plot result
plot(data.pca, cex=0, main="PCA")
text(data.pca, labels=data.lab, col='black', cex=1)

# cria biplot
plot(data.cpca$loadings, cex = 0)
text(data.cpca$loadings, labels=labels(data.cpca$loadings)[[1]], cex=1)

# ou, de outra forma
biplot(data.cpca, expand=1, xlim=c(-0.5,0.5), ylim=c(-0.7,0.4))
