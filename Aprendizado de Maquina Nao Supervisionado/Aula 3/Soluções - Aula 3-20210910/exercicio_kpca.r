# carrega biblioteca
library(kernlab)

# carrega base de dados
data(iris)

# conjunto de teste (20 amostras em 150)
test <- sample(1:150, 20)

# para garantir repetibilidade dos resultados
set.seed(7)

# aplica análise de componentes principais com núcleo
# (função "Gaussian Radial Basis" é aplicada como núcleo)
kpc <- kpca(~., data=iris[-test, -5],
            kernel="rbfdot",
            kpar=list(sigma=0.2),
            features=2)

# apresenta primeiros vetores de componentes principais
head(pcv(kpc)) 

##            [,1]       [,2]
## [1,] -0.2241292 0.04477053
## [2,] -0.2172845 0.02801389
## [3,] -0.2145753 0.02855451
## [4,] -0.2239498 0.05151568
## [5,] -0.2032523 0.01213679
## [6,] -0.2235308 0.02982976

# mostra gráfico com a projeção dos dados
plot(rotated(kpc), col=as.integer(iris[-test, 5]),
     xlab="Componente Principal 1",
     ylab="Componente Principal 2")

# prediz pontos restantes
emb <- predict(kpc, iris[test,-5])

# points(emb, col=as.integer(iris[test, 5]))

Observações:
- pcv: matriz contendo os vetores das componentes principais
- eig: autovalores correspondentes
- rotated: dados originais projetados nas componentes principais
- xmatrix: dados originais
