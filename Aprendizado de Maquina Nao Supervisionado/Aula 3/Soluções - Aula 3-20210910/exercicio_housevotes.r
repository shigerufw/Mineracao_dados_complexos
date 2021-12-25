library(e1071)
library(dplyr)  
library(caret)
library(mlbench)
library(magrittr) # need to run every time you start R and want to use %>%

# carrega base
data(HouseVotes84)

# verifica dados
head(HouseVotes84)

summary(HouseVotes84$Class)

summary(HouseVotes84)

# verifica se há dados faltantes
head(is.na(HouseVotes84))

CleanDataset <- na.omit(HouseVotes84)
qplot(Class, data=CleanDataset, geom="bar") + theme(axis.text.x=element_text(angle=90, hjust=1))

# tenta aplica PCA aos dados
HouseVotes84 %>% select(-Class) %>% prcomp
## Error in colMeans(x, na.rm = TRUE): 'x' must be numeric

# erro: há dados não numéricos. PCA requer dados numéricos, entretanto, "factors" são fornecidos.
# uma solução é mapear votos em 0s e 1s.

HouseVotes84 %>%
  select(-Class) %>%
  apply(c(1,2), . %>% { ifelse(as.character(.) == "n", 0, 1) }) %>%
  prcomp
## Error in svd(x, nu = 0): infinite or missing values in 'x'

# agora sim:
vote_patterns <- HouseVotes84 %>%
  select(-Class) %>%
  apply(c(1,2), . %>% { ifelse(as.character(.) == "n", 0, 1) }) %>%
  apply(c(1,2), . %>% { ifelse(is.na(.), 0.5, .) })

pca <- vote_patterns %>% prcomp

# mapear os padrões de votos em componentes principais
mapped_votes <- pca %>% predict(vote_patterns)

# mostra PC1 x PC2
mapped_votes %>%
  as.data.frame %>%
  cbind(Class = HouseVotes84$Class) %>%
  ggplot() +
  geom_point(aes(x = PC1, y = PC2, colour = Class))

# há uma clara separação nos padrões de votos, pelo menos na primeira componente principal.
# isto não é algo que poderíamos notar imediatamente a partir dos dados originais.
