#----------------------------------------------------------------#
# INF-0611 Recuperacao de Informacao                             #
#                                                                #
# Trabalho Avaliativo 2                                          #
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes do grupo:                        #
# -                                                              #
# -                                                              #
# -                                                              #
#                                                                #
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Configuracao dos arquivos auxiliares   
#----------------------------------------------------------------#
# configure o caminho antes de executar
setwd("C:\\Users\\Eric\\Documents\\GitHub\\mineiracao_dados_complexos\\Recuperacao de Informacao\\Atividade 2\\") 
source("./ranking_metrics.R")
source("./trabalho2_base.R")

# caminho da pasta de imagens
path_plantas = './plantas'

#----------------------------------------------------------------#
# Leitura das imagens                 
#----------------------------------------------------------------#
imagens <- read_images(path_plantas)

#----------------------------------------------------------------#
# Obtem classe de cada imagem             
#----------------------------------------------------------------#
nome_classes <- get_classes(path_plantas)

#----------------------------------------------------------------#
# obtem ground_truth para cada classe 
#----------------------------------------------------------------#
ground_truth_biloba <- get_ground_truth(path_plantas, nome_classes, 'biloba')
ground_truth_europaea <- get_ground_truth(path_plantas, nome_classes, 'europaea')
ground_truth_ilex <- get_ground_truth(path_plantas, nome_classes, 'ilex')
ground_truth_monogyna <- get_ground_truth(path_plantas, nome_classes, 'monogyna')
ground_truth_regia <- get_ground_truth(path_plantas, nome_classes, 'regia')



#----------------------------------------------------------------#
# Questao 1                               
#----------------------------------------------------------------#

# obtem caracteristicas de cor  
hist_cor_desc <- function(img){
  r <- hist(img[,,1]*255, plot=FALSE, breaks=0:255)$counts
  g <- hist(img[,,2]*255, plot=FALSE, breaks=0:255)$counts
  b <- hist(img[,,3]*255, plot=FALSE, breaks=0:255)$counts
  return(c(r, g, b))
}

# obtem caracteristicas de textura   
lbp_desc <- function(img){
  r1 <- lbp(img[,,1,1],1)
  lbp_uniforme <- hist(r1$lbp.u2, plot=FALSE, breaks=59)$counts
  return(lbp_uniforme)
}

# obtem caracteristicas de forma 
Momentos <-function(img){
  
  centroide <- function(M) {
    c(momento(M, 1, 0) / momento(M, 0, 0),
      momento(M, 0, 1) / momento(M, 0, 0))
  }
  
  momento <- function(M, p, q, central = FALSE) {
    r <- 0
    if (central) {
      c <- centroide(M)
      x <- c[1]
      y <- c[2]
    } else {
      x <- 0
      y <- 0
    }
    for (i in 1:nrow(M))
      for (j in 1:ncol(M))
        r <- r + (i - x)^p * (j - y)^q * M[i,j]  
    return(r)
  }
  
  area <- function(M) {
    momento(M, 0, 0, central = TRUE)
  }
  
  assimetria <- function(M) {
    momento(M, 3, 3, central = TRUE)
  }

  curtose <- function(M) {
    momento(M, 4, 4, central = TRUE)
  }
  
  aux <- img[,,1,1]
  
  m00 <- area(aux)
  m33 <- assimetria(aux)
  m44 <- curtose(aux)
  
  return(c(m00, m33, m44))
}

#----------------------------------------------------------------#
# obtem caracteristicas de cor, textura e forma para todas as imagens e 
# armazena em matrizes onde uma linha representa uma imagem 

gray_img <- sapply(imagens, grayscale)

features_c <- t(sapply(imagens, hist_cor_desc))
rownames(features_c) <- names(imagens)

features_t <- t(sapply(gray_img, lbp_desc))
rownames(features_t) <- names(gray_img)

features_s <- t(sapply(gray_img, Momentos))
rownames(features_s) <- names(gray_img)

#----------------------------------------------------------------#
# Questao 2                               
#----------------------------------------------------------------#

# definindo as consultas
# obs.:  use o caminho completo para a imagem
consulta_biloba <- "./plantas/biloba_02.jpg"
consulta_europaea <- "./plantas/europaea_01.jpg"
consulta_ilex <- "./plantas/ilex_08.jpg"
consulta_monogyna <- "./plantas/monogyna_04.jpg"
consulta_regia <- "./plantas/regia_07.jpg"

# visualizando as consultas
par(mfrow = c(3,3), mar = rep(2, 4))
mostrarImagemColorida(consulta_biloba, consulta_biloba)
mostrarImagemColorida(consulta_europaea, consulta_europaea)
mostrarImagemColorida(consulta_ilex, consulta_ilex)
mostrarImagemColorida(consulta_monogyna, consulta_monogyna)
mostrarImagemColorida(consulta_regia, consulta_regia)

#-----------------------------#
# construindo rankings                          
# para cada uma das 5 consultas, construa um ranking com base na cor

ranking_c_biloba <-   get_ranking_by_distance(features_c, consulta_biloba, method="euclidean")
ranking_c_europaea <- get_ranking_by_distance(features_c, consulta_europaea, method="euclidean")
ranking_c_ilex <-     get_ranking_by_distance(features_c, consulta_ilex, method="euclidean")
ranking_c_monogyna <- get_ranking_by_distance(features_c, consulta_monogyna, method="euclidean")
ranking_c_regia <-    get_ranking_by_distance(features_c, consulta_regia, method="euclidean")

# para cada uma das 5 consultas, construa um ranking com base na textura
ranking_t_biloba <-   get_ranking_by_distance(features_t, consulta_biloba, method="euclidean")
ranking_t_europaea <- get_ranking_by_distance(features_t, consulta_europaea, method="euclidean")
ranking_t_ilex <-     get_ranking_by_distance(features_t, consulta_ilex, method="euclidean")
ranking_t_monogyna <- get_ranking_by_distance(features_t, consulta_monogyna, method="euclidean")
ranking_t_regia <-    get_ranking_by_distance(features_t, consulta_regia, method="euclidean")
  
# para cada uma das 5 consultas, construa um ranking com base na forma
ranking_s_biloba <-   get_ranking_by_distance(features_s, consulta_biloba, method="euclidean")
ranking_s_europaea <- get_ranking_by_distance(features_s, consulta_europaea, method="euclidean")
ranking_s_ilex <-     get_ranking_by_distance(features_s, consulta_ilex, method="euclidean")
ranking_s_monogyna <- get_ranking_by_distance(features_s, consulta_monogyna, method="euclidean")
ranking_s_regia <-    get_ranking_by_distance(features_s, consulta_regia, method="euclidean")

#-----------------------------#
# comparando  rankings                              

## utilize as funções do arquivo ranking_metrics.R para calcular 
# a precisão, revocação, taxa F1 e precisão média nos 
# top 5, 10, 15 e 20

analyse_rankings <- function(ranking, ground_truth) {
  top_k <- c(5, 10, 15, 20)
  
  resultados <- c()
  
  for (k in top_k){
    precisao <- precision(ground_truth, ranking, k)
    revocacao <-  recall(ground_truth, ranking, k)
    f1 <- f1_score(ground_truth, ranking, k)
    avg_prec <- ap(ground_truth, ranking, k)
    resultados <- rbind(resultados, c(k, precisao, revocacao, f1, avg_prec))
    
  }
  df <- data.frame(resultados)
  colnames(df) <- c('k', 'precisao', 'revocacao', 'f1', 'avg_prec')
  return(df)
}



# analisando rankings gerados com caracteristicas de cor
analyse_rankings(ranking_c_biloba, ground_truth_biloba)
analyse_rankings(ranking_c_europaea, ground_truth_europaea)
analyse_rankings(ranking_c_ilex, ground_truth_ilex)
analyse_rankings(ranking_c_monogyna, ground_truth_monogyna)
analyse_rankings(ranking_c_regia, ground_truth_regia)

# analisando rankings gerados com caracteristicas de textura
analyse_rankings(ranking_t_biloba, ground_truth_biloba)
analyse_rankings(ranking_t_europaea, ground_truth_europaea)
analyse_rankings(ranking_t_ilex, ground_truth_ilex)
analyse_rankings(ranking_t_monogyna, ground_truth_monogyna)
analyse_rankings(ranking_t_regia, ground_truth_regia)

# analisando rankings gerados com caracteristicas de forma
analyse_rankings(ranking_s_biloba, ground_truth_biloba)
analyse_rankings(ranking_s_europaea, ground_truth_europaea)
analyse_rankings(ranking_s_ilex, ground_truth_ilex)
analyse_rankings(ranking_s_monogyna, ground_truth_monogyna)
analyse_rankings(ranking_s_regia, ground_truth_regia)


# Analise de dados
plot_prec_e_rev(ranking_c_biloba, ground_truth_biloba, 20, 'Ranking Cor')
plot_prec_e_rev(ranking_t_biloba, ground_truth_biloba, 20, 'Ranking Cor')
plot_prec_e_rev(ranking_s_biloba, ground_truth_biloba, 20, 'Ranking Cor')

mostrarGrayscale <- function(path_img, nome=''){
  path_img <- as.character(path_img)
  img <- load.image(path_img)
  return(plot(grayscale(img), axes = FALSE, main = nome))
}

lbp_desc <- function(img){
  r1 <- lbp(img[,,1,1],1)
  lbp_uniforme <- hist(r1$lbp.u2, plot=True, breaks=59)$counts
  return(lbp_uniforme)
}

for (img_name in ranking_c_biloba[1:20]){
  mostrarImagemColorida(img_name, img_name)
}

for (img_name in ranking_t_biloba[1:20]){
  mostrarGrayscale(img_name, img_name)
}

par(mfrow = c(4,5), mar = rep(2, 4))
for (img_name in ranking_s_biloba[30:50]){
  mostrarGrayscale(img_name, img_name)
}


par(mfrow = c(4,5), mar = rep(2, 4))
for (img_name in ranking_t_biloba[1:20]){
  path_img <- as.character(img_name)
  img <- load.image(path_img)
  gsc <- grayscale(img)
  r1 <- lbp(gsc[,,1,1],1)
  lbp_uniforme <- hist(r1$lbp.u2, main=img_name, plot=TRUE, breaks=59)$counts
}
#----------------------------------------------------------------#
# Questao 2 - RESPONDA:                   
# (a) Escolha uma consulta para analisar mais detalhadamente e
# responda: Para essa consulta qual descritor retornou o melhor
# ranking? Lembre-se de analisar visualmente as imagens da classe,
# contextualizando o que foi extraído em cada descritor. Também
# aponte pontos fortes e fracos dos descritores usados que podem
# justificar esse comportamento.
#                                         
#                                         
#                                         
#                                         
#                                         
#                                         
# (b) Considerando as 5 consultas definidas, calcule a m?dia das precis?es m?dias em top 10. 
# Avaliando por essa medida, qual descritor obteve melhores resultados? Justifique. 
# Lembre-se que para justificar sua resposta, voc? pode complementar sua an?lise usando 
# tamb?m outras medidas de avalia??o de ranking adicionais vistas na Aula 1, caso seja pertinente
#                                         
#                                         
#                                         
#                                         
#                                         
#                                         
#                                         
#                                         
#                                         
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Questao 3
#----------------------------------------------------------------#
# concatenando caracteristicas                      

## obter vetores finais de caracteristicas pela concatenação de 
# cada tipo de caracteristica (cor, textura e forma):
features_concat = cbind(features_c, features_t, features_t)
  
# gerar novos rankings
ranking_concat_biloba <-  get_ranking_by_distance(features_concat, consulta_biloba, method="euclidean")
ranking_concat_europaea <- get_ranking_by_distance(features_concat, consulta_europaea, method="euclidean") 
ranking_concat_ilex <- get_ranking_by_distance(features_concat, consulta_ilex, method="euclidean") 
ranking_concat_monogyna <- get_ranking_by_distance(features_concat, consulta_monogyna, method="euclidean") 
ranking_concat_regia <- get_ranking_by_distance(features_concat, consulta_regia, method="euclidean") 

# analisando rankings gerados com caracteristicas concatenadas
analyse_rankings(ranking_concat_biloba, ground_truth_biloba)
analyse_rankings(ranking_concat_europaea, ground_truth_europaea)
analyse_rankings(ranking_concat_ilex, ground_truth_ilex)
analyse_rankings(ranking_concat_monogyna, ground_truth_monogyna)
analyse_rankings(ranking_concat_regia, ground_truth_regia)

#----------------------------------------------------------------#
# Questao 3 - RESPONDA:  
# (a) Qual o impacto dessas alterações nas medidas de avaliação
# calculadas?
# 
# 
# (b) Os descritores combinados apresentaram melhores resultados?
# Justifique sua resposta.
# 
# 
# 
# 
# (c) Você acredita que algum dos descritores apresentou maior
# influência na combinação? Justifique sua resposta.
# 
# 
# 
#----------------------------------------------------------------#
