#----------------------------------------------------------------#
# INF-0611 Recuperacao de Informacao       
#                       
# Trabalho Avaliativo 3 
#----------------------------------------------------------------#
# Nome COMPLETO dos integrantes dp grupo:  
# - Daniel Noriaki Kurosawa                                        
# - Eric Uyemura Suda                                       
# - Fernando Shigeru Wakabayashi                                        
# 
#----------------------------------------------------------------#

#----------------------------------------------------------------#
# Configuracao dos arquivos auxiliares 
#----------------------------------------------------------------#
# configure o caminho antes de executar
setwd("C:\\Users\\Eric\\Documents\\GitHub\\mineiracao_dados_complexos\\Recuperacao de Informacao\\Atividade 3\\") 
source("./ranking_metrics.R")
source("./trabalho3_base.R")

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
ground_truth_biloba <- get_ground_truth(path_plantas, nome_classes, "biloba")
ground_truth_europaea <- get_ground_truth(path_plantas, nome_classes, "europaea")
ground_truth_ilex <- get_ground_truth(path_plantas, nome_classes, "ilex")
ground_truth_monogyna <- get_ground_truth(path_plantas, nome_classes, "monogyna")
ground_truth_regia <- get_ground_truth(path_plantas, nome_classes, "regia")

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
  img <- grayscale(img)
  r1 <- lbp(img[,,1,1],1)
  lbp_uniforme <- hist(r1$lbp.u2, plot=FALSE, breaks=59)$counts
  return(c(lbp_uniforme))
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

  img <- grayscale(img)[,,1,1]
  features <-NULL
  
  for(i in c(0,3,4)){
    features <- cbind(features,momento(img, i,i, central=TRUE))
  }
  return(features)
}


#----------------------------------------------------------------#
# obtem características de cor, textura e forma  
# para todas as imagens e armazena em matrizes 
# onde uma linha e uma imagem 
features_c <- t(sapply(imagens, hist_cor_desc))
rownames(features_c) <- names(imagens)
features_t <-t(sapply(imagens, lbp_desc))
rownames(features_t) <- names(imagens)
features_s <- t(sapply(imagens, Momentos))
rownames(features_s) <- names(imagens)


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

# analisando rankings
analyse_rankings <- function(ranking, ground_truth) {
  resultados <- c()
  top_k = seq(5,20,5)
  for(k in top_k){
    prec <-precision(ground_truth, ranking, k) 
    rec <-recall(ground_truth, ranking, k) 
    f1 <-f1_score(ground_truth, ranking, k)
    avg_prec <-ap(ground_truth, ranking, k)
    resultados <- rbind(resultados, c(k, prec, rec, f1, avg_prec))
  }
  df <- data.frame(resultados)
  colnames(df) <- c('k', 'precisao', 'revocacao', 'f1', 'avg_prec')
  return(df)
}


# criando descritor concatenando 
desc_all <- cbind(features_c,features_t,features_s)
# criando rankings com descrito concatenado
ranking_concat_biloba <- get_ranking_by_distance(desc_all, consulta_biloba, method="euclidean")
ranking_concat_europaea <- get_ranking_by_distance(desc_all, consulta_europaea, method="euclidean")


# analisando os rankings 
analyse_rankings(ranking_concat_biloba, ground_truth_biloba)
analyse_rankings(ranking_concat_europaea, ground_truth_europaea)


#----------------------------------------------------------------#
# Questao 3 
#----------------------------------------------------------------#

# calculando as distancias, descritor:  histograma de cor 
dist_hist_biloba <- get_distance_vector(features_c, consulta_biloba, method="euclidean") 
dist_hist_europaea <- get_distance_vector(features_c, consulta_europaea, method="euclidean")

# calculando as distancias, descritor:  textura 
dist_text_biloba <- get_distance_vector(features_t, consulta_biloba, method="euclidean")
dist_text_europaea <- get_distance_vector(features_t, consulta_europaea, method="euclidean") 

# calculando as distancias, descritor:  forma 
dist_forma_biloba <- get_distance_vector(features_s, consulta_biloba, method="euclidean")
dist_forma_europaea <- get_distance_vector(features_s, consulta_europaea, method="euclidean") 

# calculando e analisando  rankings combmin
r_combmin_biloba <- names(imagens)[combmin(dist_hist_biloba, dist_text_biloba, dist_forma_biloba)]
r_combmin_europaea <-  names(imagens)[combmin(dist_hist_europaea, dist_text_europaea, dist_forma_europaea)]

analyse_rankings(r_combmin_biloba, ground_truth_biloba)
analyse_rankings(r_combmin_europaea, ground_truth_europaea)


# calculando e analisando  rankings combmax
r_combmax_biloba <- names(imagens)[combmax(dist_hist_biloba, dist_text_biloba, dist_forma_biloba)]
r_combmax_europaea <-  names(imagens)[combmax(dist_hist_europaea, dist_text_europaea, dist_forma_europaea)]

analyse_rankings(r_combmax_biloba, ground_truth_biloba)
analyse_rankings(r_combmax_europaea, ground_truth_europaea)

# calculando e analisando  rankings combsum
r_combsum_biloba <- names(imagens)[combsum(dist_hist_biloba, dist_text_biloba, dist_forma_biloba)]
r_combsum_europaea <-  names(imagens)[combsum(dist_hist_europaea, dist_text_europaea, dist_forma_europaea)]

analyse_rankings(r_combsum_biloba, ground_truth_biloba)
analyse_rankings(r_combsum_europaea, ground_truth_europaea)

# calculando e analisando  rankings borda
r_bordacount_biloba <- names(imagens)[bordacount(dist_hist_biloba, dist_text_biloba, dist_forma_biloba)]
r_bordacount_europaea <-  names(imagens)[bordacount(dist_hist_europaea, dist_text_europaea, dist_forma_europaea)]

analyse_rankings(r_bordacount_biloba, ground_truth_biloba)
analyse_rankings(r_bordacount_europaea, ground_truth_europaea)

#--------------------------------------------------------#
# Codigos Auxiliares
# Consulta escolhida - biloba
#--------------------------------------------------------#
combmin_ranking_bilo <- analyse_rankings(r_combmin_biloba, ground_truth_biloba);combmin_ranking_bilo
combmax_ranking_bilo <- analyse_rankings(r_combmax_biloba, ground_truth_biloba);combmax_ranking_bilo
combsum_ranking_bilo <- analyse_rankings(r_combsum_biloba, ground_truth_biloba);combsum_ranking_bilo
bordacount_ranking_bilo <- analyse_rankings(r_bordacount_biloba, ground_truth_biloba);bordacount_ranking_bilo

combmin_ranking_eur <- analyse_rankings(r_combmin_europaea, ground_truth_europaea)
combmax_ranking_eur <- analyse_rankings(r_combmax_europaea, ground_truth_europaea)
combsum_ranking_eur <- analyse_rankings(r_combsum_europaea, ground_truth_europaea)
bordacount_ranking_eur <- analyse_rankings(r_bordacount_europaea, ground_truth_europaea)

plot_prec_e_rev(r_combmin_biloba, ground_truth_biloba, 20, 'Ranking Combmin biloba')
plot_prec_e_rev(r_combmax_biloba, ground_truth_biloba, 20, 'Ranking Combmax biloba')
plot_prec_e_rev(r_combsum_biloba, ground_truth_biloba, 20, 'Ranking Combsum biloba')
plot_prec_e_rev(r_bordacount_biloba, ground_truth_biloba, 20, 'Ranking Borda Count biloba')

rbind(dist_hist_biloba, dist_text_biloba, dist_forma_biloba)

par(mfrow = c(5,2), mar = rep(2, 4))
gtruth_pattern='/biloba_'
mask=grep(gtruth_pattern,names(ground_truth_biloba))
for (img_name in names(ground_truth_biloba[mask])){
  mostrarImagemColorida(img_name, img_name)
}

# imagens dos primeiros 20 elementos do combmin
par(mfrow = c(4,5), mar = rep(2, 4))
for (img_name in r_combmin_biloba[1:20]){
  mostrarImagemColorida(img_name, img_name)
}

# imagens dos primeiros 20 elementos do combmax
par(mfrow = c(4,5), mar = rep(2, 4))
for (img_name in r_combmax_biloba[1:20]){
  mostrarImagemColorida(img_name, img_name)
}

# imagens dos primeiros 20 elementos do combsum
par(mfrow = c(4,5), mar = rep(2, 4))
for (img_name in r_combsum_biloba[1:20]){
  mostrarImagemColorida(img_name, img_name)
}

# imagens dos primeiros 20 elementos do bordacount
par(mfrow = c(4,5), mar = rep(2, 4))
for (img_name in r_bordacount_biloba[1:20]){
  mostrarImagemColorida(img_name, img_name)
}

# Empilhando as consultas para avaliar como as combinacoes se comportam por imagem recuperada
empilhado <- rbind(r_combmin_biloba, r_combmax_biloba, r_combsum_biloba, r_bordacount_biloba)

# Media das precisoes medias

media_precisoes_med_bilo <- rbind(mean(combmin_ranking_bilo$avg_prec)
                                  ,mean(combmax_ranking_bilo$avg_prec)
                                  ,mean(combsum_ranking_bilo$avg_prec)
                                  ,mean(bordacount_ranking_bilo$avg_prec))

media_precisoes_med_bilo <-  cbind(c('combmin', 'combmax', 'combsum', 'bordacount'), media_precisoes_med_bilo)
colnames(media_precisoes_med_bilo) <- c('agreg', 'MAP'); media_precisoes_med_bilo

media_precisoes_med_eur <- rbind(mean(combmin_ranking_eur$avg_prec)
                                  ,mean(combmax_ranking_eur$avg_prec)
                                  ,mean(combsum_ranking_eur$avg_prec)
                                  ,mean(bordacount_ranking_eur$avg_prec))
 
media_precisoes_med_eur <- cbind(c('combmin', 'combmax', 'combsum', 'bordacount'), media_precisoes_med_eur)
colnames(media_precisoes_med_eur) <- c('agreg', 'MAP'); media_precisoes_med_eur
#----------------------------------------------------------------#
# Questao 3 - RESPONDA:                   
# (i) 
# Query utilizada - consulta_biloba
# Observando a precisao media, dentro do intervalo considerado com k ate 20, vemos que a agregacao 
# via combmax retorna a maior precisao media (ap=1) em k=9 porem nao consegue chegar em revocacao=1, 
# o que acontece com as demais agregacoes, porem as outras agregacoes possuem uma precisao media 
# menor que o combmax.
# 
# A agregacao combmax nao chega a revocacao pois traz a folha de biloba_01 apenas na posicao 48 
# por causa que a distancia calculada do vetor de forma (dist = 1.0) foi muito maior do que a 
# distancia de textura (0.169) e de cor (0.0863). 
#
# Notamos tambem que as folhas de biloba associam-se fortemente a cor em todos as
# comparacoes, favorecendo o combmin e o combsum.

# (j) 
# Para a planta biloba o melhor agregador foi o combmax com MAP = 1 porem nao esta muito atras
# do combsum (MAP = 0.9885) e bordacount (MAP = 0.9309).
# Ja para a folha europaea o combmax foi o unico que nao atingiu MAP = 1, com todos os outros
# agregadores (combmin, combsum e bordacount) empatados com MAP = 1.
# 
# 
# 
#----------------------------------------------------------------#

# Analises Questao 4

# Biloba - Analise concatenado vs combsum
analyse_rankings(ranking_concat_biloba, ground_truth_biloba)
analyse_rankings(r_combsum_biloba, ground_truth_biloba)

plot_prec_e_rev(ranking_concat_biloba, ground_truth_biloba, 20, 'Ranking Contatenado biloba')
plot_prec_e_rev(r_combsum_biloba, ground_truth_biloba, 20, 'Ranking Combsum biloba')


# Europaea - Analise concatenado vs combsum
analyse_rankings(ranking_concat_europaea, ground_truth_europaea)
analyse_rankings(r_combsum_europaea, ground_truth_europaea)

plot_prec_e_rev(ranking_concat_europaea, ground_truth_europaea, 20, 'Ranking Contatenado europaea')
plot_prec_e_rev(r_combsum_europaea, ground_truth_europaea, 20, 'Ranking Combsum europaea')


# Analise visual das imagens - Consulta Biloba
# descritores concatenados
par(mfrow = c(2,3), mar = rep(2, 4))
for (img_name in ranking_concat_biloba[1:6]){
  mostrarImagemColorida(img_name, img_name)
}
# Agregacao via combsum
par(mfrow = c(2,3), mar = rep(2, 4))
for (img_name in r_combmax_biloba[1:6]){
  mostrarImagemColorida(img_name, img_name)
}

# Analise visual das imagens - Consulta Europaea
# descritores concatenados
par(mfrow = c(2,3), mar = rep(2, 4))
for (img_name in ranking_concat_europaea[1:6]){
  mostrarImagemColorida(img_name, img_name)
}
# Agregacao via combsum
par(mfrow = c(2,3), mar = rep(2, 4))
for (img_name in r_combsum_europaea[1:6]){
  mostrarImagemColorida(img_name, img_name)
}


# Melhor metodo em top 10

# Funcao para retornar o MAP por descritor selecionado
get_map <- function(feature){
  k=5
  # Ranking Concatenado
  vetor_consultas <- c(
    consulta_biloba,
    consulta_europaea,
    consulta_ilex,
    consulta_monogyna,
    consulta_regia
  )
  vetor_groundtruth <- list(
    ground_truth_biloba,
    ground_truth_europaea,
    ground_truth_ilex,
    ground_truth_monogyna,
    ground_truth_regia
  )
  avg_p = 0
  for (i in 1:5){
    ranking <- get_ranking_by_distance(feature, vetor_consultas[i], method='euclidean')
    avg_p <- analyse_rankings(ranking, vetor_groundtruth[[i]])[2,5] + avg_p
  }
  return(avg_p/k)
}

# Funcao para retornar o MAP o metodo de agregacao combmax
get_map_combmax <- function(feature_c, feature_t, feature_s){
  k=5
  # Ranking Concatenado
  vetor_consultas <- c(
    consulta_biloba,
    consulta_europaea,
    consulta_ilex,
    consulta_monogyna,
    consulta_regia
  )
  vetor_groundtruth <- list(
    ground_truth_biloba,
    ground_truth_europaea,
    ground_truth_ilex,
    ground_truth_monogyna,
    ground_truth_regia
  )
  avg_p = 0
  for (i in 1:5){
    dist_c <- get_distance_vector(feature_c, vetor_consultas[i], method='euclidean')
    dist_t <- get_distance_vector(feature_t, vetor_consultas[i], method='euclidean')
    dist_s <- get_distance_vector(feature_s, vetor_consultas[i], method='euclidean')
    
    r_combmax <- names(imagens)[combmax(dist_c, dist_t, dist_s)]
    
    avg_p <- analyse_rankings(r_combmax, vetor_groundtruth[[i]])[2,5] + avg_p
  }
  return(avg_p/k)
}

# MAPs para cada metodo selecionado
get_map(features_c)
get_map(features_t)
get_map(features_s)
get_map(desc_all)
get_map_combmax(features_c, features_t, features_s)

#----------------------------------------------------------------#
# Questao 4 - RESPONDA:                   
# (i) 
# Para a consulta da folha biloba podemos ver que a precisao e revocacao nos k=5, 10, 15, 20
# da consulta por combsum apresenta valors superiores em todos os pontos quando comparado a 
# concatenacao dos descritores. Alem disso pelo grafico de Precisao e Revocacao por k podemos observar
# como a consulta por combsum recupera os itens de forma mais rapida e tambem chega a revocacao = 1 em k=16.
# Logo para esta consulta a agregacao dos descritores foi mais efetiva que a concatenacao dos descritores. 
#
# Para a consulta da folha europaea, as metricas de precisao e revocacao dos rankings, bem como a precisao
# media possuem os mesmos valores independente do k utilizado.O grafico de Precisao e Revocacao por k
# apresenta exatamente o mesmo comportamento chegando em uma revocacao = 1 em k = 10. Logo nao ha diferenca
# entre os descritores serem cocatenados ou utilizar a agregacao dos rankings. Notamos apenas que a ordem
# de recuperacao das folhas varia entre as duas buscas.
# 
# (ii) 
# Analisando os dois conjunto de imagens para a consulta da folha biloba, podemos ver que a consulta atraves
# do metodo de agregacao (combmax) foi mais efeciente por trazer apenas folhas de biloba com as mesmas caracteristicas
# visuais de tamanho e de cor. Ja a consulta via agregacao de descritores recuperou apenas o primeiro resultado 
# correto de biloba(ela mesma), recuperando  ilex, monogyna e regia respectivamente. Visualmente falando 
# os descritores de forma foram ignorados pois trouxeram resultados de ilex e regia e o os descritores
# de cor tambem, pois todas as folhas apresentam um tom de verde mais esturo que a folha biloba.
#
# (iii)
# Considerando que por interessante seria a recuperacao de todas as imagens de forma precisa e rapida.
# Para o caso da consulta da folha biloba, o melhor agregador (combmax) traz tambem o conjunto mais interessante
# de folhas recuperadas, visto que recupera 9 folhas consecutivas de biloba ate k=9.
# 
# (iv)
#  Conforme o calculado nas linhas 309 e 403, o melhor metodo de recuperacao para todos os tipos de folhas em k=10 
# levando em consideracao o MAP foi o combmax com um MAP=0.9134. 
#----------------------------------------------------------------#