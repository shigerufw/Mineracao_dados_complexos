
library(wvtool)
library(imager)
library(tidyverse)

#------------------------------------------------#
# Carrega e retorna todas as imagens             #
#------------------------------------------------#
read_images <- function(path_soccer){
  name_imgs <- list.files(path_soccer, full.names = TRUE)
  all_im <- lapply(name_imgs, load.image)
  names(all_im) <- name_imgs
  return(all_im)
}

#------------------------------------------------#
# Exibe a classe de cada imagem (time)           #
#------------------------------------------------#
get_classes<- function(path_soccer){
  name_imgs <- list.files(path_soccer, full.names = FALSE)
  classes<-NULL
  for(name in name_imgs){
    name<-strsplit(name, '_')
    classes<-cbind(classes,name[[1]][1])
  }
  return(classes)
}

#------------------------------------------------#
# Retorna ground_truth escolhida classe relevante#
#------------------------------------------------#
get_ground_truth<- function(path_soccer, classes, classe_relevante){
  ground_truth <- integer(length(classes))
  ground_truth[which(classes %in% classe_relevante)] <-1
  names(ground_truth) <- list.files(path_soccer, full.names = TRUE)
  return(ground_truth)
}


#-----------------------------------------------------#
# Retorna as distâncias entre uma consulta e as       #
# demais imagens com base na feature fornecida        #
#-----------------------------------------------------#
get_distance_vector <- function(M, query, method="euclidean") {
  distancias <- lapply(rownames(M), 
                     function(x) dist(M[c(query, x),], 
                                      method = method))
  distancias <- unlist(distancias)
  names(distancias) <- rownames(M)
  distancias <- (distancias - min(distancias)) /
                (max(distancias) - min(distancias))
  return(distancias)
}

get_distance_color_moment <- function(M, w, query) {
  moment_distance <- function(img_a, img_b, w) {
    return (sum(abs(img_a-img_b) * w))
  }
  d <- lapply(rownames(M), function(x) moment_distance(M[query,], M[x,], w))
  d <- unlist(d)
  names(d) <- rownames(M)
  d <- (d - min(d)) / (max(d) - min(d))
  return(d)
}



#-----------------------------------------------------#
# Retorna caminhos das imagens ordenadas por distancia#
#-----------------------------------------------------#
get_ranking_by_distance <- function(M, query, method="euclidean"){
  distancias <- get_distance_vector(M, query, method)
  return(names(sort(distancias)))
}
get_ranking_by_color_moment <- function(M, w, query){
  distancias <- get_distance_color_moment(M, w, query)
  return(names(sort(distancias)))
}


#-----------------------------------------------------#
# Exibe imagem                                        #
#-----------------------------------------------------#
mostrarImagemColorida <- function(path_img, nome=''){
  path_img <- as.character(path_img)
  img <- load.image(path_img)
  return(plot(img, axes = FALSE, main = nome))
}


#-----------------------------------------------------#
# Gráfico precisao e revocacao                        #
#-----------------------------------------------------#
plot_prec_e_rev <- function(ranking, groundtruth, k, text) {
  # Calculando a precisão com a função precision para 
  # cada valor de 1 ate k e armazenando no vetor p
  p <- mapply(precision, 1:k, MoreArgs = list(
    gtruth = groundtruth, ranking = ranking))
  # Calculando a revocação com a função recall para 
  # cada valor de 1 ate k e armazenando no vetor r
  r <- mapply(recall, 1:k, MoreArgs = list(
    gtruth = groundtruth, ranking = ranking))
  
  # Criando um data.frame com k linhas e duas colunas 
  # que armazenam os valores de precisão (prec) e 
  # revocação (rev)
  pr <- data.frame(prec = p, rec = r)
  # Criando plot a partir data.frame pr
  ggplot(pr, aes(x = 1:k)) + 
    # Adicionado linha e pontos referentes a precisão 
    geom_point(aes(y = prec, colour = "Precisão")) + 
    geom_line(aes(y = prec, colour = "Precisão")) +
    # Adicionado linha e pontos referentes a revocação 
    geom_point(aes(y = rec, colour = "Revocação")) + 
    geom_line(aes(y = rec, colour = "Revocação")) +
    # Definindo um tema com configurações básicas de 
    # visualização do gráfico a ser gerado
    theme_light() +
    # Definindo conteúdo do título do gráfico
    labs(colour = element_blank(), 
         title = paste("Precisão e Revocação X Top k", text)) +
    theme(plot.title = element_text(hjust = 0.5)) +
    # Configurando a legenda
    theme(legend.box.background = 
            element_rect(colour = "black")) +
    # Configurando o eixo x com legenda e número 
    # de marcações
    scale_x_continuous(name = "Top k", 
                       limits = c(1, k), 
                       breaks = 5 * 1:(k/5), 
                       minor_breaks = NULL) +
    # Configurando o eixo y com legenda e número 
    # de marcações
    scale_y_continuous(name = "", limits = c(0, 1), 
                       breaks = 0.1 * 0:10, 
                       minor_breaks = NULL)
}

#------------------------------------------------#
#                     BORDA                      #
#------------------------------------------------#
bordacount <- function(...) {
  # obtem os rankings
  rankings <- lapply(list(...), rank)
  # calcula a ordem baseada na soma 
  # das posições dos rankings
  return(do.call(combsum, rankings))
}


#------------------------------------------------#
#                   COMBMIN                      #
#------------------------------------------------#
combmin <- function(...) {
  order(mapply(min, ...))
}

#------------------------------------------------#
#                   COMBMAX                      #
#------------------------------------------------#
combmax <- function(...) {
  order(mapply(max, ...))
}


#------------------------------------------------#
#                   COMBSUM                      #
#------------------------------------------------#
combsum <- function(...) {
  order(mapply(sum, ...))
}


