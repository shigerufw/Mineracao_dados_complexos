####################################################
#  MDC010 - Aprendizado Supervisionado I           #
#  Ex02 - Código de Apoio para Data Cleaning       #
#################################################### 



#### Toma as features que tem uma quantidade de valores faltantes (NA) abaixo
#### de um threshold. Assim podemos substituir esses valores faltantes pela 
#### média dos valores presentes.
#### Features com muitos valores faltantes podem
#### fornecer uma estimativa de média muito precária. Assim selecionamos
#### apenas aquelas com uma quantidade mínima de valores 
#### para cálculo de média.
handleNA <- function(data, threshold){
    t <- 0
    features_idx <- c()
    for(i in 1:ncol(data)){
        na_positions <- is.na(data[,i])
        ratio <- sum(na_positions)/nrow(data)
        if(ratio < threshold){
            t <- t + 1
            mean_value <- mean(data[,i], na.rm = TRUE)
            data[is.na(data[,i]), i] <- mean_value
            features_idx[t] <- i
        }
    }
    return(list(data,features_idx))
}


### Elimina os N maiores e N menores valores de uma feature. Dependendo
### da distribuição da feature, eliminaremos outliers tanto abaixo quanto
### a cima da distribuição. Essa função retorna os índices dos exemplos
### que não serão eliminados (não estão nos extremos).
filterExtremes <- function(data, n){
    
    s <- rep(TRUE, length(data))
    sorted_data <- sort(data)
    min_values <- c(sorted_data[1:n])
    max_values <- c(sorted_data[(length(sorted_data)-n+1):length(sorted_data)])
    
    #extreme_values <- c(min_values, max_values)
    # Se comentar a linha abaixo e descomentar a de cima, teremos
    # uma filtragem que elimina apenas os N maiores valores e os N menores
    # valores. Abaixo elimina apenas os N maiores valores. Se substituíssemos
    # "max_values" por "min_valures", o comando eliminaria os N menores 
    # valores. 
    extreme_values <- max_values
    
    
    idxs <- match(extreme_values, data)
    s[idxs] <- FALSE
    return(s)
}

### Função que remove os outliers considerando a análise de todas
### as features. Assim, permanecem apenas os exemplos que não apresentam 
### valores extremos em nenhuma de suas features. Ou seja, exemplos que 
### tendem a ser bem comportados ao longo de todos os atributos.
removeOutliers <- function(data, n){
   
    sample_selected <- rep(TRUE, dim(data)[1])
    for (i in 1:ncol(data)){
        
        s <- filterExtremes(data[,i], n)
        sample_selected <- s & sample_selected
        
    }
    print(sum(sample_selected))
    data <- data[sample_selected,]
}
