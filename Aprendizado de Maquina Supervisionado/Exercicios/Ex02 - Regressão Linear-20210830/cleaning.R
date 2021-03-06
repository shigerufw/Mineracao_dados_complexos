####################################################
#  MDC010 - Aprendizado Supervisionado I           #
#  Ex02 - C�digo de Apoio para Data Cleaning       #
#################################################### 



#### Toma as features que tem uma quantidade de valores faltantes (NA) abaixo
#### de um threshold. Assim podemos substituir esses valores faltantes pela 
#### m�dia dos valores presentes.
#### Features com muitos valores faltantes podem
#### fornecer uma estimativa de m�dia muito prec�ria. Assim selecionamos
#### apenas aquelas com uma quantidade m�nima de valores 
#### para c�lculo de m�dia.
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
### da distribui��o da feature, eliminaremos outliers tanto abaixo quanto
### a cima da distribui��o. Essa fun��o retorna os �ndices dos exemplos
### que n�o ser�o eliminados (n�o est�o nos extremos).
filterExtremes <- function(data, n){
    
    s <- rep(TRUE, length(data))
    sorted_data <- sort(data)
    min_values <- c(sorted_data[1:n])
    max_values <- c(sorted_data[(length(sorted_data)-n+1):length(sorted_data)])
    
    #extreme_values <- c(min_values, max_values)
    # Se comentar a linha abaixo e descomentar a de cima, teremos
    # uma filtragem que elimina apenas os N maiores valores e os N menores
    # valores. Abaixo elimina apenas os N maiores valores. Se substitu�ssemos
    # "max_values" por "min_valures", o comando eliminaria os N menores 
    # valores. 
    extreme_values <- max_values
    
    
    idxs <- match(extreme_values, data)
    s[idxs] <- FALSE
    return(s)
}

### Fun��o que remove os outliers considerando a an�lise de todas
### as features. Assim, permanecem apenas os exemplos que n�o apresentam 
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
