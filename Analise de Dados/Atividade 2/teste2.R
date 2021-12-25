########################################
# Teste 2         
# Nome(s): 
########################################

## 1 - Agrupamento

groupsum <- function(df, colgroup, colsum){
  df <- tapply(df[[colsum]],df[[colgroup]], sum)
  df <- data.frame(df)
  df$cidades <- rownames(df)
  rownames(df) = NULL
  colnames(df) <- c(colsum, colgroup)
  df[,c(colgroup, colsum)]
}

##### Exemplos no PDF:
dia <- c(01, 01, 02, 02, 03, 03, 04, 04, 05, 05)
cidade <- c('Campinas', 'Vinhedo', 'Campinas', 'Limeira', 'Campinas', 'Vinhedo', 'Campinas', 'Vinhedo', 'Limeira', 'Campinas')
chuva <- c(0.15, 0.02, 0.01, 0.13, 0.12, 2.19, 1.11, 0.76, 2.98, 0.45)
chuvas <- data.frame(cidade, dia, chuva)
groupsum(chuvas, "cidade", "chuva")

## 2 - Binario para Decimal

binToDec <- function(...){
  
  transform <- function(bin){
    pos <- length(bin)
    dec <- 0
    while (pos > 0){
      dec <- dec + (bin[pos]*2^(length(bin)-pos))
      pos = pos - 1
    }
    return(dec)
  }
  
  ans <- sapply(list(...), transform)
  return(ans)
  
}
##### Exemplos no PDF:
binToDec(c(1, 0))
binToDec(c(0, 0, 1), c(1, 1))
binToDec(rep(1, 3), rep(0, 2), rep(c(1,0), 2))

## 3 - Ocorrencia de Palavras

wordCount <- function(word, text){
  punct <- '[.,!?]*'
  txt_list <- strsplit(gsub(punct, '', tolower(text)), split=' ')
  print(txt_list[[1]])
  return(sum(txt_list[[1]] == word))
}

##### Exemplos no PDF:
text <- "O rAto roeu a roupa do Rei de Roma! RainhA raivosa rasgou o resto."
wordCount("rato", text)
wordCount("roma", text)

text <- "A vaca malHada foi molhADA por outra VACA, MOLhada e MALhaDa."
wordCount("outra", text)
wordCount("vaca", text)
wordCount("malhada", text)

text <- "Se a liga me ligasse, eu tambem ligava a liga. Mas a liga nao me liga, eu tambem nao ligo a liga."
wordCount("liga", text)
wordCount("ligasse", text)
