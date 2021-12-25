# Exercicios de Logica de programacao Aula 3

# Produto dos elementos de um vetor
myprod <- function(...){
  prod = 1
  if (length(c(...)) == 0) {
    return(0)
  }
  for (i in c(...)){
    prod = prod*i
  }
  return(prod)
}

# Min valor de um vetor
mymin <- function(...){
  min <- Inf
  if(missing(c(...))){
    warning("Missing Input, Returning Infinite")
  }
  for (i in c(...)){
    if (i < min){
      min <- i
    }
  }
}

# Media dos elementos de um vetor
mymean <- function(...){
  sum = 0
  if (length(c(...)) == 0) {
    warning("Missing Input, Returning NaN")
  }
  for (i in c(...)){
    sum = sum + i
  }
  return(sum/length(c(...)))
}

# Mediana dos elementos de um vetor
mymedian <- function(...){
  pos = length(c(...))/2
  
  if (pos %% 2 != 0) {
    return((c(...)[floor(pos)] + c(...)[ceiling(pos)]) / 2)
  }
  else {
    return(c(...)[ceiling(pos)])
  }
}

# Verificar se um conjunto eh subconjunto de outro
subset <- function(set1, set2){
  all(is.element(set1, set2))
}

# Verificar se dois conjuntos sao distintos
disjoint <- function(set1, set2){
  !any(is.element(set1, set2))
}

# Reverter String
revstr <- function(string){
  len <- nchar(string)
  rev <- ""
  while (len >= 0){
    rev = paste(rev, substr(string, len, len))
    len = len - 1
  }
  return(rev)
}
