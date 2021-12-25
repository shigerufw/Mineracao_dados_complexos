
#Imports
setwd("~/Documents/UnicampMDC/INF-0612/Trabalho 3")
library(ggplot2)


source_url <- url("https://ic.unicamp.br/~zanoni/cepagri/cepagri.csv")
col_names <- c("horario", "temp", "vento", "umid", "sensa")
cepagri <- read.table(
  source_url, header = FALSE, fill = TRUE, sep = ";", col.names = col_names
)

#Formatando horario
cepagri$horario <- as.POSIXlt(cepagri$horario)
cepagri$ano <- unclass(cepagri$horario)$year + 1900
cepagri$mes <- unclass(cepagri$horario)$mon + 1
cepagri$dia <- unclass(cepagri$horario)$yday

tapply(cepagri$temp, cepagri$dia, mean)
tapply(cepagri$temp, cepagri$dia, median)
tapply(cepagri$temp, cepagri$dia, sd)

cepagri$horario <- as.POSIXct(
  cepagri$horario, format = '%d/%m/%Y-%H:%M', tz = "America/Sao_Paulo"
)

summary(cepagri)

cepagri[cepagri$sensa == 99.9, 5] <- NA

cepagri$temp <- as.numeric(cepagri$temp)
filtro <- consecutive(cepagri$temp, 144)

unique(as.Date(cepagri[filtro, 1]))


time_period_start <- as.POSIXct('2015-01-01',format='%Y-%m-%d')
time_period_end <- as.POSIXct('2020-12-31',format='%Y-%m-%d')
timeframe <- (as.Date(cepagri$horario) >= time_period_start & as.Date(cepagri$horario) <= time_period_end)
