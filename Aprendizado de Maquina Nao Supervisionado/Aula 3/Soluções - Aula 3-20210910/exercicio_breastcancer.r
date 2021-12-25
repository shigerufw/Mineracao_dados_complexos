library(caret)
library(mlbench)
set.seed(7)

cancerData <- read.csv("bcwd.csv", sep=",", header=T)

# verifica correlação
matriz_correlacao <- cor(cancerData[, 3:32])

# encontra atributos altamente correlacionados
altamente_corr <- findCorrelation(matriz_correlacao, cutoff=0.5)

# mostra índice de atributos altamente correlacionados
print(altamente_corr)

[1]  7  8  6 28 27 23 21  3 26 24  1 13 11 18 16 14 17  5  9 10 30 22

# seleção de atributos usando florestas aleatórias
controle <- rfeControl(functions=rfFuncs, method="cv", number=10)

# executa o método RFE
resultados <- rfe(cancerData[,3:32], 
                  cancerData[,2], sizes=c(3:32),
                  rfeControl=controle)

print(resultados)

# saída

Recursive feature selection

Outer resampling method: Cross-Validated (10 fold) 

Resampling performance over subset size:

 Variables Accuracy  Kappa AccuracySD KappaSD Selected
         3   0.9349 0.8594    0.03742 0.08275         
         4   0.9368 0.8642    0.02761 0.05983         
         5   0.9507 0.8940    0.03196 0.06920         
         6   0.9560 0.9057    0.02526 0.05454         
         7   0.9543 0.9015    0.02372 0.05158         
         8   0.9613 0.9164    0.02172 0.04757         
         9   0.9630 0.9203    0.02558 0.05548         
        10   0.9595 0.9127    0.02999 0.06479         
        11   0.9683 0.9316    0.02851 0.06181         
        12   0.9648 0.9241    0.02623 0.05704         
        13   0.9665 0.9277    0.02808 0.06096         
        14   0.9666 0.9281    0.02806 0.06081         
        15   0.9666 0.9280    0.02681 0.05816         
        16   0.9700 0.9355    0.02882 0.06253        *
        17   0.9700 0.9354    0.02881 0.06251         
        18   0.9700 0.9353    0.02882 0.06273         
        19   0.9700 0.9354    0.02759 0.05999         
        20   0.9683 0.9316    0.02729 0.05951         
        21   0.9648 0.9241    0.02990 0.06474         
        22   0.9665 0.9278    0.02684 0.05841         
        23   0.9648 0.9241    0.02872 0.06228         
        24   0.9665 0.9274    0.03052 0.06677         
        25   0.9665 0.9274    0.03052 0.06677         
        26   0.9665 0.9278    0.02927 0.06354         
        27   0.9648 0.9241    0.02752 0.05986         
        28   0.9648 0.9241    0.02623 0.05714         
        29   0.9613 0.9163    0.02857 0.06249         
        30   0.9666 0.9279    0.02681 0.05843         

The top 5 variables (out of 16):
   area_worst, perimeter_worst, concave.points_worst, radius_worst, texture_worst

# lista de variáveis escolhidas
predictors(resultados)

 [1] "area_worst"           "perimeter_worst"      "concave.points_worst"
 [4] "radius_worst"         "texture_worst"        "concave.points_mean" 
 [7] "area_se"              "texture_mean"         "concavity_worst"     
[10] "concavity_mean"       "smoothness_worst"     "area_mean"           
[13] "radius_se"            "radius_mean"          "perimeter_se"        
[16] "perimeter_mean"      

# mostra gráfico com acurácias
plot(resultados, type=c("g", "o"))
