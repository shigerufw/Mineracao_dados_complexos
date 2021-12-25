library(caret)
library(mlbench)
data(Sonar)
set.seed(7)

controle <- rfeControl(method="cv",
                       number=3,
                       returnResamp="all",
                       functions=caretFuncs,
                       saveDetails=TRUE)

modelo <- rfe(Sonar[, 1:60],
              Sonar[, 61],
              sizes=c(1, 5, 10, 15),
              method="knn",
              trControl=trainControl(method="cv",
                                     classProbs=TRUE),
              tuneGrid=data.frame(k=1:10),
              rfeControl=controle)

modelo

# saída
Recursive feature selection

Outer resampling method: Cross-Validated (3 fold) 

Resampling performance over subset size:

 Variables Accuracy  Kappa AccuracySD KappaSD Selected
         1   0.7357 0.4647    0.04241 0.08474         
         5   0.7114 0.4143    0.02725 0.05446         
        10   0.7690 0.5302    0.07610 0.15790         
        15   0.8121 0.6204    0.06412 0.12964         
        60   0.8123 0.6208    0.03952 0.08196        *

The top 5 variables (out of 60):
   V11, V12, V10, V49, V9

modelo$fit$results

# saída

    k  Accuracy     Kappa AccuracySD   KappaSD
1   1 0.8143506 0.6230148 0.10075664 0.2117007
2   2 0.8088961 0.6131249 0.08089464 0.1682217
3   3 0.7987013 0.5909160 0.09651992 0.1990984
4   4 0.8075325 0.6101845 0.05104247 0.1051279
5   5 0.8020779 0.5998756 0.06471409 0.1309752
6   6 0.7737229 0.5412993 0.07605105 0.1519914
7   7 0.7882468 0.5715257 0.06249030 0.1258503
8   8 0.7496753 0.4934019 0.07467008 0.1479710
9   9 0.7249351 0.4391826 0.09553083 0.1940142
10 10 0.6999351 0.3859991 0.09924263 0.2062064
