
library(earth)    #MARS.
library(kernlab)  #SVM para regressao.
library(caret)    #Para o KNN e outros algoritmos de IA.
library(nnet)     #Para a rede neural.
library(glmnet)   #RIDGE REGRESSION
library(rpart)    #CART

sementeAleatoria <- 857

leArquivoCSV <- function(path) {
  return(read.table(path, header = TRUE, sep = ","))
}

TestaDiferentesLimiares<-function(ValoresPreditos, valoresDeTestes, nomeArquivoSaida, MinSplit, MaxDepth, cp, splitFunction){
  
  #var1 <- as.matrix(ValoresPreditos[,1])
  #print(var1)
  #print(ValoresPreditos[1,1])
  #print(ValoresPreditos[1,2])
  #print(class(valoresDeTestes))
  
  #print(length(ValoresPreditos))
  #print(dim(ValoresPreditos))
  #x <-dim(ValoresPreditos)[1]
  #print(x)
  for(x in 1:length(ValoresPreditos)) {
    
    matrizTmp <- as.matrix(ValoresPreditos[,1])
    
    for(y in 1:dim(ValoresPreditos)[1]) {
      
      limiar <- ValoresPreditos[x]
      if(ValoresPreditos[y,1] < ValoresPreditos[y,2]){
        matrizTmp[y] <- 0
      }
      else {
        matrizTmp[y] <- 1
      }
    }
   
    tb <- table(matrizTmp, valoresDeTestes)
    
    sink(paste('/home/toasty/Desktop/RBooks/Experimentos/Predicao/CART/', nomeArquivoSaida, '.txt'), append=TRUE)
    print(tb)
    print(paste("MinSplit: ", MinSplit, " MaxDepth: ", MaxDepth," cp: ", cp, " splitFunction: ", splitFunction))
    print("--------------------------------------------------")
    sink()
    
  }
}

TestesRegressaoCART<-function(ConjuntoTeste, ConjuntoTreino, MinSplit, MaxDepth, cp, splitFunction){
  
  set.seed(sementeAleatoria)
  
  #Conjuntos de treinamento e testes
  training <- ConjuntoTreino
  position <- which(colnames(training)=="Class")
  
  x <- as.matrix(training[-position])
  y <- as.matrix(training[position])
  testing <- ConjuntoTeste
  
  controle <- rpart.control(minsplit=MinSplit, cp=cp, maxdepth=MaxDepth)
  
  fit <- rpart(data=training, Class~.,control=controle,
               parms=list(split=splitFunction))
  
  # Constroi modelo para classificacao
  predictions <- predict(fit, testing[-position])
  
  #print(predictions)
  #print(testing$Class)
  
  #print(predictions)
  TestaDiferentesLimiares(predictions, testing$Class, "outputCART", MinSplit, MaxDepth, cp, splitFunction)
  
}


# Chamadas de testes
conjuntoTeste <- leArquivoCSV("/home/toasty/Desktop/RBooks/Experimentos/Predicao/saidaDesBalanceada_teste.csv") 
conjuntoTreino <- leArquivoCSV("/home/toasty/Desktop/RBooks/Experimentos/Predicao/saidaDesBalanceada_treino.csv")

#Parametros
valoresDeSplit<-c( "information", "gini")
valoresDeminsplit <- c(0, 1, 2, 3, 10, 20)
valoresDeCP <- c(0.000001, 0.00001, 0.0001, 0.001, 0.01,  0.1, 1, 2, 3, 4, 5, 10)
valoresDemaxdepth <- c(3,5,10,20,30)

for(c in seq(valoresDeSplit)){
  for(gamma in seq(valoresDeminsplit)){
    for(coef0 in seq(valoresDeCP)){
      for(degree in seq(valoresDemaxdepth)){       
        
        print(paste("split =",valoresDeSplit[c],
                    "minSplit = ", valoresDeminsplit[gamma] , 
                    "cp=",valoresDeCP[coef0], 
                    "maxdepth = ", valoresDemaxdepth[degree]))
        
        tryCatch({  TestesRegressaoCART(conjuntoTeste,
                                        conjuntoTreino,
                                        MinSplit=valoresDeminsplit[gamma], 
                                        MaxDepth=valoresDemaxdepth[degree], 
                                        cp=valoresDeCP[coef0],
                                        splitFunction=valoresDeSplit[c])
                })
        
      } 
    } 
  }  
}


#Desabilita warnings
options(warn=-1)