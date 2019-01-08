#library(caret)
#library(mlbench)
library(e1071)
library(kernlab)
#library(klaR)
#library(nnet)

#Leitura de arquivo csv e paste de path de arquivos
source('Utils.R')

sementeAleatoria <- 857

TestesSVMOriginal<-function(dados, kernel, c, gamma=1, coef0=0, degree=3, vetorBest, params){
  
  set.seed(sementeAleatoria)
  
  print(paste("kernel =", kernel, "c = ", c, "gamma=", gamma, "coef0 = ", coef0, "degree = ",degree))

  #imprimeLinhaPontilhada()
#set.seed(sementeAleatoria)
  
  #Gera os indices para o 10-fold cross validation
  numeroLinhas <- nrow(dados)
  indices <- seq(from=0, to=numeroLinhas, by=118)
  folds <- seq(from=1, to=64, by=7)
  folds[11] <- 74
  
  #print(indices)
  #print("------------------------------------")
  #print(folds)
  #tb <-c()# table(c(0,1),c(0,1))
  
  tb <- table(c(0,1),c(0,1))
  
  tb[4] <- tb[4] - 1
  tb[1] <- tb[1] - 1
  
  for(i in seq(folds)){
    if(is.na(indices[folds[i+1]]) == FALSE) {
      #print(paste(i, " - i: ",indices[folds[i]],"i+1: ",indices[folds[i+1]]))
      #print(" ")
      
     # print("Delano")
      
      #Teste e treino
      testing   <- subset(dados, idunico > indices[folds[i]] & idunico <= indices[folds[i+1]])
      training  <- subset(dados, idunico <= indices[folds[i]] | idunico > indices[folds[i+1]])
      
     # print("OXY")
      
      position <- which(colnames(testing)=="Class")
      
    #  print("POSICAO")
      
      fit <- svm(Class~., 
                 data=training[1:position], 
                 type='C-classification', 
                 kernel=kernel, 
                 cost=10, 
                 gamma=10, 
                 coef0=10, 
                 degree=1)
      
      #print("0000000000000")
      predictions <- predict(fit, testing[-position], type="class")
      #tb <- table(predictions, testing$Class)
      
      predictions <- as.character(predictions)
      testing$Class <-  as.character(testing$Class)
      
      numTRUE   <- nrow(as.data.frame(subset(predictions, predictions == "1")))
      numFALSE  <- nrow(as.data.frame(subset(predictions, predictions == "0")))
      
     # print("111111111111111")
      if(numTRUE == 0){
        tmp3 <- table(c(1,predictions), c(0,testing$Class))
        tmp3[2] <- 0
        tb <- tb + tmp3
      }
      if(numFALSE == 0){
        tmp2 <- table(c(predictions,0), c(testing$Class,0))
        tmp2[1] <- 0
        tb <- tb + tmp2      
      }
      if(numFALSE != 0 && numTRUE != 0){
        tb <- tb + table(predictions, testing$Class)
       
      }
    }
    
  }
  #print(tb)
  #print("tasty")
  #Encontra os melhores parametros
  if(is.null(vetorBest) == TRUE) {
    
    vetorBest <<- tb
    vetorBest[1] <<- tb[1]
    vetorBest[2] <<- tb[2]
    vetorBest[3] <<- tb[3]
    vetorBest[4] <<- tb[4]
    params[2] <<- kernel
    params[4] <<- c
    params[6] <<- gamma
    params[8] <<- coef0
    params[10] <<- degree
    
    salvaArquivo("outputSVMClassifications.txt", params, vetorBest)
  
    }else{#Nao eh nullo
    
    #print(tb)
    #print(vetorBest)
    if(tb[1] > vetorBest[1] && tb[4] > vetorBest[4]){
      
      vetorBest[1] <<- tb[1]
      vetorBest[2] <<- tb[2]
      vetorBest[3] <<- tb[3]
      vetorBest[4] <<- tb[4]
      params[2] <<- kernel
      params[4] <<- c
      params[6] <<- gamma
      params[8] <<- coef0
      params[10] <<- degree
      
      salvaArquivo("outputSVMClassifications.txt", params, vetorBest)
    }    
  } 
}


dados <- leArquivoCSV(getCaminhoAbsolutoArquivoLocal("dados.csv"))

#Parametros globais
vetorBest <-c()
params <-c()
params[1] <- "kernel"
params[3] <- "c"
params[5] <- "gamma"
params[7] <- "coef0"
params[9] <- "degree"

vartmp <- 1

if(vartmp == 0){
  
  TestesSVMOriginal(dados, 
                    kernel='polynomial', 
                    c=0.001, 
                    gamma=0.01,
                    coef0=0.01, 
                    degree=10,
                    vetorBest,
                    params)
  
  salvaArquivo("outputSVMClassifications.txt", params, vetorBest)
  
}else{
  
  valoresDeC<-c(   0.1,  1)
  valoresDeGamma <- c(  0.01, 10)
  valoresDeCoef0 <- c(0.0001, 0.001, 0.01, 5, 10)
  valoresDeDegree <- c( 0.0001, 0.001, 0.01, 4, 5, 10)
  tiposDeKernel <- c('linear', 'polynomial', 'radial', 'sigmoid')
  
  for(c in seq(valoresDeC)){
    for(gamma in seq(valoresDeGamma)){
      for(coef0 in seq(valoresDeCoef0)){
        for(degree in seq(valoresDeDegree)){
          for(kernel in seq(tiposDeKernel)){
            
            #print(paste("kernel =",tiposDeKernel[kernel],"c = ",valoresDeC[c] , "gamma=",valoresDeGamma[gamma], 
            #            "coef0 = ", valoresDeCoef0[coef0], "degree = ",valoresDeDegree[degree]))
            
            tryCatch({  TestesSVMOriginal(dados, 
                                          tiposDeKernel[kernel], 
                                          c=valoresDeC[c], 
                                          gamma=valoresDeGamma[gamma],
                                          coef0=valoresDeCoef0[coef0], 
                                          degree=valoresDeDegree[degree],
                                          vetorBest,
                                          params)
            })
            salvaArquivo("outputSVMClassifications.txt", params, vetorBest)
            
          } 
        } 
      } 
    }  
  }
  
  #salvaArquivo("outputSVMClassifications.txt", params, vetorBest)
}

#Desabilita warnings
options(warn=-1)