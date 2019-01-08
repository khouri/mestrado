#library(kernlab)  #SVM para regressao.
library(e1071)

sementeAleatoria <- 857

#Leitura de arquivo csv e paste de path de arquivos
source('Utils.R')

#Usa o SVM para regressao, precisa parametrizar
#http://machinelearningmastery.com/non-linear-regression-in-r/
TestesRegressaoSVRegressionLINEAR<-function(dados, c, epsilon, tolerancia, kernel){
  
  set.seed(sementeAleatoria)  
  imprimeLinhaPontilhada()
  
  #Gera os indices para o 10-fold cross validation
  numeroLinhas <- nrow(dados)
  indices <- seq(from=0, to=numeroLinhas, by=118)
  folds <- seq(from=1, to=64, by=7)
  folds[11] <- 74
  
  tb <- table(c(0,1),c(0,1))
  tb[4] <- tb[4] - 1
  tb[1] <- tb[1] - 1
  
  for(i in seq(folds)){#itera como k-fold-cross validation
    if(is.na(indices[folds[i+1]]) == FALSE) {
      print(paste(i, " - i: ",indices[folds[i]],"i+1: ",indices[folds[i+1]]))
      #print(" ")
      
      #Teste e treino
      testing   <- subset(dados, idunico > indices[folds[i]] & idunico <= indices[folds[i+1]])
      training  <- subset(dados, idunico <= indices[folds[i]] | idunico > indices[folds[i+1]])
      
      position <- which(colnames(testing)=="Class")
      
      fit <- svm( 
        x=training[,-position],
        y=training[,position],
        type='eps-regression', 
        kernel='linear',
        cost=c,
        epsilon=epsilon, 
        tolerance=tolerancia,
        fitted=TRUE,
        shrinking=TRUE,
        scale=TRUE
      )
      
      predictions <- predict(fit, testing[-position])
      
      TestaDiferentesLimiares(predictions, testing$Class, c, epsilon, tolerancia, kernel)
      
    }
  }  
  
  print(tbOLD)
  
  params[2] <<-  c
  params[4] <<-  epsilon
  params[6] <<-  tolerancia
  params[8] <<-  kernel
  
  salvaArquivo("outputSVMRESSION.txt", params, tbOLD)
  
  tbOLD <<-  table(c(0,1),c(0,1))
  tbOLD[4] <<-  tbOLD[4] - 1
  tbOLD[3] <<-  0
  tbOLD[2] <<-  0
  tbOLD[1] <<-  tbOLD[1] - 1
  
  print(tbOLD)
  
}

TestaDiferentesLimiares<-function(ValoresPreditos, valoresDeTestes, c, epsilon, tolerancia, kernel){
  
  counter <- 0
  counter2 <- 0
  
  tb333 <- table(c(0,1),c(0,1))
  tb333[4] <- tb333[4] - 1
  tb333[1] <- tb333[1] - 1  
  
  #Permite verificar o melhor resultado de uma sessao do metodo 'TestaDiferentesLimiares'
  tbSessao <- table(c(0,1),c(0,1))
  tbSessao[4] <- tbSessao[4] - 1
  tbSessao[1] <- tbSessao[1] - 1
  
  contadorDeRodadas <- 0
  
  for( x in 1:length(ValoresPreditos)) {
    
    matrizTmp <- ValoresPreditos
    for( y in 1:length(ValoresPreditos)) {
      
      limiar <- ValoresPreditos[x]
      if(ValoresPreditos[y] < limiar){
        matrizTmp[y] <- 0
      }
      else{
        matrizTmp[y] <- 1
      }      
      
      counter <- counter + 1
    }
    counter2 <- counter2 + 1
    
    #Pega os resultados de um limiar e guarda em tbTmp (tb333)
    numTRUE2   <- nrow(as.data.frame(subset(matrizTmp, matrizTmp == "1")))
    numFALSE2  <- nrow(as.data.frame(subset(matrizTmp, matrizTmp == "0")))
    
    if(numTRUE2 == 0){
      tmp3 <- table(c(1,matrizTmp), c(0,valoresDeTestes))
      tmp3[2] <- 0
      tb333 <- tmp3
      #print("TRUE")
    }
    if(numFALSE2 == 0){
      tmp2 <- table(c(matrizTmp,0), c(valoresDeTestes,0))
      tmp2[1] <- 0
      tb333 <- tmp2
      #print("FALSE")        
    }
    if(numFALSE2 != 0 && numTRUE2 != 0){
      tb333 <- table(matrizTmp, valoresDeTestes)
      #print("?????") 
    }
    #Pega os resultados de um limiar e guarda em tbTmp (tb333)
    
    #Verifica o melhor valor de limiar e armazena esse resultado.
    if (contadorDeRodadas == 0){ #primeira execucao da rodada 
      
      tbSessao[1] <- tb333[1]
      tbSessao[2] <- tb333[2]
      tbSessao[3] <- tb333[3]
      tbSessao[4] <- tb333[4]
      params[2] <- limiar
      #print(limiar)
      
    }else {
      
      #Se o novo resultado for melhor que o anterior
      if( (tb333[1] > tbSessao[1] && tb333[4] > tbSessao[4]) == TRUE){
        
        tbSessao[1] <- tb333[1]
        tbSessao[2] <- tb333[2]
        tbSessao[3] <- tb333[3]
        tbSessao[4] <- tb333[4]
        params[2] <- limiar
        #print(limiar)
      }
      
    }
    #Verifica o melhor valor de limiar e armazena esse resultado.
    
    contadorDeRodadas <- contadorDeRodadas + 1
  }  
  
  #Verifica o melhor valor da sessao e guarda em variavel global.
  tbOLD[1] <<- tbOLD[1] + tbSessao[1]
  tbOLD[2] <<- tbOLD[2] + tbSessao[2]
  tbOLD[3] <<- tbOLD[3] + tbSessao[3]
  tbOLD[4] <<- tbOLD[4] + tbSessao[4]
  
  #Vou retornar a melhor matriz nessa situacao
  #return (matrizTmp)
  return (tbSessao)
}

dados <- leArquivoCSV(getCaminhoAbsolutoArquivoLocal("dados.csv"))

#Parametros globais
vetorBest <-  c()
params <-  c()
params[1] <-  "valoresDeC"
params[3] <-  "valoresDeEpsilon"
params[5] <-  "valoresDeTolerancia"
params[7] <-  "valoresDeKernel"

tbOLD <-  table(c(0,1),c(0,1))
tbOLD[4] <-  tbOLD[4] - 1
tbOLD[1] <-  tbOLD[1] - 1

vartmp <- 1
if(vartmp == 0){ #Melhor Rodada
  
  TestesRegressaoSVRegressionLINEAR(dados, 10, 5, 'sigmoid')
  
}else{  
  
  valoresDeC          <- c(10)
  valoresDeEpsilon    <- c(  1)
  valoresDeTolerancia <- c(0.00001, 0.0001, 0.001, 0.01, 0.1, 1)
  valoresDeKernel     <- c('linear', 'polynomial', 'radial', 'sigmoid')
  
  for(c in seq(valoresDeC)){
    for(epsilon in seq(valoresDeEpsilon)){      
      for(tolerancia in seq(valoresDeTolerancia)){
        for(kernel in seq(valoresDeKernel)){
          
          print( paste(" valoresDeC: ",          valoresDeC[c] , 
                       " valoresDeEpsilon: ",    valoresDeEpsilon[epsilon], 
                       " valoresDeTolerancia: ", valoresDeTolerancia[tolerancia],
                       " valoresDeKernel: ",     valoresDeKernel[kernel]
          ))
          
          TestesRegressaoSVRegressionLINEAR(dados, 
                                            valoresDeC[c],
                                            valoresDeEpsilon[epsilon],
                                            valoresDeTolerancia[tolerancia],
                                            valoresDeKernel[kernel] )
        }
      }
    } 
  } 
}

#Desabilita warnings
options(warn=-1)