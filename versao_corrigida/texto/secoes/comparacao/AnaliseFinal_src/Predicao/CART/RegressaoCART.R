#library(earth)    #MARS.
#library(kernlab)  #SVM para regressao.
#library(caret)    #Para o KNN e outros algoritmos de IA.
#library(nnet)     #Para a rede neural.
#library(glmnet)   #RIDGE REGRESSION
library(rpart)    #CART


#Leitura de arquivo csv e paste de path de arquivos
source('Utils.R')

sementeAleatoria <- 857

TestaDiferentesLimiares<-function(ValoresPreditos, valoresDeTestes, nomeArquivoSaida, MinSplit, MaxDepth, cp, splitFunction){
  
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

TestesRegressaoCART<-function(dados, MinSplit, MaxDepth, cp, splitFunction){
  
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
      
      controle <- rpart.control(minsplit=MinSplit, cp=cp, maxdepth=MaxDepth)
      
      fit <- rpart(Class~.,
                   data=training, 
                   control=controle, 
                   parms=list(split=splitFunction), 
                   method = "anova")
      
      predictions <- predict(fit, testing[-position])
      
      TestaDiferentesLimiares(predictions, testing$Class, "outputCART", MinSplit, MaxDepth, cp, splitFunction)
      
    }
  }  

  print(tbOLD)
  
  params[2] <<-  MinSplit
  params[4] <<-  MaxDepth
  params[6] <<-  cp
  params[8] <<-  splitFunction
  
  salvaArquivo("outputCARTREGRESSION2.txt", params, tbOLD)
  
  tbOLD <<-  table(c(0,1),c(0,1))
  tbOLD[4] <<-  tbOLD[4] - 1
  tbOLD[3] <<-  0
  tbOLD[2] <<-  0
  tbOLD[1] <<-  tbOLD[1] - 1
  
  print(tbOLD)
  
}



dados <- leArquivoCSV(getCaminhoAbsolutoArquivoLocal("dados.csv"))

#Parametros globais
vetorBest <-  c()
params <-  c()
params[1] <-  "MinSplit"
params[3] <-  "MaxDepth"
params[5] <-  "cp"
params[7] <-  "splitFunction"


tbOLD <-  table(c(0,1),c(0,1))
tbOLD[4] <-  tbOLD[4] - 1
tbOLD[1] <-  tbOLD[1] - 1

#Tabela que armazena a soma dos melhores limiares da regressao
#tbGlobal <<-  table(c(0,1),c(0,1))
#tbGlobal[4] <<-  tbGlobal[4] - 1
#tbGlobal[1] <<-  tbGlobal[1] - 1


vartmp <- 0

if(vartmp == 0){ #Melhor Rodada
  
  TestesRegressaoCART(dados,
                      MinSplit=2, 
                      MaxDepth=20, 
                      cp=0.00001,
                      splitFunction="information")
  
  #print(tbOLD)
  
}else{
  
  #Parametros
  valoresDeSplit<-c("information", "gini")
  valoresDeminsplit <- c(0.000001, 0.00001, 0.0001, 0.001, 0.01,0, 1, 2, 3, 10, 20)
  valoresDeCP <- c(0.000001, 0.00001, 0.0001, 0.001, 0.01,  0.1, 1)
  valoresDemaxdepth <- c(3,5,10,20,30)
  
  for(c in seq(valoresDeSplit)){
    for(gamma in seq(valoresDeminsplit)){
      for(coef0 in seq(valoresDeCP)){
        for(degree in seq(valoresDemaxdepth)){       
          
          print(paste("split =",valoresDeSplit[c],
                      "minSplit = ", valoresDeminsplit[gamma] , 
                      "cp=",valoresDeCP[coef0], 
                      "maxdepth = ", valoresDemaxdepth[degree]))
          
         tryCatch({  TestesRegressaoCART(dados,
                                          MinSplit=valoresDeminsplit[gamma], 
                                          MaxDepth=valoresDemaxdepth[degree], 
                                          cp=valoresDeCP[coef0],
                                          splitFunction=valoresDeSplit[c])
          })
          
        } 
      } 
    }  
  }  
}

#Desabilita warnings
options(warn=-1)