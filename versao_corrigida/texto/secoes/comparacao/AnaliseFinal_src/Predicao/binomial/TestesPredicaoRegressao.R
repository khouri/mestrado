library(glmnet)   #RIDGE REGRESSION
source('Utils.R')

sementeAleatoria <- 857

TestesRegressaoBinomial<-function(dados, vetorBest, params){
  
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
      
      fit <- glm( Class~., data =training[1:position], family=binomial())
      predictions <- predict(fit, testing[-position], type="response")
      
      #print(i)
      TestaDiferentesLimiares(predictions, testing$Class, "outputBinomial", params)
      
      
      #       matriz <- as.character(matriz)
      #       testing$Class <-  as.character(testing$Class)
      #       
      #       numTRUE   <- nrow(as.data.frame(subset(matriz, matriz == "1")))
      #       numFALSE  <- nrow(as.data.frame(subset(matriz, matriz == "0")))
      #       
      #       if(numTRUE == 0){
      #         tmp3 <- table(c(1,matriz), c(0,testing$Class))
      #         tmp3[2] <- 0
      #         tb <- tb + tmp3
      #         print("TRUE")
      #       }
      #       if(numFALSE == 0){
      #         tmp2 <- table(c(matriz,0), c(testing$Class,0))
      #         tmp2[1] <- 0
      #         tb <- tb + tmp2      
      #         print("FALSE")        
      #       }
      #       if(numFALSE != 0 && numTRUE != 0){
      #         tb <- tb + table(matriz, testing$Class)
      #         print("?????") 
      #       }
      #     }
      #   }
      
      
      #   #Encontra os melhores parametros
      #   if(is.null(vetorBest) == TRUE) {
      #     
      #     vetorBest <<- tb
      #     vetorBest[1] <<- tb[1]
      #     vetorBest[2] <<- tb[2]
      #     vetorBest[3] <<- tb[3]
      #     vetorBest[4] <<- tb[4]
      # 
      #   }else{#Nao eh nullo
      #     
      #     if(tb[1] > vetorBest[1] && tb[4] > vetorBest[4]){
      #       
      #       vetorBest[1] <<- tb[1]
      #       vetorBest[2] <<- tb[2]
      #       vetorBest[3] <<- tb[3]
      #       vetorBest[4] <<- tb[4]
      #       
      #     }
    }  
    
    #print("Finish him!")
  }
}


#testa a matriz para cada valor de limiar obtido
TestaDiferentesLimiares<-function(ValoresPreditos, valoresDeTestes, nomeArquivoSaida, params){
  
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
vetorBest <-c()
params <-c()
params[1] <- "limiar"
tbOLD <- table(c(0,1),c(0,1))
tbOLD[4] <- tbOLD[4] - 1
tbOLD[1] <- tbOLD[1] - 1

#Tabela que armazena a soma dos melhores limiares da regressao
tbGlobal <- table(c(0,1),c(0,1))
tbGlobal[4] <- tbGlobal[4] - 1
tbGlobal[1] <- tbGlobal[1] - 1

TestesRegressaoBinomial(dados, tbOLD, params)
salvaArquivo("outputBINOMIALClassifications.txt", params, tbOLD)

#Desabilita warnings
options(warn=-1)