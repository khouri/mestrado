#   Script responsavel por executar experimentos de classificação 
#usando uma rede neural precisa do pacote Caret. Redireciona a saida
#(matriz de confusao) para um arquivo texto.

# library(caret)
# library(mlbench)
# library(e1071)
# library(kernlab)
# library(klaR)
library(nnet)

#Leitura de arquivo csv e paste de path de arquivos
source('Utils.R')

sementeAleatoria <- 857

TestesNNETOriginal<-function(dados, numNeuronios, txAprendizagem){
  
  set.seed(sementeAleatoria)
  
  
  print(paste("numNeuronios: ", numNeuronios, "txAprendizagem: ", txAprendizagem))
  imprimeLinhaPontilhada()
  set.seed(sementeAleatoria)
  
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
      
      #Teste e treino
      testing   <- subset(dados, idunico > indices[folds[i]] & idunico <= indices[folds[i+1]])
      training  <- subset(dados, idunico <= indices[folds[i]] | idunico > indices[folds[i+1]])
      
      position <- which(colnames(testing)=="Class")
      
      fit <- nnet(Class~., 
                  data=training, 
                  size=numNeuronios, 
                  decay=txAprendizagem, 
                  entropy = TRUE,
                  maxit=5000, 
                  MaxNWts = 1000000, 
                  trace = FALSE)
      
      predictions <- predict(fit, testing[-position], type="class")
      predictions <- as.character(predictions)
      testing$Class <-  as.character(testing$Class)

      numTRUE   <- nrow(as.data.frame(subset(predictions, predictions == "1")))
      numFALSE  <- nrow(as.data.frame(subset(predictions, predictions == "0")))
      
      #print(numTRUE)      
      #print(numFALSE)
      #tmp <- table(predictions, testing$Class)
      #print(table(predictions, testing$Class))     
      
      if(numTRUE == 0){
        #print("TRUE")
        tmp3 <- table(c(1,predictions), c(0,testing$Class))
        tmp3[2] <- 0
        ##print(tmp3[3])
        #print(tmp3[2])
        #print(paste("1",tmp3[1]))
        #print(paste("2",tmp3[2]))
       # print(paste("3",tmp3[3]))
      #  print(paste("4",tmp3[4]))
        
        tb <- tb + tmp3
        #print(tmp3)
        #pausa()
      }
      if(numFALSE == 0){
       # print("FALSE")
        tmp2 <- table(c(predictions,0), c(testing$Class,0))
        tmp2[1] <- 0
        #print(paste("1",tmp2[1]))
        #print(paste("2",tmp2[2]))
        #print(paste("3",tmp2[3]))
        #print(paste("4",tmp2[4]))
        tb <- tb + tmp2      
        #print(tmp2)
      }
      if(numFALSE != 0 && numTRUE != 0){
        #print("porco")
        #print(table(predictions, testing$Class))
        tb <- tb + table(predictions, testing$Class)
        #print(paste("1",tb[1]))
        #print(paste("2",tb[2]))
        #print(paste("3",tb[3]))
        #print(paste("4",tb[4]))
      }
      
     # imprimeLinhaPontilhada()
    }
    
    
  }
  print(tb)
  
  #Encontra os melhores parametros
  if(is.null(vetorBest) == TRUE) {
    
    vetorBest <<- tb
    vetorBest[1] <<- tb[1]
    vetorBest[2] <<- tb[2]
    vetorBest[3] <<- tb[3]
    vetorBest[4] <<- tb[4]
    params[2] <<- numNeuronios
    params[4] <<- txAprendizagem
   
    
  }else{#Nao eh nullo
    
    #print(tb)
    #print(vetorBest)
    if(tb[1] > vetorBest[1] && tb[4] > vetorBest[4]){
      
      vetorBest[1] <<- tb[1]
      vetorBest[2] <<- tb[2]
      vetorBest[3] <<- tb[3]
      vetorBest[4] <<- tb[4]
      params[2] <<- numNeuronios
      params[4] <<- txAprendizagem
    
    }    
  } 
  
  
}

dados <- leArquivoCSV(getCaminhoAbsolutoArquivoLocal("dados.csv"))

#Parametros globais
vetorBest <-c()
params <-c()
params[1] <- "valoresDeNeuronios"
params[3] <- "valoresDeAprendizagem"

vartmp <- 1
if(vartmp == 0){ #Melhor Rodada

  TestesNNETOriginal(dados, 2, 0.001)
  salvaArquivo("outputNNETClassifications.txt", params, vetorBest)
  
  
}else{
  
  valoresDeNeuronios<-c(1:20)
  valoresDeAprendizagem<-c(0.01,0.001,0.0001,0.00001,0.000001,0.0000001)
  
  for(neuronio in seq(valoresDeNeuronios)) {
    for(taxa in seq(valoresDeAprendizagem)) {
      TestesNNETOriginal(dados, valoresDeNeuronios[neuronio], valoresDeAprendizagem[taxa])
      #print(paste("neuronio=",valoresDeNeuronios[neuronio], "txAprendizagem=",valoresDeAprendizagem[taxa]))
    }
  }
  
  salvaArquivo("outputNNETClassifications.txt", params, vetorBest)
  
}

#Desabilita warnings
options(warn=-1)



#pausa()
#       
#       if(length(table(predictions, testing$Class)) != 4){
#         
#         var33 <- table(c(predictions,1), c(testing$Class,1))
#         var33[1] <- 1
#         var33[3] <- 0
#         
#       }else{
#         #print(typeof(predictions))
#         #print(typeof(testing$Class))
#         
#         # if(is.null(tb)){
#         
#         #print(typeof(tb))
#         #   tb <- table(predictions, testing$Class) 
#         #print(tb)
#         # }else{
#         
#         #print(predictions)
#         print("------------------------------------------------")
#         print("else")
#         #print(tb)
#         
#         #varTMP <- tb
#         
#         #varTMP <- varTMP +  table(predictions, testing$Class)
#         #print(length(varTMP))
#         #print(table(predictions, testing$Class))
#         ##varTMP[4] <- 0
#         #varTMP[3] <- 0
#         #print(varTMP)
#         print(length(tb))
#         print(length(table(predictions, testing$Class)))
#         var33 <- table(c(predictions,1), c(testing$Class,1))
#         var33[1] <- 1
#         var33[3] <- 0
#         
#         print(var33)
#         print(tb)
#         
#         
#         tb <- tb + var33#table(predictions, testing$Class) #table(predictions, testing$Class)  
#         #}     
#       }