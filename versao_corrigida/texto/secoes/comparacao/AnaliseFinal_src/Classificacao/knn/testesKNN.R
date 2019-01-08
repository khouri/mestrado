library(caret)
source("Utils.R")
sementeAleatoria <- 857

KNNOriginal<-function(valorDeK, dados, vetorBest, params){
  
  set.seed(sementeAleatoria)
  
  print(paste("valorDeK: ", valorDeK))
  print("----------------------------------------------------------------")
  set.seed(sementeAleatoria)
  
  #Gera os indices para o 10-fold cross validation
  numeroLinhas <- nrow(dados)
  indices <- seq(from=0, to=numeroLinhas, by=118)
  folds <- seq(from=1, to=64, by=7)
  folds[11] <- 74
  
  tb <-c()
  for(i in seq(folds)){
    if(is.na(indices[folds[i+1]]) == FALSE) {
      print(paste(i, " - i: ",indices[folds[i]],"i+1: ",indices[folds[i+1]]))
      print(" ")
      
      #Teste e treino
      testing   <- subset(dados, idunico > indices[folds[i]] & idunico <= indices[folds[i+1]])
      training  <- subset(dados, idunico <= indices[folds[i]] | idunico > indices[folds[i+1]])      
      position <- which(colnames(testing)=="Class")
      
      #Calcula qtd de true e false
      numTRUE   <- nrow(as.data.frame(subset(training, training$Class == "1")))
      numFALSE  <- nrow(as.data.frame(subset(training, training$Class == "0")))
      
      #cl <- factor(c(rep("T",3894), rep("F",3894)))
      cl <- factor(c(rep("1",numTRUE), rep("0",numFALSE)))
      #print(summary(cl))
      #print(summary(training))
      fit <- knn3(Class~., data=training, k=valorDeK)
      
    
     #salvaArquivo("outputKNNClassifications.txt", cl, training)
      resultadoTreinamento <- knn3Train(train=training, test=testing, cl=cl, k = valorDeK, prob = FALSE) 
      write.table(resultadoTreinamento, file = "MyData.csv", append = TRUE, sep = ", ") 
     
     if(is.null(tb)){
       tb <- table(resultadoTreinamento, testing$Class)  
     }else{
       tb <- tb + table(resultadoTreinamento, testing$Class)  
     }
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
    params[2] <<- valorDeK
    
  }else{#Nao eh nullo
    
    if(tb[1] > vetorBest[1] && tb[4] > vetorBest[4]){
      
      vetorBest[1] <<- tb[1]
      vetorBest[2] <<- tb[2]
      vetorBest[3] <<- tb[3]
      vetorBest[4] <<- tb[4]
      params[2] <<- valorDeK
    }    
  }  
}


#Rodar experimentos
dados <- leArquivoCSV(getCaminhoAbsolutoArquivoLocal("dados.csv"))

vetorBest <-c()

params <-c()
params[1] <- "k"

varTmp <- 0
if(varTmp == 0){
  print("porco")
  melhorK <- 1
  KNNOriginal(melhorK, dados, vetorBest,params)
  #salvaArquivo("outputKNNClassifications.txt", params, vetorBest)
  
}else{
  
  valoresDeK <- c(1:72)
  for(k in seq(valoresDeK)) {
    KNNOriginal(valoresDeK[k], dados, vetorBest,params)
    print(paste("iteracao: ", k))   
  }
  
  print(vetorBest)
  print(params)
  
  salvaArquivo("outputKNNClassifications.txt", params, vetorBest)
}

#Desabilita warnings
options(warn=-1)