#   Script responsavel por executar experimentos de classificação 
#usando o KNN precisa do pacote Caret. Executa com valores de 
#k entre [1,80] redireciona a saida (matriz de confusao) para um arquivo texto.

library(e1071)

#Leitura de arquivo csv e paste de path de arquivos
source('Utils.R')

sementeAleatoria <- 857

leArquivoCSV <- function(path) {
  return(read.table(path, header = TRUE, sep = ","))
}

testeNaiveBayes<-function(dados, laplaceSmoothing) {
  
  print(paste("laplaceSmoothing: ", laplaceSmoothing))
  print("----------------------------------------------------------------")
  set.seed(sementeAleatoria)
  
  #Gera os indices para o 10-fold cross validation
  numeroLinhas <- nrow(dados)
  indices <- seq(from=0, to=numeroLinhas, by=118)
  folds <- seq(from=1, to=64, by=7)
  folds[11] <- 74
  
  #print(indices)
  #print("------------------------------------")
  #print(folds)
  tb <-c()
  for(i in seq(folds)){
    if(is.na(indices[folds[i+1]]) == FALSE) {
      #print(paste(i, " - i: ",indices[folds[i]],"i+1: ",indices[folds[i+1]]))
      #print(" ")
      
      #Teste e treino
      testing   <- subset(dados, idunico > indices[folds[i]] & idunico <= indices[folds[i+1]])
      training  <- subset(dados, idunico <= indices[folds[i]] | idunico > indices[folds[i+1]])
      
      position <- which(colnames(testing)=="Class")
      #model <- naiveBayes(as.factor(training$Class)~.,
      model <- naiveBayes(as.factor(training$Class)~.,
                          data=training, 
                          laplace = laplaceSmoothing)
      
      
      predictions <- predict(model, testing[-position])
      #tb<- table(predictions, testing$Class)
      
      if(is.null(tb)){
        tb <- table(predictions, testing$Class)  
      }else{
        tb <- tb + table(predictions, testing$Class)  
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
    params[2] <<- laplaceSmoothing
   
  }else{#Nao eh nullo
    
    if(tb[1] > vetorBest[1] && tb[4] > vetorBest[4]){
      
      vetorBest[1] <<- tb[1]
      vetorBest[2] <<- tb[2]
      vetorBest[3] <<- tb[3]
      vetorBest[4] <<- tb[4]
      params[2] <<- laplaceSmoothing
    
    }
  } 

  
}

dados <- leArquivoCSV(getCaminhoAbsolutoArquivoLocal("dados.csv"))

#Parametros globais
vetorBest <-c()
params <-c()
params[1] <- "valoresDeLaplace"

vartmp <- 1

if(vartmp == 0){ #Melhor Rodada

  testeNaiveBayes(dados, 0)
  salvaArquivo("outputNAIVEBAYESClassifications.txt", params, vetorBest)
  
}else{
  valoresDeLaplace <- c(0:100)
  for(laplace in seq(valoresDeLaplace)) {
    
    testeNaiveBayes(dados, valoresDeLaplace[laplace])
   # print(paste("iteracao: ", laplace))
    
  }
  
  salvaArquivo("outputNAIVEBAYESClassifications.txt", params, vetorBest)
}



#Desabilita warnings
options(warn=-1)