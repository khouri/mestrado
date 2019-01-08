library(rpart)

#Leitura de arquivo csv e paste de path de arquivos
source('Utils.R')

sementeAleatoria <- 857

testeCART<-function(dados, #dados
                    #parametros cart
                    MinSplit, 
                    MaxDepth, 
                    cp, 
                    splitFunction, 
                    #
                    atividadesOrdenar,
                    todasAsAtividades,
                    #melhores parametros
                    vetorBest,
                    params) {
  
  print(paste("MinSplit: ", MinSplit, "MaxDepth: ", MaxDepth, "cp: ", cp, "splitFunction: ", splitFunction))
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
      controle <- rpart.control(minsplit=MinSplit, cp=cp, maxdepth=MaxDepth)
      
      fit <- rpart(Class~., data = training, control = controle, parms = list(split = splitFunction), method="class")
      predictions <- predict(fit, testing[-position],type="class")
    
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
    params[2] <<- MinSplit
    params[4] <<- MaxDepth
    params[6] <<- cp
    params[8] <<- splitFunction
    
  }else{#Nao eh nullo
    
    #print(tb)
    #print(vetorBest)
    if(tb[1] > vetorBest[1] && tb[4] > vetorBest[4]){
      
      vetorBest[1] <<- tb[1]
      vetorBest[2] <<- tb[2]
      vetorBest[3] <<- tb[3]
      vetorBest[4] <<- tb[4]
      params[2] <<- MinSplit
      params[4] <<- MaxDepth
      params[6] <<- cp
      params[8] <<- splitFunction
    }
    
  } 
  
  
}

dados <- leArquivoCSV(getCaminhoAbsolutoArquivoLocal("dados.csv"))

#Parametros globais
vetorBest <-c()
params <-c()
params[1] <- "minsplit"
params[3] <- "MaxDepth"
params[5] <- "cp"
params[7] <- "splitFunction"


vartmp <- 1
if(vartmp == 0){ #Melhor Rodada
    
  testeCART(dados,
            MinSplit=1, 
            MaxDepth=30, 
            cp=0.01,
            splitFunction="gini",
            ActivitiesFake,
            AllActivities,
            vetorBest,
            params)
  
  salvaArquivo("outputCARTClassifications.txt", params, vetorBest)
  
  
}else{
  
  #Parametros
  valoresDeSplit<-c( "information", "gini")
  valoresDeminsplit <- c(0, 1, 2, 3, 10, 20)
  valoresDeCP <- c(0.000001, 0.00001, 0.0001, 0.001, 0.01, 0.1, 1, 10)
  valoresDemaxdepth <- c(3, 5, 10, 20, 30)
  
  for(c in seq(valoresDeSplit)){
    for(gamma in seq(valoresDeminsplit)){
      for(coef0 in seq(valoresDeCP)){
        for(degree in seq(valoresDemaxdepth)){       
          
            print(paste("split =",valoresDeSplit[c],
                       "minSplit = ", valoresDeminsplit[gamma] , 
                       "cp=",valoresDeCP[coef0], 
                       "maxdepth = ", valoresDemaxdepth[degree]))
            
            tryCatch({  testeCART(dados,
                                  MinSplit=valoresDeminsplit[gamma], 
                                  MaxDepth=valoresDemaxdepth[degree], 
                                  cp=valoresDeCP[coef0],
                                  splitFunction=valoresDeSplit[c],
                                  ActivitiesFake,
                                  AllActivities,
                                  vetorBest,
                                  params)
            })
          
        } 
      } 
    }  
  }
  
  salvaArquivo("outputCARTClassifications.txt", params, vetorBest)
  
  
}






#Desabilita warnings
options(warn=-1)