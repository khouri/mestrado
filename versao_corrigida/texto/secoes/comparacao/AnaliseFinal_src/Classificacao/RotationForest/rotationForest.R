#library(e1071)
#library(kernlab)
library(rpart)
library(rotationForest)

#Leitura de arquivo csv e paste de path de arquivos
source('Utils.R')

sementeAleatoria <- 857

TestesRotationForest<-function(dados, minsplit, maxdepth, splitFunction, listofK, 
                               listofL, limiarDeCorte, vetorBest, params){
  
  set.seed(sementeAleatoria)
  
  print(paste("minsplit =", minsplit, "maxdepth = ", maxdepth, 
              "splitFunction=", splitFunction, "listofK = ", listofK, 
              "listofL = ",listofL, "limiar de corte = ", limiarDeCorte))
  
  #Gera os indices para o 10-fold cross validation
  numeroLinhas <- nrow(dados)
  indices <- seq(from=0, to=numeroLinhas, by=118)
  folds <- seq(from=1, to=64, by=7)
  folds[11] <- 74
  
  tb <- table(c(0,1),c(0,1))
  
  tb[4] <- tb[4] - 1
  tb[1] <- tb[1] - 1
  
  for(i in seq(folds)){
    if(is.na(indices[folds[i+1]]) == FALSE) {
      print(paste(i, " - i: ",indices[folds[i]],"i+1: ",indices[folds[i+1]]))
      print(" ")
      
      #Teste e treino
      testing   <- subset(dados, idunico > indices[folds[i]] & idunico <= indices[folds[i+1]])
      training  <- subset(dados, idunico <= indices[folds[i]] | idunico > indices[folds[i+1]])
      
     # print(nrow(testing))
    #  print(testing$idunico)
      
    #  print("------------------------------------------")
    #  print(nrow(training))
    #  print(training$idunico)
      
      
    #  print("------------------------------------------")
     # print("------------------------------------------")
      
      position <- which(colnames(testing)=="Class")
      
      #Comandos rotationForest
      controle <- rpart.control(minsplit=minsplit, maxdepth=maxdepth, parms = list(split = splitFunction))#cp=2, 
      
      #training[position] precisa ser um factor pra funcionar
      converteFator <- as.factor(ifelse(training$Class=="1",1,0))
      rF <- rotationForest(x=training[-position], y=converteFator, control = controle, K = listofK, L = listofL)
      
      predictions <- predict(object=rF, newdata=testing[-position], all=FALSE)
      
      #como converter isso em uma matriz de confusao?
      #print(predictions)
      
      #Comandos rotationForest
      #predictions <- predictions
      predictions[which(predictions < limiarDeCorte)] <- 0
      predictions[which(predictions >= limiarDeCorte)] <- 1
      
      
      predictions <- as.character(predictions)
      testing$Class <-  as.character(testing$Class)
      
      numTRUE   <- nrow(as.data.frame(subset(predictions, predictions == "1")))
      numFALSE  <- nrow(as.data.frame(subset(predictions, predictions == "0")))
      
      print("porco")
      if(numTRUE == 0){
        #print(predictions)
        #print(testing$Class)
        
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
  print(tb)
  
  #Encontra os melhores parametros
  params[2] <<- minsplit
  params[4] <<- maxdepth
  params[6] <<- splitFunction
  params[8] <<- listofK
  params[10] <<- listofL
  params[12] <<-  limiarDeCorte
  
  salvaArquivo("outputSVMClassifications.txt", params, tb)
  
  if(is.null(vetorBest) == TRUE) {
    
    #valores
    vetorBest <<- tb
    vetorBest[1] <<- tb[1]
    vetorBest[2] <<- tb[2]
    vetorBest[3] <<- tb[3]
    vetorBest[4] <<- tb[4]
    #params
    params[2] <<- minsplit
    params[4] <<- maxdepth
    params[6] <<- splitFunction
    params[8] <<- listofK
    params[10] <<- listofL
    params[12] <<- limiarDeCorte
    
    
  }else{
    
    if(tb[1] > vetorBest[1] && tb[4] > vetorBest[4]){
      
      #valores
      vetorBest[1] <<- tb[1]
      vetorBest[2] <<- tb[2]
      vetorBest[3] <<- tb[3]
      vetorBest[4] <<- tb[4]
      #params
      params[2] <<- minsplit
      params[4] <<- maxdepth
      params[6] <<- splitFunction
      params[8] <<- listofK
      params[10] <<- listofL
      params[12] <<- limiarDeCorte
      
    }    
  } 
}


dados <- leArquivoCSV("C:\\Users\\Gimayma\\Desktop\\RotationForest\\dados.csv")

#Parametros globais
vetorBest <-c()
params <-c()
params[1] <- "minsplit"
params[3] <- "maxdepth"
params[5] <- "splitFunction"
params[7] <- "listofK"
params[9] <- "listofL"
params[11] <- "limiarDeCorte"

vartmp <- 1
if(vartmp == 0){
  
  TestesRotationForest(dados, 
                       minsplit= 10, 
                       maxdepth=10, 
                       splitFunction='information',
                       listofK=10, 
                       listofL=10,
                       limiarDeCorte=0.5,
                       vetorBest,
                       params)
  
  
  
  
}else{
  
  minsplit<-c(seq(from = 3, to = 30, by =4))
  maxdepth <- c(seq(from = 3, to = 30, by =4))
  splitFunction <- c("information", "gini")
  listofK <- c(1,5,10)
  listofL <- c(1,5,10)
  listofCorte <- c(0.25, 0.50, 0.75)
  
  for(minSpl in seq(minsplit)){
    for(maxdep in seq(maxdepth)){
      for(spFunc in seq(splitFunction)){
        for(lsK in seq(listofK)){
          for(lsL in seq(listofL)){
            for(lim in seq(listofCorte)){
              
              tryCatch({  TestesRotationForest(dados, 
                                               minsplit= minsplit[minSpl], 
                                               maxdepth=maxdepth[maxdep], 
                                               splitFunction=splitFunction[spFunc],
                                               listofK=listofK[lsK], 
                                               listofL=listofL[lsL],
                                               limiarDeCorte=listofCorte[lim],
                                               vetorBest,
                                               params)
              })
            }
            
          } 
        } 
      } 
    }  
  }
  
  
}

#Desabilita warnings
options(warn=-1)

#data(iris)
#print(iris)
#print(iris$Species[1:100])
#y <- as.factor(ifelse(iris$Species=="setosa",0,1))
#x <- iris[,-5]
#print(class(y))
#print(class(x))
#controle <- rpart.control(minsplit=1, maxdepth=11, parms = list(split = "gini"))#cp=2, 

#rF <- rotationForest(x, y, control = controle, K = 2, L = 3)
#objetoPredito <- predict(object=rF, newdata=x, all=FALSE)

#print(objetoPredito)