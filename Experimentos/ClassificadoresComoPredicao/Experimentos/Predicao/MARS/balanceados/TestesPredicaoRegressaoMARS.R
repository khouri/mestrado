
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


#Multivariate Adaptive Regression Splines, precisa parametrizar
#http://machinelearningmastery.com/non-linear-regression-in-r/
TestesRegressaoMARS<-function(ConjuntoTeste, ConjuntoTreino){
  
  set.seed(sementeAleatoria)

  #Conjuntos de treinamento e testes
  training <- ConjuntoTreino#variavelData[inTrain,]
  testing <- ConjuntoTeste #variavelData[-inTrain,]
  position <- which(colnames(testing)=="Class")
  
  fit <- earth(Class~., data=training)
  
  # Constroi modelo para classificacao
  predictions <- predict(fit, testing[-position], type="response")
  
  # Cria tabela de predicao
  tb <- table(predictions, testing$Class)
  
  #print(predictions)
  #print(testing$Class)
  
  
  TestaDiferentesLimiares(predictions, testing$Class, "outputMARS")
  
  #Grava arquivo
  #sink('/home/toasty/Desktop/RBooks/Experimentos/Predicao/outputLM.txt', append=TRUE)
  #print(paste("valor de numNeuronios =", numNeuronios, "e txAprendizagem = ", txAprendizagem))
  #print(tb)
  #print(summary(tb))
  
  #print("--------------------------------------------------")
  #sink()
}



TestaDiferentesLimiares<-function(ValoresPreditos, valoresDeTestes, nomeArquivoSaida){
  
  counter <- 0
  counter2 <- 0
 # print(ValoresPreditos)
#  print("\n")
  for( x in 1:length(ValoresPreditos)) {
    
    matrizTmp <- ValoresPreditos
    for( y in 1:length(ValoresPreditos)) {
      
      limiar <- ValoresPreditos[x]
      #cria if com a pergunta e roda os exemplos
      if(ValoresPreditos[y] < limiar){
        matrizTmp[y] <- 0
      }
      else{
        matrizTmp[y] <- 1
      }
      
      counter <- counter + 1
    }
   counter2 <- counter2 + 1
   
   print(valoresDeTestes)
   print(class(matrizTmp))
   
   #gravaa primeira matriz de confusao
    tb <- table(matrizTmp, valoresDeTestes)
    #print(tb)
    #print(matrizTmp)

     #Grava arquivo
     sink(paste('/home/toasty/Desktop/RBooks/Experimentos/Predicao/', nomeArquivoSaida, '.txt'), append=TRUE)
     print(tb)
     print(paste("limiar de corte: ", limiar))
     print("--------------------------------------------------")
     sink()
  
  
  }




  #print(counter)
  #print(counter2)
  
}


#
# Chamadas de testes
#

conjuntoTeste <- leArquivoCSV("/home/toasty/Desktop/RBooks/Experimentos/Predicao/saidaBalanceada_teste.csv") 
conjuntoTreino <- leArquivoCSV("/home/toasty/Desktop/RBooks/Experimentos/Predicao/saidaBalanceada_treino.csv")

TestesRegressaoMARS(conjuntoTeste, conjuntoTreino)








#Desabilita warnings
options(warn=-1)