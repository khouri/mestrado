
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


#Usa o NNET para regressao, precisa parametrizar
#http://machinelearningmastery.com/non-linear-regression-in-r/
TestesRegressaoCART<-function(ConjuntoTeste, ConjuntoTreino){
  
  set.seed(sementeAleatoria)
  
  #Conjuntos de treinamento e testes
  training <- ConjuntoTreino
  position <- which(colnames(training)=="Class")
  x <- as.matrix(training[-position])
  y <- as.matrix(training[position])
  print(y)
  testing <- ConjuntoTeste
  
  fit <- rpart(data=training, Class~.)
  
  # Constroi modelo para classificacao
  predictions <- predict(fit, testing[-position])
  
  # Cria tabela de predicao
  #tb <- table(predictions, testing$Class)
  
  print(predictions)
  TestaDiferentesLimiares(predictions, testing$Class, "outputCART")
  #Grava arquivo
  #sink('/home/toasty/Desktop/RBooks/Experimentos/Predicao/outputLM.txt', append=TRUE)
  #print(paste("valor de numNeuronios =", numNeuronios, "e txAprendizagem = ", txAprendizagem))
  #print(tb)
  #print(summary(tb))
  
  #print("--------------------------------------------------")
  #sink()
}

#Usa o NNET para regressao, precisa parametrizar
#http://machinelearningmastery.com/non-linear-regression-in-r/
TestesRegressaoNNET<-function(ConjuntoTeste, ConjuntoTreino, numNeuronios, taxa){
  
  set.seed(sementeAleatoria)
  
  #Conjuntos de treinamento e testes
  training <- ConjuntoTreino
  testing <- ConjuntoTeste
  position <- which(colnames(testing)=="Class")
  
  fit <- nnet(data=training, Class~.,  size=numNeuronios, maxit=500,  decay=taxa, MaxNWts = 1000000)
  
  # Constroi modelo para classificacao
  predictions <- predict(fit, testing[-position])
  
  # Cria tabela de predicao
  #tb <- table(predictions, testing$Class)
  
  print(predictions)
  TestaDiferentesLimiaresNNET(predictions, testing$Class, "outputNNET", numNeuronios, taxa)
  #Grava arquivo
  #sink('/home/toasty/Desktop/RBooks/Experimentos/Predicao/outputLM.txt', append=TRUE)
  #print(paste("valor de numNeuronios =", numNeuronios, "e txAprendizagem = ", txAprendizagem))
  #print(tb)
  #print(summary(tb))
  
  #print("--------------------------------------------------")
  #sink()
}

#Usa o SVM para regressao, precisa parametrizar
#http://machinelearningmastery.com/non-linear-regression-in-r/
TestesRegressaoKNN<-function(ConjuntoTeste, ConjuntoTreino){
  
  set.seed(sementeAleatoria)
  
  #Conjuntos de treinamento e testes
  training <- ConjuntoTreino#variavelData[inTrain,]
  testing <- ConjuntoTeste #variavelData[-inTrain,]
  position <- which(colnames(testing)=="Class")
  
  fit <- knnreg(data=training, Class~., k=3)
  
  # Constroi modelo para classificacao
  predictions <- predict(fit, testing[-position])

  print(predictions)
  print(testing$Class)
  
  #tb <- table(predictions, testing$Class)
  #TestaDiferentesLimiares(predictions, testing$Class, "outputKNN")
  
  #Grava arquivo
  #sink('/home/toasty/Desktop/RBooks/Experimentos/Predicao/outputLM.txt', append=TRUE)
  #print(paste("valor de numNeuronios =", numNeuronios, "e txAprendizagem = ", txAprendizagem))
  #print(tb)
  #print(summary(tb))
  
  #print("--------------------------------------------------")
  #sink()
}

#Usa o SVM para regressao, precisa parametrizar
#http://machinelearningmastery.com/non-linear-regression-in-r/
#Falta usar mais parametros no caso do SVM
TestesRegressaoSVRegression<-function(ConjuntoTeste, ConjuntoTreino){
  
  set.seed(sementeAleatoria)
  
  #Conjuntos de treinamento e testes
  training <- ConjuntoTreino#variavelData[inTrain,]
  testing <- ConjuntoTeste #variavelData[-inTrain,]
  position <- which(colnames(testing)=="Class")
  
  fit <- ksvm(Class~., data=training)
  
  # Constroi modelo para classificacao
  predictions <- predict(fit, testing[-position], type="response")
  
  # Cria tabela de predicao
  tb <- table(predictions, testing$Class)
  
  #Grava arquivo
  sink(paste('/home/toasty/Desktop/RBooks/Experimentos/Predicao/outputNNET.txt'), append=TRUE)
  print(tb)
  #print(paste("limiar de corte: ", limiar))
  print("--------------------------------------------------")
  sink()
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
  
  TestaDiferentesLimiares(predictions, testing$Class, "outputMARS")
  
  #Grava arquivo
  #sink('/home/toasty/Desktop/RBooks/Experimentos/Predicao/outputLM.txt', append=TRUE)
  #print(paste("valor de numNeuronios =", numNeuronios, "e txAprendizagem = ", txAprendizagem))
  #print(tb)
  #print(summary(tb))
  
  #print("--------------------------------------------------")
  #sink()
}

#Regressao binomial, precisa parametrizar
TestesRegressaoBinomial<-function(ConjuntoTeste, ConjuntoTreino){
  
  set.seed(sementeAleatoria)
  
  #Conjuntos de treinamento e testes
  training <- ConjuntoTreino
  testing <- ConjuntoTeste
  position <- which(colnames(testing)=="Class")
  
  fit <- glm( Class~., data =training, family=binomial())
  
  # Constroi modelo para classificacao
  predictions <- predict(fit, testing[-position], type="response")
  
  # Cria tabela de predicao
  tb <- table(predictions, testing$Class)
  
  
  TestaDiferentesLimiares(predictions, testing$Class, "outputBinomial")
  
  
  #print(testing$Class)
  #print(class(predictions))
  #print(predictions)
  #print(predictions[5])
  #print(length(predictions))
  
  #Preciso criar uma funcao que sumarize isso...
  
  
  #Grava arquivo
  #sink('/home/toasty/Desktop/RBooks/Experimentos/Predicao/outputBinomial.txt', append=TRUE)
  #print(tb)
  #print(summary(tb))
  #print(mean(predictions))
  #print("--------------------------------------------------")
  #sink()
}


TestaDiferentesLimiaresNNET<-function(ValoresPreditos, valoresDeTestes, nomeArquivoSaida, numNeuronios, taxa){
  
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
    
    #gravaa primeira matriz de confusao
    tb <- table(matrizTmp, valoresDeTestes)
    #print(tb)
    #print(matrizTmp)
    
    #Grava arquivo
    sink(paste('/home/toasty/Desktop/RBooks/Experimentos/Predicao/', nomeArquivoSaida, '.txt'), append=TRUE)
    print(tb)
    print(paste("neuronios: ", numNeuronios, " aprendizado: ",taxa))
    print("--------------------------------------------------")
    sink()
    
    
  }
  
  
  
  
  #print(counter)
  #print(counter2)
  
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
   
   #gravaa primeira matriz de confusao
    tb <- table(matrizTmp, valoresDeTestes)
    #print(tb)
    #print(matrizTmp)

     #Grava arquivo
     #sink(paste('/home/toasty/Desktop/RBooks/Experimentos/Predicao/', nomeArquivoSaida, '.txt'), append=TRUE)
     #print(tb)
     #print(paste("limiar de corte: ", limiar))
     #print("--------------------------------------------------")
     #sink()
  
  
  }




  #print(counter)
  #print(counter2)
  
}


#
# Chamadas de testes
#

conjuntoTeste <- leArquivoCSV("/home/toasty/Desktop/RBooks/Experimentos/Predicao/saidaDesBalanceada_teste.csv") 
conjuntoTreino <- leArquivoCSV("/home/toasty/Desktop/RBooks/Experimentos/Predicao/saidaDesBalanceada_treino.csv")

#TestesRegressaoBinomial(conjuntoTeste, conjuntoTreino)
#TestesRegressaoMARS(conjuntoTeste, conjuntoTreino)
#TestesRegressaoSVRegression(conjuntoTeste, conjuntoTreino)#Deu merda!!!


#TestesRegressaoKNN(conjuntoTeste, conjuntoTreino)


#TestesRegressaoNNET(conjuntoTeste, conjuntoTreino)
#TestesRegressaoCART(conjuntoTeste, conjuntoTreino)






valoresDeNeuronios<-c(1:40)
valoresDeAprendizagem<-c(0.001,0.0001,0.00001,0.000001,0.0000001)

for(neuronio in seq(valoresDeNeuronios)) {
  for(taxa in seq(valoresDeAprendizagem)) {
    
    TestesRegressaoNNET(conjuntoTeste, conjuntoTreino, valoresDeNeuronios[neuronio], valoresDeAprendizagem[taxa])
    
    print(paste("neuronio=",valoresDeNeuronios[neuronio], "txAprendizagem=",valoresDeAprendizagem[taxa]))
  
  }
}



#Desabilita warnings
options(warn=-1)