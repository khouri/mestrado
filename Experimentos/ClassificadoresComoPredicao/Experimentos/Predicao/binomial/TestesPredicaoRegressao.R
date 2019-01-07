
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

conjuntoTeste <- leArquivoCSV("/home/toasty/Desktop/RBooks/Experimentos/Predicao/saidaBalanceada_teste.csv") 
conjuntoTreino <- leArquivoCSV("/home/toasty/Desktop/RBooks/Experimentos/Predicao/saidaBalanceada_treino.csv")

TestesRegressaoBinomial(conjuntoTeste, conjuntoTreino)
#TestesRegressaoMARS(conjuntoTeste, conjuntoTreino)
#TestesRegressaoSVRegression(conjuntoTeste, conjuntoTreino)#Deu merda!!!


#TestesRegressaoKNN(conjuntoTeste, conjuntoTreino)


#TestesRegressaoNNET(conjuntoTeste, conjuntoTreino)
#TestesRegressaoCART(conjuntoTeste, conjuntoTreino)






#Desabilita warnings
options(warn=-1)