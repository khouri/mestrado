
library(earth)#MARS.
library(kernlab)#SVM para regressao.
library(caret)#Para o KNN e outros algoritmos de IA.
library(nnet)#Para a rede neural.
library(glmnet)#RIDGE REGRESSION
library(lars)

sementeAleatoria <- 857

leArquivoCSV <- function(path) {
  return(read.table(path, header = TRUE, sep = ","))
}


#Least Absolute Shrinkage and Selection Operator "larso"
#http://machinelearningmastery.com/non-linear-regression-in-r/
TestesRegressaoRIDGEREGRESSION<-function(ConjuntoTeste, ConjuntoTreino){
  
  set.seed(sementeAleatoria)
  
  #Conjuntos de treinamento e testes
  training <- ConjuntoTreino
  position <- which(colnames(training)=="Class")
  #x <- as.matrix(training[-position])
  #y <- as.matrix(training[position])
  #print(y)
  testing <- ConjuntoTeste
    
  fit <- lars(training, Class~., type="lasso")
  
  # select a step with a minimum error
  best_step <- fit$df[which.min(fit$RSS)]
  
  # make predictions
  predictions <- predict(fit, x, s=best_step, type="fit")$fit
  
  
  # Cria tabela de predicao
  #tb <- table(predictions, testing$Class)
  
  print(predictions)
  #Grava arquivo
  #sink('/home/toasty/Desktop/RBooks/Experimentos/Predicao/outputLM.txt', append=TRUE)
  #print(paste("valor de numNeuronios =", numNeuronios, "e txAprendizagem = ", txAprendizagem))
  #print(tb)
  #print(summary(tb))
  
  #print("--------------------------------------------------")
  #sink()
}



#
# Chamadas de testes
#

conjuntoTeste <- leArquivoCSV("/home/toasty/Desktop/RBooks/Experimentos/Predicao/glmnet/GLMNETsaidaBalanceada_teste.csv") 
conjuntoTreino <- leArquivoCSV("/home/toasty/Desktop/RBooks/Experimentos/Predicao/glmnet/GLMNETsaidaBalanceada_treino.csv")

#TestesRegressaoMARS(conjuntoTeste, conjuntoTreino)
#TestesRegressaoBinomial(conjuntoTeste, conjuntoTreino)
#TestesRegressaoSVRegression(conjuntoTeste, conjuntoTreino)
#TestesRegressaoKNN(conjuntoTeste, conjuntoTreino)
#TestesRegressaoNNET(conjuntoTeste, conjuntoTreino)
TestesRegressaoRIDGEREGRESSION(conjuntoTeste, conjuntoTreino)




#Desabilita warnings
options(warn=-1)