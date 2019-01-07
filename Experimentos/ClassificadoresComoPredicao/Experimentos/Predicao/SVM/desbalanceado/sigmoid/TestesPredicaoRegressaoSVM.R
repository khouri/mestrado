
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


#Usa o SVM para regressao, precisa parametrizar
#http://machinelearningmastery.com/non-linear-regression-in-r/
#Falta usar mais parametros no caso do SVM
TestesRegressaoSVRegressionRBF<-function(ConjuntoTeste, ConjuntoTreino, c, coef, sigma){
  
  set.seed(sementeAleatoria)
  
  #Conjuntos de treinamento e testes
  training <- ConjuntoTreino#variavelData[inTrain,]
  testing <- ConjuntoTeste #variavelData[-inTrain,]
  position <- which(colnames(testing)=="Class")
  
  fit <- ksvm(Class~., 
              data=training, 
              type="nu-svr", 
              C=c, 
              kernel="tanhdot",
              coef=coef#,
              #kpar=list(sigma=sigma)#, degree= 0.04
              )
  
  # Constroi modelo para classificacao
  predictions <- predict(fit, testing[-position], type="response")
  
  TestaDiferentesLimiares(predictions, testing$Class, "outputSVMSIGMOID", c, coef, sigma)
  
  # Cria tabela de predicao
  #tb <- table(predictions, testing$Class)
  
  #Grava arquivo
  ##sink(paste('/home/toasty/Desktop/RBooks/Experimentos/Predicao/outputSVM.txt'), append=TRUE)
  #print(tb)
  #print(paste("limiar de corte: ", limiar))
  #print("--------------------------------------------------")
  #sink()
}


TestaDiferentesLimiares<-function(ValoresPreditos, valoresDeTestes, nomeArquivoSaida, c, coef, sigma){
  
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
    print(paste("c: ", c, "coef: ", coef, "sigma: ", sigma))
    print("--------------------------------------------------")
    sink()
    
    
  }
  
  
  
  
  #print(counter)
  #print(counter2)
  
}


#
# Chamadas de testes
#

conjuntoTeste <- leArquivoCSV("/home/toasty/Desktop/RBooks/Experimentos/Predicao/saidaDesBalanceada_teste.csv") 
conjuntoTreino <- leArquivoCSV("/home/toasty/Desktop/RBooks/Experimentos/Predicao/saidaDesBalanceada_treino.csv")

valoresDeC<-c( 0.01, 0.1, 1, 2, 3, 4, 5, 10)
valoresDeSigma <- c(0.1, 0.3, 0.5, 0.0001, 0.001, 0.01,  0.1, 1, 2, 3, 4, 5, 10)
valoresDeCoef0 <- c(0.1, 0.3, 0.5, 0.0001, 0.001, 0.01,  0.1, 1, 2, 3, 4, 5, 10)

for(c in seq(valoresDeC)){
  for(sigma in seq(valoresDeSigma)){
    for(coef0 in seq(valoresDeCoef0)){
      
      print(paste(c = ",valoresDeC[c]" , "gamma=",valoresDeSigma[sigma], "coef0 = ", valoresDeCoef0[coef0]))
      
      tryCatch({TestesRegressaoSVRegressionRBF(conjuntoTeste, conjuntoTreino, 
                                               valoresDeC[c], valoresDeCoef0[coef0], valoresDeSigma[sigma])})
      
    } 
  } 
} 



#Desabilita warnings
options(warn=-1)