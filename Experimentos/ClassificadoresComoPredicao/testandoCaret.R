library(caret)
library(mlbench)
library(e1071)
library(kernlab)
library(klaR)


data(Sonar)
data(BloodBrain)
data(iris)


testeFolds<- function (){
  
  #como ele entendeu que eram 10?
  folds <- createFolds(k=5, Sonar$Class)
  str(folds)
  
  print(folds)
}


#Executa nnet para varios conjuntos de dados.
geraMatrizConfusao <- function(true, pred){
  tp <- 0
  fp <- 0
  fn <- 0
  tn <- 0
  
  count <- 1
  size <- 1:length(true)
  
  for(count in seq(along=size)){
    if(true[count]==1){
      if(pred[count]==1){
        tp <- tp+1
      }else{
        fp <- fp+1
      }
    }else{
      if(pred[count]==2){
        tn <- tn+1
      }else{
        fn <- fn+1
      }
    }
    count <- count+1
  }
  
  retorno <- matrix(c(tp,fp,fn,tn),nrow=2, ncol=2)
  colnames(retorno) <- c(0,1)
  rownames(retorno) <- c(0,1)
  as.table(retorno)
}



testeRedeNeural<- function (){
  
  set.seed(107)
  inTrain <- createDataPartition(y = Sonar$Class,
                                 ## the outcome data are needed
                                 p = .90,
                                 ## The percentage of data in the
                                 ## training set
                                 list = FALSE)

  training <- Sonar[inTrain,]
  testing <- Sonar[-inTrain,]
  
  #parametros do treinamento
  nnetGrid <- expand.grid(.size = 1:10, .decay = c( 0.1, 1, 2))
  maxSize <- max(nnetGrid$.size)
  numWts <- 1*(maxSize * (length(training) + 1) + maxSize + 1)
  
  ctrl <- trainControl(method = "repeatedcv",
                       repeats = 3,
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary
                       )
  
  nnetFit <- train(x = training,
                   y = training$Class,
                   method = "nnet",
                   metric = "ROC",
                  # preProc = c("center", "scale", "spatialSign"),
                   tuneGrid = nnetGrid,
                   trace = FALSE,
                   maxit = 2000,
                   MaxNWts = numWts,
                   ## ctrl was defined in the previous chapter
                   trControl = ctrl)
  
  #Usa apenas o conjunto de treino para predizer.
  plsClasses <- predict(nnetFit, newdata = testing)
  results <- confusionMatrix(data=plsClasses, reference=testing$Class, dnn = c("Prediction", "Reference"))

}

testeSVM <- function(){
  
  set.seed(107)
  inTrain <- createDataPartition(y = Sonar$Class,
                                 ## the outcome data are needed
                                 p = .90,
                                 ## The percentage of data in the
                                 ## training set
                                 list = FALSE)
  
  training <- Sonar[inTrain,]
  testing <- Sonar[-inTrain,]
  
  ctrl <- trainControl(method = "repeatedcv",
                       repeats = 3,
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary
  )
  
  set.seed(202)
  svmRGridReduced <- expand.grid(.sigma = 1, .C = 2^(1))
  
  svmRModel <- train(training, training$Class,
                     method = "svmRadial",
                     metric = "ROC",
                     #preProc = c("center", "scale"),
                     tuneGrid = svmRGridReduced,
                     fit = FALSE,
                     trControl = ctrl)
  print(svmRModel)
  
  predict(svmRModel, newdata = testing)
  #predict(svmRModel, newdata = testing, type = "prob")

}

testeSVM2<-function(){
  
  inTrain <- createDataPartition(y = iris$Species, # which columns is the outcome data
                                 p = .75,          # training set relative size
                                 list = FALSE)
  training <- iris[ inTrain,]
  testing  <- iris[-inTrain,]
  
  #svm_fit <- train(Species ~ .,
  #                 data = training,
  #                 method = "svmLinear",
  #                 preProcess = c("center", "scale"))
 
  posicao <- which(colnames(training)=="Species")
  print(training[-posicao])   #training["Species"])
  
  svm_fit <- train(training$Species ~ .,
                   data = training[-posicao],
                   method = "svmLinear"#,
                   #preProcess = c("center", "scale")
                   )
  
  # print(svm_fit)
  
  #print(iris)
  #test_pred <- predict(svm_fit, newdata=testing)
  #mat <- confusionMatrix(data=test_pred, testing$Species)
  #print(mat)
}


testeSVM3 <- function(){
  
  set.seed(107)
  inTrain <- createDataPartition(y = Sonar$Class,
                                 ## the outcome data are needed
                                 p = .90,
                                 ## The percentage of data in the
                                 ## training set
                                 list = FALSE)
  
  training <- na.omit(Sonar[inTrain,])
  testing <- na.omit(Sonar[-inTrain,])
  
  ctr <- trainControl(method='repeatedcv',
                      repeats = 3,
                      classProbs = FALSE,
                      summaryFunction = twoClassSummary
                      )
  
#  training, training$Class,
  #fac <- factor(training$Class)
  svm.c <- train(training$Class, training,
                 method='svmRadial',
                 trControl=ctr,
                 metric="ROC")
  
  yhat.c <- predict(svm.c, testing)
  mama <- confusionMatrix(yhat.c, testing)
  print(mama)

}


testeNaiveBayes<-function() {
  inTrain <- createDataPartition(y = iris$Species, # which columns is the outcome data
                                 p = .75,          # training set relative size
                                 list = FALSE)
  training <- iris[ inTrain,]
  testing  <- iris[-inTrain,]
  
  model <- NaiveBayes(Species~., data=training)
  predictions <- predict(model, testing)
  matrix <- confusionMatrix(predictions$class, testing$Species)
  print(matrix)
}

testeNaiveBayes02<-function() {
  
  inTrain <- createDataPartition(y = iris$Species, # which columns is the outcome data
                                 p = .75,          # training set relative size
                                 list = FALSE)
  training <- iris[ inTrain,]
  testing  <- iris[-inTrain,]
  
  train_control <- trainControl(method="cv", number=10)
  model <- train(Species~., data=training, trControl=train_control, method="nb")
  # make predictions
  predictions <- predict(model, testing)
  
  matt <- confusionMatrix(predictions, testing$Species)
  print(matt)
}

testesCART<- function() {

  inTrain <- createDataPartition(y = iris$Species, # which columns is the outcome data
                                 p = .75,          # training set relative size
                                 list = FALSE)
  training <- iris[ inTrain,]
  testing  <- iris[-inTrain,]
  
  set.seed(857)
  ctrl <- trainControl(method="cv", number=10)
  rpFitCost <- train(x = training,
                       y = training$Species,
                       method = "rpart",
                       #metric = "Cost",
                       maximize = FALSE,
                       #tuneLength = 20,
                       ## rpart structures the cost matrix so that
                       ## the true classes are in rows, so we
                       ## transpose the cost matrix
                      # parms =list(loss = t(costMatrix)),
                       trControl = ctrl)
                       
  predictions <- predict(rpFitCost, testing)
  
  matt <- confusionMatrix(predictions, testing$Species)
  print(matt)
}

testeCART02<-function(){
  
  inTrain <- createDataPartition(y = iris$Species, # which columns is the outcome data
                                 p = .75,          # training set relative size
                                 list = FALSE)
  training <- iris[ inTrain,]
  testing  <- iris[-inTrain,]
  
  set.seed(857)
  ctrl <- trainControl(method="cv", number=10)
  
  formula <- as.formula(Species ~.)
  t <- train(formula,training,method = "rpart",cp=0.002,maxdepth=8)
  
  predictions <- predict(t , testing)
  
  matt <- confusionMatrix(predictions, testing$Species)
  print(matt)
  #plot(t)
}
#testeRedeNeural()
#testeSVM2()
#testeNaiveBayes02()

#testesCART()

#testeCART02()

testesKNN<- function() {
  set.seed(400)
  inTrain <- createDataPartition(y = iris$Species, # which columns is the outcome data
                                 p = .75,          # training set relative size
                                 list = FALSE)
  training <- iris[ inTrain,]
  testing  <- iris[-inTrain,]
  
  set.seed(857)
  ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
 
  knnFit <- train(training$Species~., data = training, method = "knn", trControl = ctrl, 
                  preProcess = c("center","scale"), tuneLength = 20)
  
  knnPredict <- predict(knnFit, newdata = testing )
  #Get the confusion matrix to see accuracy value and other parameter values
  mat <- confusionMatrix(knnPredict, testing$Species )
  print(mat)
}



testesKNN()









