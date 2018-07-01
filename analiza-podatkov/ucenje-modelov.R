# ucenje modelov, izbira modela
# Anze Marinko, 27151124
# Izbrane teme iz analize podatkov

####### uvoz vseh potrebnih knjižnic in spremenljivk #####
library(caret)
library(randomForest)
library(mxnet)

source("analiza-podatkov/uvoz-in-transformacija.r", encoding = "UTF-8")
# zdaj imamo: trainX, trainY, testX

####### preverjanje natančnosti ############

# cena napake
customPenalty <- function(data, levels, ...){
  # napaka tipa 2 = "false negative"
  # napaka tipa 1 = "false positive"
  falseNegative <- sum(data$pred==levels[1] & data$obs == levels[2])
  actualPositive <- sum(data$obs==levels[2])
  fnr <- falseNegative / actualPositive
  falsePositive <- sum(data$pred==levels[2] & data$obs == levels[1])
  actualNegative <- sum(data$obs==levels[1])
  fpr <- falsePositive / actualNegative
  
  cost = 2 * fpr + fnr
  names(cost) <- 'cost'
  cost
}

#Traincontrol
trC <- trainControl(method = "cv",
                    number = 5,
                    savePredictions = TRUE,
                    classProbs = FALSE,
                    summaryFunction = customPenalty)

######## definicije napovednih modelov ###############
# print("učenje modelov")

# svm model (izbere najbolj natančnega izmed naučenih)
SVMModel <- function(trainX, trainY){
  # print("svm")
  linSvm <- train(Y~., data=data.frame(trainX, Y=trainY),
                  method = 'svmLinear',
                  metric = "cost",
                  maximize = "false",
                  scale = FALSE,
                  trControl = trC,
                  tuneGrid = data.frame(C=exp(c(-7,-3,1,5,9))))
  return(linSvm)
}

# random forest model
randomForestModel <- function(trainX, trainY) {
  # print("rf")
  rf <- randomForest(trainX, y=trainY, ntree=200, mtry=20,
                     maxnodes = 600)
  return(rf)
}

# knn model
knnModel <- function(trainX, trainY) {
  # print("knn")
  knnM <- train(Y~., data = data.frame(trainX, Y=trainY),
                method = "knn",
                metric = "cost",
                maximize = "false",
                trControl = trC,
                tuneGrid = data.frame(k=2:7))
  return(knnM)
}

# glm model
glmModel <- function(trainX, trainY) {
  # print("glm")
  glmM <- train(trainX, trainY,
                method = "glm",
                metric = "cost",
                maximize = "false",
                trControl = trC)
  return(glmM)
}

# nevronske mreže
neuronNetworkModel <- function(trainX, trainY) {
  input <- mx.symbol.Variable("data")
  layer1 <- mx.symbol.FullyConnected(input, num_hidden=200)
  act1 <- mx.symbol.Activation(layer1, act_type="relu")
  layer2 <- mx.symbol.FullyConnected(act1, num_hidden=100)
  act2 <- mx.symbol.Activation(layer2, act_type="relu")
  layer3 <- mx.symbol.FullyConnected(act2, num_hidden=3)
  softmax <- mx.symbol.SoftmaxOutput(layer3)
  
  mx.set.seed(0)
  nnM <- mx.model.FeedForward.create(softmax, X=data.matrix(trainX), y=trainY,
                                           num.round=10, array.batch.size=400,
                                           learning.rate=0.07, momentum=0.9,  eval.metric=mx.metric.accuracy,
                                           initializer=mx.init.uniform(0.01),
                                           batch.end.callback=mx.callback.log.train.metric(100))
  # najdi kako narises (nnM$symbol)
  # vceraj nasel ob iskanju predict funkcije
  pred <- predict(nnM, data.matrix(testX))
  return(nnM)
}


# print("")
############## gradnja modelov ##############

# svmM <- SVMModel(trainX,trainY)
# cost = 
# print(paste("svm cost=",svmM$results$cost))

rfM <- randomForestModel(trainX, trainY)
# na testnih podatkih cost=0.371465190168879
# (vecinoma (90%) med 0.35 in 0.45 za particijo p=0.8, pride pa tudi do 0.88 in 0.32)
pred <- predict(rfM, trainX)
cost <- customPenalty(data.frame(pred=pred,obs=trainY), levels(trainY))
# print(paste("rf cost= ",cost,sep=""))
# plot(rfM) # na ucnih podatkih cost = 0.015716845343239 (tudi do 0.00893608977333265)

# knnM <- knnModel(trainX, trainY)
# print(paste("knn cost=",knnM$results$cost))
# pred <- predict(knnM, trainX)
# cost <- customPenalty(data.frame(pred=pred,obs=trainY), levels(trainY))
# print(paste("knn cost= ",cost,sep=""))
# cv, k=2: cost=1.08113432572012
# ucni cost = 0.651930484819049

# glmM <- glmModel(trainX, trainY)
# cv cost = 0.80650758890118
# print(paste("glm cost=",glmM$results$cost))
# pred <- predict(glmM, trainX)
# cost <- customPenalty(data.frame(pred=pred,obs=trainY), levels(trainY))
# print(paste("glm cost= ",cost,sep=""))
# ucni cost=0.776023195845984

# nnM <- neuronNetworkModel(trainX, trainY)
# cost = 

############ izbira modela #############
finalModel <- rfM
