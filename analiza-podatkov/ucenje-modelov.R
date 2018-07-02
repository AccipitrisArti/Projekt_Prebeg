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
                    number = 10,
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
                  trControl = trainControl(method = "cv",
                                           number = 3,
                                           savePredictions = TRUE,
                                           classProbs = FALSE,
                                           summaryFunction = customPenalty),
                  tuneGrid = data.frame(C=exp(c(-3,5,12,15,18))))
  return(linSvm)
}
tsvm1 <- Sys.time()
svmM <- SVMModel(trainX, trainY)
tsvm2 <- Sys.time()

# random forest model
randomForestModel <- function(trainX, trainY) {
  # print("rf")
  rf <- train(Y~., data=data.frame(trainX, Y=trainY),
                method="rf", metric="cost", ntree=200,
                tuneGrid=expand.grid(.mtry=c(10)), trControl=trC)
  # rf <- randomForest(trainX, y=trainY, ntree=200, mtry=20,
  #                    maxnodes = 600)
  return(rf)
}
trf1 <- Sys.time()
rfM <- randomForestModel(trainX, trainY)
trf2 <- Sys.time()

# knn model
knnModel <- function(trainX, trainY) {
  # print("knn")
  knnM <- train(Y~., data = data.frame(trainX, Y=trainY),
                method = "knn",
                metric = "cost",
                maximize = "false",
                trControl = trC,
                tuneGrid = data.frame(k=2:8))
  return(knnM)
}
tknn1 <- Sys.time()
knnM <- knnModel(trainX,trainY)
tknn2 <- Sys.time()

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
tglm1 <- Sys.time()
glmM <- glmModel(trainX,trainY)
tglm2 <- Sys.time()


# print("")
############## gradnja modelov ##############

# modeli: svmM, rfM, knnM, glmM

casi <- c(tsvm2-tsvm1,
          trf2-trf1,
          tknn2-tknn1,
          tglm2-tglm1)

############ izbira modela #############
finalModel <- rfM
