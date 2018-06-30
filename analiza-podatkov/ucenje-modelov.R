# ucenje modelov, izbira modela
# Anze Marinko, 27151124
# Izbrane teme iz analize podatkov

####### uvoz vseh potrebnih knjižnic in spremenljivk #####
library(caret)
library(randomForest)

source("analiza-podatkov/uvoz-in-transformacija.r", encoding = "UTF-8")
# zdaj imamo: trainX, trainY, testX

####### preverjanje natančnosti ############
izracunAcc <- function(op) {
  truePos <- sum(op$pred==op$obs & op$obs==levels(op$obs)[1])
  trueNeg <- sum(op$pred==op$obs & op$obs==levels(op$obs)[2])
  acc <- (truePos + trueNeg) / length(op$obs)
  names(acc) <- 'Acc'
  return(acc)
}

######## definicije napovednih modelov ###############

# svm model (izbere najbolj natančnega izmed naučenih)
najSVMModel <- function(trainX, trainY){
  
  trainIndex <- createDataPartition(trainY, p=0.8, list=FALSE)
  trainData <- cbind(trainX[trainIndex,], Lodobren=trainY[trainIndex])
  testniX <- trainX[-trainIndex,]
  testniY <- trainY[-trainIndex]
  print("ucenje svm modelov (C=e^c)")
  acc <- vector()
  for (c in c(-3,-1,1,3,5)) {
    linSvm <- train(Lodobren~., data=trainData,
                    method = 'svmLinear',
                    scale = FALSE,
                    trControl = trainControl(method='cv'),
                    tuneGrid = data.frame(C=exp(c)))
    pred <- predict(linSvm, testniX)
    acc[(c+5)/2] <- izracunAcc(data.frame(pred, obs=testniY))
    print(paste("svm c=",c," acc=",acc[(c+5)/2],sep=""))
  }
  optC <- c(-3,-1,1,3,5)[acc==max(acc)][1]
  print(paste("svm optC=",optC,sep=""))
  linSvm <- train(Lodobren~., data=trainData,
                  method = 'svmLinear',
                  scale = FALSE,
                  trControl = trainControl(method='cv'),
                  tuneGrid = data.frame(C=exp(optC)))
  return(linSvm)
}

# random forest model
rfModel <- function(trainX, trainY) {
  trainIndex <- createDataPartition(trainY, p=0.8, list=FALSE)
  trainDataX <- trainX[trainIndex,]
  trainDataY <- trainY[trainIndex]
  testniX <- trainX[-trainIndex,]
  testniY <- trainY[-trainIndex]
  print("ucenje rf modela")
  rf <- randomForest(trainDataX, y=trainDataY, ntree=80, mtry=30,
                        maxnodes = 500)
  
  plot(rf)
  pred <- predict(rf, testniX)
  acc <- izracunAcc(data.frame(pred, obs=testniY))
  print(paste("Random forest acc=",acc,sep=""))
  rf <- randomForest(trainX, y=trainY, ntree=80, mtry=30,
                     maxnodes = 500)
  return(rf)
}

############## gradnja modelov ##############

svmModel <- najSVMModel(trainX,trainY)
pred1 <- predict(svmModel, trainX)
accT <- izracunAcc(data.frame(pred1, obs=trainY))
print(paste("    SVM (na učnih podatkih) acc=",accT,sep=""))
# opt. je pri c=5: acc=0.709331131296449 

randomForestModel <- rfModel(trainX, trainY)
plot(randomForestModel)
pred2 <- predict(randomForestModel, trainX)
accT <- izracunAcc(data.frame(pred2, obs=trainY))
print(paste("    RF (na učnih podatkih) acc=",accT,sep=""))
# = 0.8992082 

############ izbira modela #############
finalModel <- randomForestModel
