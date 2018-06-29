# ucenje modelov, podatkovno rudarjenje
# Anze Marinko, 27151124
# Izbrane teme iz analize podatkov

library(caret)
library(randomForest)

source("analiza-podatkov/uvoz-in-transformacija.r", encoding = "UTF-8")
# zdaj imamo: trainX, trainY, testX

# funkcije za preverjanje natančnosti, trainControl ...
izracunAcc <- function(op) {
  truePos <- sum(op$pred==op$obs & op$obs==levels(op$obs)[1])
  trueNeg <- sum(op$pred==op$obs & op$obs==levels(op$obs)[2])
  acc <- (truePos + trueNeg) / length(op$obs)
  names(acc) <- 'Acc'
  return(acc)
}
# napovedni modeli
najSVMModel <- function(X, Y){
  
  trainIndex <- createDataPartition(Y, p=0.8, list=FALSE)
  trainData <- cbind(X[trainIndex,], Lodobren=Y[trainIndex])
  teX <- X[-trainIndex,]
  teY <- Y[-trainIndex]
  print("ucenje svm modelov (C=e^c)")
  acc <- vector()
  for (c in c(-3,-1,1,3,5)) {
    linSvm <- train(Lodobren~., data=trainData,
                    method = 'svmLinear',
                    scale = FALSE,
                    trControl = trainControl(method='none'),
                    tuneGrid = data.frame(C=exp(c)))
    pred <- predict(linSvm, teX)
    acc[(c+5)/2] <- izracunAcc(data.frame(pred, obs=teY))
    print(paste("svm c=",c," acc=",acc[(c+5)/2],sep=""))
  }
  optC <- c(-3,-1,1,3,5)[acc==max(acc)][1]
  print(paste("svm optC=",optC,sep=""))
  linSvm <- train(Lodobren~., data=trainData,
                  method = 'svmLinear',
                  scale = FALSE,
                  trControl = trainControl(method='none'),
                  tuneGrid = data.frame(C=exp(optC)))
  return(linSvm)
}
# izbira modela
svmModel <- najSVMModel(trainX,trainY)

rfModel <- randomForest(trainX, y=trainY, ntree=60, mtry=20,
                        maxnodes = 300)
plot(svmModel)
plot(rfModel)

finalModel <- svmModel
