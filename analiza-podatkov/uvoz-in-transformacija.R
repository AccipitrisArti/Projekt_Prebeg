# uvoz knjiznic, uvoz in transformacija ucnih podatkov
# Anze Marinko, 27151124
# Izbrane teme iz analize podatkov

library(readr)
library(dplyr)
library(caret)
library(dummies)

uvoziUcnePodatke <- function() {
  data <- read_csv("Podatki/orange-churn-train.csv",
                   locale = locale(encoding = "Windows-1250"))
  data$churn <- factor(data$churn)
  for (i in 1:42) {
    data[,i] <- as.numeric(data[[i]])
    data[,i] <- 1000*(data[,i]-min(na.omit(data[,i])))/max(na.omit(data[,i]))
  }
  # new.data <- data[,1:42]
  for (i in 43:76) {
    data[,i] <- as.factor(data[[i]])
    # iz kategoricnih v numericne
    # dmy <- dummyVars(" ~ .", data[,i])
    # new.data <- bind_cols(new.data, data.frame(predict(dmy, newdata = data[,i])))
  }
  # data <- bind_cols(new.data, data$churn)
  return(data)
}
trainData <- uvoziUcnePodatke()
trainX <- trainData[, colnames(trainData)!="churn"]
trainY <- trainData$churn

