# uvoz knjiznic, uvoz in transformacija ucnih podatkov
# Anze Marinko, 27151124
# Izbrane teme iz analize podatkov

library(readr)
library(dplyr)

uvoziUcnePodatke <- function() {
  data <- read_csv("orange-churn-train.csv",
                   locale = locale(encoding = "Windows-1250"))
  data$churn <- factor(data$churn)
  return(data)
}
trainData <- uvoziUcnePodatke()
trainX <- trainData[, colnames(trainData)!="churn"]
trainY <- trainData$churn

