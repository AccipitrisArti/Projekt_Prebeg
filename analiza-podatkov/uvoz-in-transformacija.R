# uvoz knjiznic, uvoz podatkov, normalizacija in
# dodajanje manjkajocih podatkov
#    outputi tega dela programa:
#        trainX,
#        trainY,
#        testX,
#        testY
#
# Anze Marinko, 27151124
# Izbrane teme iz analize podatkov

######### uvoz knjižnic #####
library(readr)
library(dplyr)
library(caret)
library(data.table)

########## uvoz in normalizacija podatkov ####
uvoziPodatke <- function() {
  data <- fread(
    "Podatki/leasing-train.csv",
    encoding = "UTF-8")
  # ucili se bomo polnadzorovano (obdelali bomo tudi testne podatke)
  data <- data %>% rbind( fread(
    "Podatki/leasing-test.csv",
    encoding = "UTF-8"))
  for (i in 1:31) {
    if (is.numeric(data[[i]])) {
      data[,i] <- data[[i]] %>% as.numeric()
      # normaliziramo numerične podatke (od 0 do 1000)
      data[,i] <- 1000*(data[[i]]-min(na.omit(data[[i]])))/
        max(na.omit(data[[i]]))
    } else {
      data[,i] <- as.factor(data[[i]])
    }
  }
  return(data)
}

######### nadomeščanje manjkajočih podatkov ########
dodajManjkajoce <- function(data) {
  # v Splacilni_indeks in Szapadlo_neplacano
  manjka <- is.na(data[, c(12,13)])
  # manjkajoce vrednosti v obeh stolpcih na istih mestih
  data[manjka[,1],12] <- mean(na.omit(data[[12]]))
  data[manjka[,2],13] <- mean(na.omit(data[[13]]))
  # dodamo stolpec, ki ve, kje so manjkali podatki (v obeh na istih mestih)
  data$SmanjkaI <- as.factor(as.numeric(manjka[,1]))
  
  return(data)
}

######### odstranjevanje nepomembnih spremenljivk ###########
odstraniNepotrebneSpremenljivke  <- function(data) {
  # konstruiramo variancno-kovariancno matriko za numerične spr.
  # varCov <- matrix(0, ncol = 31, nrow = 31)
  # var <- rep(0, 31)
  # for (i in 1:31) {
  #   varCov[i,i] <- mean((data[[i]]-mean(data[[i]]))^2)
  #   var[i] <- varCov[i,i]
  #   for (j in 1:42) {
  #     varCov[i,j] <- mean((data[[i]]-mean(data[[i]]))*
  #                           (data[[j]]-mean(data[[j]])))
  #     varCov[j,i] <- varCov[i,j]
  #   }
  # }
  
  return(data)
}

######### pogon vseh funkcij za uvoz in obdelavo podatkov #######
data <- uvoziPodatke() %>%
  dodajManjkajoce() %>%
  odstraniNepotrebneSpremenljivke()

trainData <- data[!is.na(data$Lodobren)]
testData <- data[is.na(data$Lodobren)]

######### output ##########
trainX <- trainData[, -1]
trainY <- trainData$Lodobren
testX <- testData[, -1]

