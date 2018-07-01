# uvoz knjižnic, uvoz podatkov, normalizacija in
# nadomeščanje manjkajocih podatkov, dodajanje naivnih
# spremenljivk, odstranjevanje nepomembnih, popravljanje
# razmerja pri ciljni spremenljivki
#    outputi tega dela programa:
#        trainX,
#        trainY,
#        testX
#
# Anze Marinko, 27151124
# Izbrane teme iz analize podatkov

t <- Sys.time()
########## uvoz knjižnic ##################
library(readr)
library(dplyr)
library(data.table)
library(dummies)
library(DMwR)

########## uvoz in normalizacija podatkov ####
uvoziPodatke <- function() {
  data <- fread(
    "Podatki/leasing-train.csv",
    encoding = "UTF-8")
  # ucili se bomo polnadzorovano (obdelali bomo tudi testne podatke)
  data <- data %>% rbind( fread(
    "Podatki/leasing-test.csv",
    encoding = "UTF-8"))
  # nastavimo pravilne tipe stolpcev
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
  # v Splacilni_indeks in Szapadlo_neplacano so manjkajoči podatki
  manjka <- data.frame(m12=is.na(data[,12]), m13=is.na(data[,13]))
  # manjkajoce vrednosti v obeh stolpcih so v istih vrsticah (sum(manjka[,1]!=manjka[,2]) = 0)
  data[manjka[,1],12] <- mean(na.omit(data[[12]]))
  data[manjka[,2],13] <- mean(na.omit(data[[13]]))
  # dodamo stolpec, ki ve, kje so manjkali podatki (v obeh na istih mestih, zato dovolj en stolpec)
  data$SmanjkaI <- as.factor(as.numeric(manjka[,1]))
  return(data)
}

######### dodajanje naivnih spremenljivk ##########
dodajNaivneSpremenljivke  <- function(data) {
  data <- data.table(dummy.data.frame(data))
  # odstranimo nepotrebne dummy stolpce (funkcija je naredila nekaj stolpcev prevec)
  data <- data[,-c("LodobrenNE", "VznamkaALFA", "Scrna_listaNE",
  "Dcrna_listaNE","Strajanje_zaposlitveUPOKOJENEC", "SpostaBA-7", "SdrzavljanstvoALBANIA",                
  "Sdrzavljanstvo_euNE", "SDtipSVOBOD. POKL., TUJINA, D", "SDblokada_racunaNE", "SDinsolventnostNE",                      
  "SDcrna_listaNE", "SmanjkaI0")]
  data$Lodobren <- as.factor(data$LodobrenDA)
  data$Lodobren[data$LodobrenNA == 1] <- NA
  data$LodobrenNA <- NULL
  data$LodobrenDA <- NULL
  
  return(data)
}

######### odstranjevanje nepomembnih spremenljivk ###########
# variančno-kovariančna matrika
varCov <- function(data) {
  varCov <- matrix(NA, ncol = length(colnames(data))-1, nrow = length(colnames(data))-1)
  for (i in 1:(length(colnames(data))-1)) {
    for (j in 1:(length(colnames(data))-1)) {
      varCov[i,j] <- mean((data[[i]]-mean(data[[i]]))*
                                    (data[[j]]-mean(data[[j]])))
    }
    varCov[j,i] <- varCov[i,j]
  }
  return(varCov)
}

odstraniNepotrebneSpremenljivke  <- function(data) {
  varCo <- varCov(data)
  var <- diag(varCo)
  # odstranimo spremenljivke z zelo majhno varianco (vse so dummy)
  ind <- (1:107)[var<min(var)+0.005]
  # pri eps = min(var)+0.005 odstranimo 45 stolpcev od 107
  # pri tem je imel dummy z največ enicami 232 enic (to je cca 3% vseh podatkov)
  data[, ind] <- NULL
  
  varCo <- varCov(data)
  var <- diag(varCo)
  
  # korelacijska matrika:
  # ro <- t(varCo/var^(1/2))/var^(1/2)
  # mogoče odstranimo kakšnega izmed stolpcev, ki so zelo korelirani
  # max(abs(ro[abs(ro)<0.999])) = 0.8999524
  # to je pri ro[50,51], zato odstranimo enega izmed njiju
  data[, 51] <- NULL
  # v tej točki imamo zdaj 61 napovednih spremenljivk
  colnames(data)<-c("VznamkaAUDI", "VznamkaBMW", "VznamkaCHEVROLET", "VznamkaCITROEN",  
                    "VznamkaDACIA", "VznamkaFIAT", "VznamkaFORD", "VznamkaHONDA",    
                    "VznamkaHYUNDAI", "VznamkaKIA", "VznamkaMAZDA",    
                    "VznamkaMERCEDESBENZ", "VznamkaNISSAN", "VznamkaOPEL", "VznamkaPEUGEOT",
                    "VznamkaRENAULT", "VznamkaSEAT", "VznamkaSKODA", "VznamkaSUZUKI", "VznamkaTOYOTA",
                    "VznamkaVOLKSWAGEN", "VznamkaVOLVO", "ScrnalistaDA", "Sstodobritev", "DcrnalistaDA",                        
                    "Dstpogodb", "Vletnik", "Vmpc", "Veurotax", "Lpolog", "Lobrestnamera", "Splacilniindeks",                     
                    "Szapadloneplacano", "Sobligo", "Ltrajanjemeseci", "Snetoplaca", "Srazpolozljivodzmalicoinprevozom",
                    "Sstarost", "StrajanjezaposlitveDOLOCENCAS", "StrajanjezaposlitveNEDOLOCENCAS",    
                    "SpostaSI1", "SpostaSI2", "SpostaSI3", "SpostaSI4", "SpostaSI5", "SpostaSI6", "SpostaSI8", "SpostaSI9",                           
                    "SdrzavljanstvoBIH", "SdrzavljanstvoSLOVENIA", "SDtipGOSPDRUzBSPji", "SDtipJAVNISEKTORUPOKOJENCI",         
                    "SDstzaposlenih", "SDdobaposlovanja", "SDprihodki", "SDodstotekkapitalavbilancnivsoti",
                    "SDposlovniizid", "SDblokadaracunaDA", "SDinsolventnostDA", "SDcrnalistaDA", "SmanjkaI1", "Lodobren")
  return(data)
}

######### popravi razmerje ciljne spremenljivke #########
popraviY <- function(trainData) {
  # razmerje "DA": 60%, "NE": 40%
  # preoblikuj na "DA": 48%, "NE": 52%
  trainData$Lodobren <- as.factor(trainData$Lodobren)
  trainData <- DMwR::SMOTE(Lodobren~.,trainData,k=5,
                           perc.over = 30, perc.under=400)
  # za vsakega iz manjšinskega razreda doda še 30/100 novih
  # za vsakega na novo zgeneriranega iz manjšinskega razreda vzame 400/100 primerkov iz večinskega
  # torej (0: 2383; 1: 3679) -> (0: 3097; 1: 2856)
  return(trainData)
}

######### pogon vseh funkcij za uvoz in obdelavo podatkov #######
podatki <- uvoziPodatke() %>%
  dodajManjkajoce() %>%
  dodajNaivneSpremenljivke() %>%
  odstraniNepotrebneSpremenljivke()

testData <- podatki[is.na(podatki$Lodobren)]
trainData <- podatki[!is.na(podatki$Lodobren)] %>%
  popraviY()

######### output ##########
trainX <- trainData[, -"Lodobren"]
trainY <- trainData$Lodobren
testX <- testData[, -"Lodobren"]

