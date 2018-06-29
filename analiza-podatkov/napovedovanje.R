# napovedovanje prebegov
# Anze Marinko, 27151124
# Izbrane teme iz analize podatkov

source("analiza-podatkov/ucenje-modelov.r", encoding = "UTF-8")

# od prej imamo že narejene modele in uvožene testne podatke
# napovedujemo za podatke "testX"
testY <- predict(finalModel, trainX)
View(testY) # to so zdaj verjetnosti

testY <- data.frame(Lodobren = as.numeric(testY>0.5), verjetnost = testY)
# napovedane podatke shranimo v datoteko
write.csv(testY,
          file="napovedi-zakupov.csv",
          fileEncoding = "UTF-8")
View(testY)
