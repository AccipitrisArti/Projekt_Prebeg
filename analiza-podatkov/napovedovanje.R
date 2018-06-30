# napovedovanje zakupov
# Anze Marinko, 27151124
# Izbrane teme iz analize podatkov

######## uvoz potrebnih knjižnic, spremenljivk, modelov #####
source("analiza-podatkov/ucenje-modelov.r", encoding = "UTF-8")

######## napovedovanje ###########
testY <- predict(finalModel, testX)
levels(testY) <- c("NE", "DA")
# View(testY) # lahko bi dodal še verjetnosti
# testY <- data.frame(Lodobren = as.numeric(testY>0.5), verjetnost = testY)

######## izvoz napovedanih zakupov ########
write.csv(testY,
          file="napovedi-zakupov.csv",
          fileEncoding = "UTF-8")

####### konec programa ############
print(data.frame(x=c("   Od zacetka do konca izvajanja",
                     "programa je minilo",
                     paste(Sys.time()-t,"minut."))))

