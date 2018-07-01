# napovedovanje zakupov
# Anze Marinko, 27151124
# Izbrane teme iz analize podatkov

######## uvoz potrebnih knjižnic, spremenljivk, modelov #####
source("analiza-podatkov/ucenje-modelov.r", encoding = "UTF-8")

######## napovedovanje ###########
testY <- predict(finalModel, testX)
levels(testY) <- c("NE", "DA")
testY <- data.frame(Lodobren=testY)
# View(testY) # lahko bi dodal še verjetnosti
# testY <- data.frame(Lodobren = as.numeric(testY>0.5), verjetnost = testY)

######## izvoz napovedanih zakupov ########
write.csv(testY,
          quote=FALSE,
          row.names=FALSE,
          col.names=FALSE,
          file="napovedi-zakupov.csv",
          fileEncoding = "UTF-8")


####### konec programa ############
# print(data.frame(x=c("   Od zacetka do konca izvajanja",
#                      "programa je minilo",
#                      paste(Sys.time()-t,"minut."))))
# za svm in rf 51.2585178693136 minut.
