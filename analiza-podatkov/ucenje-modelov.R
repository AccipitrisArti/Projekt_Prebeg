# ucenje modelov, podatkovno rudarjenje
# Anze Marinko, 27151124
# Izbrane teme iz analize podatkov

source("analiza-podatkov/uvoz-in-transformacija.r", encoding = "UTF-8")

# funkcije za preverjanje natančnosti, trainControl ...
# napovedni modeli
# izbira modela

finalModel <- randomForest(trainX, y=trainY, ntree=60, mtry=30,
                           maxnodes = 300)
plot(finalModel)
