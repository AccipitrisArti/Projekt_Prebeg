# ucenje modelov, podatkovno rudarjenje
# Anze Marinko, 27151124
# Izbrane teme iz analize podatkov

source("analiza-podatkov/uvoz-in-transformacija.r", encoding = "UTF-8")

# funkcije za preverjanje natančnosti, trainControl ...
# napovedni modeli
# izbira modela

finalModel <- train(trainX, trainY, method = "knn",
               tuneGrid = data.frame(k=3))
