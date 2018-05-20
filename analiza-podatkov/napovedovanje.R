# uvoz testnih podatkov in napovedovanje prebegov
# Anze Marinko, 27151124
# Izbrane teme iz analize podatkov

source("analiza-podatkov/ucenje-modelov.r", encoding = "UTF-8")

uvoziTestnePodatke <- function() {
  data <- read_csv("Podatki/orange-churn-test.csv",
                   locale = locale(encoding = "Windows-1250"))
  
  return(data)
}
testX <- uvoziTestnePodatke()

