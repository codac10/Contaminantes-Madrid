source("getDailyData.R")

require("forecast")

d <- read.csv("../Datos TFG/Anio23/sep_mo23.csv", sep = ";")
d <- data.frame(getDailyData(list(d)))
d <- d[!c(as.numeric(d$DIA) >= 7),]
d <- getGroupedLiveData(d)

# Juntar datos meteo con datos contaminacion
d_elegido <- d[["04"]][["12"]]
posicion_date <- which(colnames(pasadoMeteoData) == "date")
colnames(pasadoMeteoData)[posicion_date] <- "FECHA"
pasadoMeteoData$FECHA$zone <- NULL

# Cambiar formato fecha
pasadoMeteoData$FECHA <- as.character(pasadoMeteoData$FECHA[,1])
d_elegido$FECHA <- as.character(d_elegido$FECHA)

# Unir ambos datos
 m <- merge(d_elegido, pasadoMeteoData, by="FECHA", all.x=TRUE) %>% na.omit()

xregresors <- merged %>%
  select(colnames(pasadoMeteoData)) %>%
  select(-c("FECHA")) %>%
  as.matrix()

modelo1 <- auto.arima(ts(merged$VALOR[1:(30)]),xreg = xregresors[1:(30),1:7])
prediccion1 <- forecast(modelo1, h=(119-(30)), xreg = xregresors[(30):119,1:7])
modelo2 <- auto.arima(ts(merged$VALOR[1:(119-24)]),xreg = xregresors[1:(119-24),])
prediccion2 <- forecast(modelo2, h=24, xreg = xregresors[(119-24):119,])
plot <- autoplot(ts(merged$VALOR)) + 
  autolayer(prediccion1$mean) + 
  autolayer(prediccion2)

arimas <- mapply(function(x,xregresors) auto.arima(x$VALOR, xreg=as.matrix(xregresors)), d[["04"]], xregresors)
prediccion <- lapply(arimas, function(x)forecast(x, h=10))
df <- lapply(d[["04"]], function(x) x$VALOR)
plots <- mapply(function(df,prediccion) {
  plot <- autoplot(ts(df)) + autolayer(prediccion)
  print(plot)
  },df,prediccion)




plots[[1]]
