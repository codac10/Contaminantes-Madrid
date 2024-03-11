getPrediction <- function(meteo,dfCont,estacion,contaminante){
  
  # Seleccionar la estacion y contaminante
  df <- dfCont[[paste0("",estacion,"")]][[paste0("",contaminante,"")]]
  #df <- dfCont[["Plaza de España"]][["06"]]
  
  # Separar regresores entre pasado y futuro
  meteoPasado <- meteo[meteo$FECHA < Sys.time(),]
  meteoFuturo <- meteo[meteo$FECHA >= Sys.time(),]
  
  # Introducir datos de meteo en el df
  merged <- merge(df, meteoPasado, by="FECHA")
  
  regresoresPasado <- merged[,colnames(meteoPasado)[-1]] %>%
    as.matrix()
  regresoresFuturo <- meteoFuturo[,-1] %>%
    as.matrix()
  row.names(regresoresFuturo) <- 1:nrow(regresoresFuturo)
  
  modeloFuturo <- auto.arima(merged$VALOR, xreg=regresoresPasado[,-c(3:4)], lambda=0)
  prediccionFuturo <- forecast(modeloFuturo, h=(nrow()+nrow(regresoresFuturo)), xreg = regresoresFuturo[1:nrow(regresoresFuturo),-c(3:4)], biasadj=TRUE)
  
  modeloPasado <- auto.arima(merged$VALOR, xreg=regresoresPasado[,-c(3:4)], lambda=0)
  prediccionPasado <- fitted(modeloPasado, h=9, xreg = regresoresPasado[10:nrow(meteo),-c(3:4)], biasadj=TRUE)
  plot <- autoplot(ts(merged$VALOR)) + autolayer(prediccionFuturo) + autolayer(prediccionPasado)
  plot
  
  # Error modelo 1
  mape <- Metrics::mape(merged$VALOR[11:nrow(regresoresPasado)], as.vector(prediccionPasado)[-c(1:10)])
  
  prediccionTotal <- list(
    modeloFuturo = modeloFuturo,
    modeloPasado = modeloPasado,
    plot = plot,
    mape = mape
  )
  return(prediccionTotal)
}