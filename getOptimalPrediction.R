

getOptimalPrediction <- function(dfContEstaciones$dfCont,dfMeteoEstaciones$dfMeteo,input$){ #Name input object#
  
  
  dfCont.pasado <- lapply(dfContEstaciones[["dfCont"]][["Casa de Campo"]], function(df) filter(df,DIA == "26")) #Añadir aqui el input de la estacion
  df <- dfCont.pasado[["14"]]
  
  
  dfMeteo.pasado <- lapply(dfMeteoEstaciones[["dfMeteo"]][["Casa de Campo"]], function(df) df$VALOR)
  
  ## ARIMA DEL TIEMPO
  dfMeteo.modelo <- lapply(dfMeteo.pasado, function(df) auto.arima(df))
  
  dfMeteo.futuro <- matrix(NA, nrow = 14, ncol = length(dfMeteo.pasado))
  for(ii in 1:length(dfMeteo.pasado)){
    dfMeteo.futuro[,ii] <- t(forecast(prediction[[ii]], h = 14)$mean)
  }
  colnames(dfMeteo.futuro) <- names(dfMeteo.pasado)
  
  
  #dfMeteo.prediction <- dfMeteo.prediction %>% 
  #  list.rbind()
  #dfMeteo.prediction <- dfMeteo.prediction[,1:24] %>% t() %>% as.matrix()
  
  dfMeteo.pasado <- matrix(unlist(dfMeteo.pasado), nrow = 24, ncol = length(dfMeteo.pasado))
  colnames(dfMeteo.pasado) <- colnames(dfMeteo.futuro)
  dfCont.modelo <- auto.arima(log(dfCont.pasado[["14"]]$VALOR[1:10]), xreg = dfMeteo.pasado[1:10,c(1:3)])
  dfCont.prediction <- forecast(dfCont.modelo, xreg = dfMeteo.futuro[,c(1:3)], h = 14)
  autoplot(ts(dfCont.pasado[["14"]]$VALOR)) +
    autolayer(exp(dfCont.prediction$mean))
  
  
  prueba <- prueba%>%
    as.data.frame %>%
    as.matrix()
  dfMeteo.prediction.forecast <- lapply(dfMeteoEstaciones[["dfMeteo"]][["Casa de Campo"]], function(df) tail(df$VALOR,1))
  dfMeteo.prediction.forecast.num <- lapply(dfMeteo.prediction.forecast, function(df) rep(df,24)) %>%
    list.rbind()
  dfMeteo.prediction.forecast.num <- dfMeteo.prediction.forecast.num[1:6,] %>% t() %>% as.matrix()
  
  autoplot(ts(df$VALOR)) +
    autolayer(forecast(prediction, xreg = dfMeteo.prediction.forecast.num,h = 10))
}