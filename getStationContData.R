getStationContData <- function(dfCont){
  
  dfEstaciones <- read.csv("https://datos.madrid.es/egobfiles/MANUAL/212629/informacion_estaciones_red_calidad_aire.csv",
                           sep = ";")
  dfEstaciones$CODIGO_CORTO <- str_pad(dfEstaciones$CODIGO_CORTO, 2, pad = "0")
  dfCont <- dfCont[names(dfCont) %in% dfEstaciones$CODIGO_CORTO]
  
  dfEstaciones <- dfEstaciones[dfEstaciones$CODIGO_CORTO %in% names(dfCont),]
  
  names(dfCont) <- dfEstaciones$ESTACION
  
  dfContEstaciones <- list(dfCont = dfCont, dfEstaciones = dfEstaciones)
  return(dfContEstaciones)
}