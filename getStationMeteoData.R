getStationMeteoData <- function(dfMeteo){
  
  dfEstaciones <- read.csv("https://datos.madrid.es/egobfiles/MANUAL/212629/informacion_estaciones_red_calidad_aire.csv",
          sep = ";")
  dfEstaciones$CODIGO_CORTO <- str_pad(dfEstaciones$CODIGO_CORTO, 2, pad = "0")
  dfMeteo <- dfMeteo[names(dfMeteo) %in% dfEstaciones$CODIGO_CORTO]
  
  dfEstaciones <- dfEstaciones[dfEstaciones$CODIGO_CORTO %in% names(dfMeteo),]
  
  names(dfMeteo) <- dfEstaciones$ESTACION
  
  dfMeteoEstaciones <- list(dfMeteo = dfMeteo, dfEstaciones = dfEstaciones)
  return(dfMeteoEstaciones)
}