getDataFrameMeteo <- function(){
  
  # Descargar los datos de ayer
  ayer = list.files(path = "./Data/Meteo/",pattern="\\.csv$")
  dfAyer <- read.csv(paste0("./Data/Meteo/",ayer))
  
  # Descargar los datos de hoy
  dfHoy <- read.csv("https://www.mambiente.madrid.es/opendata/meteorologia.csv", sep = ";")
  dfMeteo <- rbind(dfAyer, dfHoy)
  
  #Obtener los datos en un solo dataframe
  d <- data.frame(getDailyData(list(dfMeteo)))
  d <- d[!c(as.numeric(d$DIA) == day(Sys.Date()) & as.numeric(d$HORA) >= hour(Sys.time())),]
  
  
  return(d)
}