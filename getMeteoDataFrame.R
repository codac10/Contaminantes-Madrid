getDataFrameMeteo <- function(){
  
  # Descargar los datos de ayer
  ayer = list.files(pattern="\\.csv$")
  dfAyer <- read.csv(ayer)
  
  # Descargar los datos de hoy
  dfHoy <- read.csv("https://www.mambiente.madrid.es/opendata/meteorologia.csv", sep = ";")
  dfMeteo <- rbind(dfAyer, dfHoy)
  
  #Obtener los datos en un solo dataframe
  d <- data.frame(getDailyData(list(dfMeteo)))
  return(d)
}