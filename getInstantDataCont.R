require("lubridate");require("stringr");require("rlist");require("dplyr");require("zoo")
require("ggplot2")

setwd("/Users/pablocodina/Library/Mobile Documents/com~apple~CloudDocs/UPM/TFG/DatosTiempoReal")

source("./getDailyData.R")#Usada
#source("./DatosTiempoReal/groupByEstacionDaily.R")
#source("./datosMeteo.R")
source("./getGroupedLiveData.R")#Usada
source("./getDataFrameMeteo.R")#Usada

##### FALTA POR ACTUALIZAR PARA QUE ESTEN LOS DATOS DE LOS CONTAMINANTES

getInstantDataCont <- function(){
  
  
  date <- Sys.Date()
  dia <- day(date)
  mes <- month(date)-1
  anho <- year(date)
  
  #mes.ant <- str_pad(mes, width = 2, pad = "0")
  #d <- read.csv(paste0("https://datos.madrid.es/datosabiertos/MEDIOAMBIENTE/CALIDAD_DEL_AIRE/",anho,"/",mes.ant,"/datos",anho,"",mes.ant,".csv"),sep = ";")
  
  #dfContaminantes <- datosMeteo(d)
  
  # Obtiene los datos meteorologicos del dia anterior y del dia actual
  d <- getDataFrameMeteo()
  
  # Agrupar los datos en directo por estacion y contaminante
  dfMeteoDirecto <- getGroupedLiveData(d)
  
  return(dfMeteoDirecto)
}