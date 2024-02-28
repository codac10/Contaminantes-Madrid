require("lubridate");require("stringr");require("rlist");require("dplyr");require("zoo")
require("ggplot2")

setwd("/Users/pablocodina/Library/Mobile Documents/com~apple~CloudDocs/UPM/TFG/DatosTiempoReal")

source("./getDailyData.R")#Usada
#source("./DatosTiempoReal/groupByEstacionDaily.R")
#source("./datosMeteo.R")
source("./getGroupedLiveData.R")#Usada
source("./getDataFrameMeteo.R")#Usada

getInstantDataMeteo <- function(){
  
  date <- Sys.Date()
  dia <- day(date)
  mes <- month(date)-1
  anho <- year(date)
  
  # Obtiene los datos meteorologicos del dia anterior y del dia actual
  d <- getDataFrameMeteo()
  
  # Agrupar los datos en directo por estacion y contaminante
  dfMeteoDirecto <- getGroupedLiveData(d)
  
  return(dfMeteoDirecto)
}

