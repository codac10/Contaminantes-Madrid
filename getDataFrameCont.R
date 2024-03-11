getDataFrameCont <- function(){
  source("./getDailyDataCont.R")
  
  # Descargar los datos de ayer
  ayer = list.files(path = "./Data/Contaminantes/",pattern="\\.csv$")
  dfAyer <- read.csv(paste0("./Data/Contaminantes/",last(ayer)))
  
  # Descargar los datos de hoy
  nombreColumnnas <- c("PROVINCIA", "MUNICIPIO", "ESTACION", "MAGNITUD", "TECNICA", "PERIODO_DE_ANALISIS",
                       "ANO", "MES", "DIA", "H01", "V01", "H02", "V02", "H03",
                       "V03", "H04", "V04", "H05", "V05", "H06", "V06", "H07", "V07", "H08", "V08",
                       "H09", "V09", "H10", "V10", "H11", "V11", "H12", "V12", "H13", "V13", "H14",
                       "V14", "H15", "V15", "H16", "V16", "H17", "V17", "H18", "V18", "H19", "V19",
                       "H20", "V20","H21", "V21", "H22", "V22", "H23", "V23", "H24", "V24")
  dfHoy <- read.csv(paste0("https://www.mambiente.madrid.es/opendata/horario.txt"))
  colnames(dfHoy) <- nombreColumnnas
  
  # Añadir columna punto muestreo
  dfCont <- rbind(dfAyer, dfHoy)
  # Filtrar las columnas
  dfCont <- dfCont[,!names(dfCont) %in% c("TECNICA", "PERIODO_DE_ANALISIS")]
  
  # Añado la columna punto de muestreo
  dfCont$PUNTO_MUESTREO <- paste0(dfCont$PROVINCIA,dfCont$MUNICIPIO,dfCont$ESTACION,"_",dfCont$MAGNITUD)
  
  #Obtener los datos en un solo dataframe
  d <- data.frame(getDailyDataCont(list(dfCont)))
  d <- d[!c(as.numeric(d$DIA) == day(Sys.Date()) & as.numeric(d$HORA) >= hour(Sys.time())),]
  
  
  return(d)
}