### Estructurar los datos por dias, estacion y contaminantes de un dia determinado ###
getDailyData <- function(datos){

datos_mes <- list()
orderData <- function(datos){
  datos_mes <- list()
  for(ii in 1:length(datos)){
    day <- list()
    for(j in 1:nrow(datos[[ii]])){
      
      # Crear una matriz con los datos por filas
      PROVINCIA <- rep(datos[[ii]][j,1],24) %>% as.vector()
      MUNICIPIO <- rep(datos[[ii]][j,2],24) %>% as.vector()
      ESTACION <- rep(datos[[ii]][j,3],24) %>% as.vector()
      ESTACION <- str_pad(ESTACION, width = 2, pad = "0")
      MAGNITUD <- rep(datos[[ii]][j,4],24) %>% as.vector()
      MAGNITUD <- str_pad(MAGNITUD, width = 2, pad = "0")
      PUNTO_MUESTREO <- rep(datos[[ii]][j,5],24) %>% as.vector()
      ANO <- rep(datos[[ii]][j,6],24) %>% as.vector()
      MES <- rep(datos[[ii]][j,7],24) %>% as.vector()
      MES <- str_pad(MES, width = 2, pad = "0")
      DIA <- rep(datos[[ii]][j,8],24) %>% as.vector()
      DIA <- str_pad(DIA, width = 2, pad = "0")
      
      x <- datos[[ii]][j,9:56] %>% t() %>% as.vector()
      HORA <- 1:24
      HORA <- str_pad(HORA, width = 2, pad = "0")
      MINUTO <- rep("00",24)
      dayValues <- cbind(HORA,MINUTO,matrix(x, ncol = 2, byrow = TRUE)) %>% as.data.frame()
      colnames(dayValues) <- c("HORA", "MINUTO", "VALOR", "ESTADO")
      
      # Crear una variable FECHA para hacer un modelo temporal
      FECHA <- paste0("",ANO,"-",MES,"-",DIA," "
                      ,HORA,":",MINUTO,"")
      FECHA <- strptime(FECHA, format = "%Y-%m-%d  %H:%M")
      
      day[[j]] <- cbind(PROVINCIA,MUNICIPIO,ESTACION,MAGNITUD,PUNTO_MUESTREO
                        ,ANO,MES,DIA,FECHA,dayValues) %>% as.data.frame()
      names(day)[j] <- paste0("", ESTACION[1],
                              "_", MAGNITUD[1],
                              "_", FECHA[1])
    }
    # Crear una lista con los datos de ese mes para almacenar
    datos_mes[[ii]] <- list.rbind(day)
  }
  return(datos_mes)
}

datos <- orderData(datos)

}