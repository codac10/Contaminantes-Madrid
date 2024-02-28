### Agrupar los datos en directo por estacion y contaminante ###
getGroupedLiveData <- function(d){
  
  # Hacer factores las variables categoricas y cambiar los datos que no son validos
  # por NA
  d$PROVINCIA <- factor(d$PROVINCIA)
  d$MUNICIPIO <- factor(d$MUNICIPIO)
  d$ESTACION <- factor(d$ESTACION)
  d$MAGNITUD <- factor(d$MAGNITUD)
  d$ESTADO <- factor(d$ESTADO)
  d$VALOR <- as.numeric(d$VALOR)
  for(ii in 1:nrow(d)){
    if(d$ESTADO[ii] == "N"){d$VALOR[ii] = NA}
  }
  
  a <- levels(d$ESTACION)
  
  # Agrupar los datos por estacion
  dMeteoEstacion <- list()
  for(ii in 1:length(levels(d$ESTACION))){
    
    dMeteoEstacion[[ii]] <- d[d$ESTACION == levels(d$ESTACION)[ii],]
  }
  
  dMeteoEstacionCont <- list()
  for(ii in 1:length(dMeteoEstacion)){
    
    dMeteoEstacionCont[[ii]] <- list()
    for(jj in 1:length(levels(dMeteoEstacion[[ii]]$MAGNITUD))){
      
      dMeteoEstacionCont[[ii]][[jj]] <- dMeteoEstacion[[ii]][dMeteoEstacion[[ii]]$MAGNITUD == levels(dMeteoEstacion[[ii]]$MAGNITUD)[[jj]],]
      
      hora <- Sys.time() %>% hour()
      
      # Eliminar los datos que tienen una hora mayor a la actual ¡¡¡ATENCION!!! Cambiar para que no elimnie los datos del dia anterior
      #dMeteoEstacionCont[[ii]][[jj]] <- subset(dMeteoEstacionCont[[ii]][[jj]], dMeteoEstacionCont[[ii]][[jj]]$HORA < hora)
      dMeteoEstacionCont[[ii]][[jj]]$VALOR <- na.approx(dMeteoEstacionCont[[ii]][[jj]]$VALOR, na.rm = FALSE)
    }
    names(dMeteoEstacionCont[[ii]]) <- levels(dMeteoEstacion[[ii]]$MAGNITUD)
  }
  
  names(dMeteoEstacionCont) <- levels(d$ESTACION)
  
  # Agrupar los datos por estacion y por contaminante
  dfMeteoAll <- list()
  for(ii in 1:length(dMeteoEstacionCont)){
    
    dfMeteoAll[[ii]] <- list()
    for(jj in 1:length(dMeteoEstacionCont[[ii]])){
      
      if(nrow(dMeteoEstacionCont[[ii]][[jj]]) > 0){
        dfMeteoAll[[ii]][[jj]] <- dMeteoEstacionCont[[ii]][[jj]]
        names(dfMeteoAll[[ii]])[[jj]] <- as.character(dfMeteoAll[[ii]][[jj]]$MAGNITUD[1])
      }
    }
    
    # Eliminar las listas que no tienen valores
    dfMeteoAll[[ii]] <- Filter(Negate(is.null), dfMeteoAll[[ii]])
  }
  
  # Renombrar los datos de las estaciones
  names(dfMeteoAll) <- as.character(levels(d$ESTACION))
  
  return(dfMeteoAll)
}