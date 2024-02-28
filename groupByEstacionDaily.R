groupByEstacionDaily <- function(meteo){  
  
  magnitud <- levels(factor(meteo$MAGNITUD))
  # Seleccionar los datos con los valores diarios
  dias <- meteo %>% 
    select(PUNTO_MUESTREO,MES,ANO,matches("D"),-MAGNITUD,-DIA) %>%
    t() %>%
    as.data.frame()
  
  # Seleccionar los datos con las validaciones
  validacion <- meteo %>% 
    select(PUNTO_MUESTREO,MES,ANO,matches("V"),-PROVINCIA) %>%
    t() %>%
    as.data.frame()
  
  # Ordenar alternativamente entre datos contaminantes y validaciones
  orden1 <- order(c(2*(seq_along(dias) - 1) + 1,
                    2*seq_along(validacion)))
  datos_dia1 <- cbind(dias,validacion)[,orden1]
  
  # Crear variables para introducir las fechas de cada dato
  ANO <- datos_dia1["ANO",]
  MES <- datos_dia1["MES",]
  (FECHA1 <- paste0(MES,"-",ANO))
  FECHA2 <- matrix(FECHA1, nrow=nrow(datos_dia1)-3, ncol=length(FECHA1), byrow=TRUE) %>%
    as.data.frame()
  impares <- seq(1, ncol(FECHA2), 2)
  FECHA2 <- FECHA2[,impares]
  
  
  # Ordenar los datos en vertical por cada mes y separados por mag. y est.
  colnames(datos_dia1) <- datos_dia1["PUNTO_MUESTREO",]
  filas_basura <- c("PUNTO_MUESTREO","MES","ANO")
  datos_dia2 <- datos_dia1[!(row.names(datos_dia1) %in% filas_basura),]
  datos_dia3<- list()
  datos_dia3 <- split.default(datos_dia2, (seq_along(datos_dia2)-1) %/% 2)
  
  # Mejorar usabilidad datos e introducir la fecha
  for(jj in 1:length(datos_dia3)){
    
    PUNTO_MUESTREO <- names(datos_dia3[[jj]])[2]
    PUNTO_MUESTREO <- rep(PUNTO_MUESTREO,nrow(datos_dia3[[jj]]))
    DIA <- seq_len(nrow(datos_dia3[[jj]]))
    datos_dia3[[jj]] <- cbind(datos_dia3[[jj]],PUNTO_MUESTREO,DIA,FECHA2[jj])
    colnames(datos_dia3[[jj]])[5] <- paste0("MESANO")
    datos_dia3[[jj]]$MESANO <- gsub(" ","",as.character(datos_dia3[[jj]]$MESANO))
    datos_dia3[[jj]]$MESANO <- str_pad(datos_dia3[[jj]]$MESANO, width = 7, pad = "0",side = "left")
    datos_dia3[[jj]]$DIA <- str_pad(datos_dia3[[jj]]$DIA, width = 2, pad = "0",side = "left")
    datos_dia3[[jj]]$FECHA<- with(datos_dia3[[jj]],paste0("",t(DIA),"-",MESANO))
    datos_dia3[[jj]]$FECHA <- as.Date(datos_dia3[[jj]]$FECHA,format = "%d-%m-%Y")
    colnames(datos_dia3[[jj]]) <- paste0("V", seq_len(ncol(datos_dia3[[jj]])))
  }
  
  # Juntar en un unico data frame
  datos_dia4 <- list.rbind(datos_dia3)
  
  datos_dia4 <- na.omit(datos_dia4)
  
  # Introducir variables de estacion y magnitud
  datos_dia4$V3 <- gsub("\\..*", "", datos_dia4$V3)
  datos_dia4$V7 <- substr(datos_dia4$V3, 6,8)
  datos_dia4$V8 <- substr(datos_dia4$V3, 10,11)
  a <- levels(factor(datos_dia4$V7))
  b <- levels(factor(datos_dia4$V8))
  
  # Agrupar por estaciones
  datos_estacion <- list()
  
  for(ii in 1:length(a)){
    
    datos_estacion[[ii]] <- datos_dia4 %>% filter(V7 == a[ii])
    names(datos_estacion)[[ii]] <- a[[ii]]
  }
  
  # Limpiar datos erroneos y mantener columnas necesarias
  nombres <- c("VALOR","ESTADO","FECHA","MAGNITUD")
  for(ii in 1:length(datos_estacion)){
    
    datos_estacion[[ii]] <- select(datos_estacion[[ii]],V1,V2,V6,V8)
    colnames(datos_estacion[[ii]]) <- nombres
    rownames(datos_estacion[[ii]]) <- NULL
    for(jj in 1:nrow(datos_estacion[[ii]])){
      
      if(datos_estacion[[ii]]$ESTADO[jj] == "N"){datos_estacion[[ii]]$VALOR[jj] = NA}
    }
  }
  
  # Organizar por estaciones y por contaminantes
  c <- list()
  d <- list()
  
  for(jj in 1:length(datos_estacion)){
    for(ii in 1:length(magnitud)){
      mag <- magnitud[ii]
      c[[ii]] <- subset(datos_estacion[[jj]], 
                        datos_estacion[[jj]]$MAGNITUD == mag)
      #FECHA <- x$FECHA
      #VALOR <- x$VALOR
      #a[[ii]] <- cbind(x,FECHA,VALOR)
      names(c)[[ii]] <- paste0("",magnitud[ii],"")
      #a <- Filter(function(df) nrow(df) = 0, a)
      #colnames(a[[ii]]) <- paste0("",colnames(a[[ii]]),".",est,".",mag)
    }
    c <- c[sapply(c, nrow) > 0]
    d[[jj]] <- c
    names(d)[[jj]] <- a[[jj]]
  }
  
  # Juntar los datos de cada contaminante por columnas
  merged_meteo <- list()
  for(jj in 1:24){
    for(ii in 1:(length(d[[jj]])-1)){
      if(length(d[[jj]])>1){
        if(ii == 1){
          merged_meteo[[jj]] <- merge(d[[jj]][[ii]], d[[jj]][[ii+1]], by="FECHA", all=TRUE);
        }
        else{
          merged_meteo[[jj]] <- merge(merged_meteo[[jj]], d[[jj]][[ii+1]], by="FECHA", all=TRUE)
        }
      }
      else{
        merged_meteo[[jj]] <- d[[1]] %>% as.data.frame()
      }
    }
  }
  
  names(merged_meteo) <- a
  
  df <- list()
  for(ii in 1:length(merged_meteo)){
    
    colnames(merged_meteo[[ii]]) <- paste0("",colnames(merged_meteo[[ii]]),"",c(1:ncol(merged_meteo[[ii]])))
    df[[ii]] <- merged_meteo[[ii]] %>% select(matches("FECHA"),matches("VALOR"))
    colnames(df[[ii]]) <- c(as.character("FECHA"),names(d[[ii]]))
  }
  
  names(df) <- a
  
  # Filtrar estaciones meteorologicas
  #estaciones <- c("008","018","036","107","024")
  #df <- df[names(df) %in% estaciones == TRUE]
  #df <- df[c("008","018","036","107","024")]
  
  return(df)
}