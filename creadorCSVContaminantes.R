## Obtiene los datos de contaminantes de la web del ayuntamiento de Madrid

# Descargar los datos
nombreColumnnas <- c("PROVINCIA", "MUNICIPIO", "ESTACION", "MAGNITUD", "TECNICA", "PERIODO_DE_ANALISIS",
  "ANO", "MES", "DIA", "H01", "V01", "H02", "V02", "H03",
  "V03", "H04", "V04", "H05", "V05", "H06", "V06", "H07", "V07", "H08", "V08",
  "H09", "V09", "H10", "V10", "H11", "V11", "H12", "V12", "H13", "V13", "H14",
  "V14", "H15", "V15", "H16", "V16", "H17", "V17", "H18", "V18", "H19", "V19",
  "H20", "V20","H21", "V21", "H22", "V22", "H23", "V23", "H24", "V24")
df <- read.csv(paste0("https://www.mambiente.madrid.es/opendata/horario.txt"))
colnames(df) <- nombreColumnnas

# Guardar los datos
setwd("/Users/pablocodina/Library/Mobile Documents/com~apple~CloudDocs/UPM/TFG/DatosTiempoReal/Data/Contaminantes")
fecha <- Sys.Date()
write.csv(df, paste0("datos_contaminantes_madrid_",fecha,".csv"), row.names = FALSE)

# Eliminar los datos antiguos
fechaAnterior <- Sys.Date() - 1
file.remove(paste0("./DatosTiempoReal/Data/datos_contaminantes_madrid_",fechaAnterior,".csv"))
setwd("/Users/pablocodina/Library/Mobile Documents/com~apple~CloudDocs/UPM/TFG/DatosTiempoReal")
