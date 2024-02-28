## Obtiene los datos meteorologicos de la web del ayuntamiento de Madrid

# Descargar los datos
df <- read.csv("https://www.mambiente.madrid.es/opendata/meteorologia.csv", sep = ";")

# Guardar los datos
setwd("/Users/pablocodina/Library/Mobile Documents/com~apple~CloudDocs/UPM/TFG/DatosTiempoReal/Data/Meteo")
fecha <- Sys.Date()
write.csv(df, paste0("datos_meteorologicos_madrid_",fecha,".csv"), row.names = FALSE)

# Eliminar los datos antiguos
fechaAnterior <- Sys.Date() - 1
file.remove(paste0("datos_meteorologicos_madrid_",fechaAnterior,".csv"))
setwd("/Users/pablocodina/Library/Mobile Documents/com~apple~CloudDocs/UPM/TFG/DatosTiempoReal")
