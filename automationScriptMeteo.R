## Obtener los datos de la meteo y los contaminantes cada día a las 00:30
## y guardarlos en un archivo csv
require("cronR")

library(cronR)

# Direccion del script a ejecutar~/Library/Mobile Documents/com~apple~CloudDocs/UPM/TFG/DatosTiempoReal/creadorCSVMeteo.R
path <- "/Users/pablocodina/Library/Mobile Documents/com~apple~CloudDocs/UPM/TFG/DatosTiempoReal/creadorCSVMeteo.R"

# Crear comando para ejecutar el script
cmd <- cron_rscript(path)

cron_add(command = cmd, frequency = "daily", at = "23:30", id = "meteorologicos",
         description = "Datos meteorologicos")


### Eliminar automatizacion ###
#cron_rm(id="meteorologicos")

# Direccion del script a ejecutar
path2 <- "/Users/pablocodina/Library/Mobile Documents/com~apple~CloudDocs/UPM/TFG/DatosTiempoReal/creadorCSVContaminantes.R"

# Crear comando para ejecutar el script
cmd2 <- cron_rscript(path2)

cron_add(command = cmd2, frequency = "daily", at = "23:31", id = "contaminantes",
         description = "Datos contaminantes")


### Eliminar automatizacion ###
#cron_rm(id="contaminantes")