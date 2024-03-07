require("XML");require("httr");require("jsonlite");require("dplyr")

getWeatherPrediction <- function(){
  
  library(httr)
  # Obtener los datos de la prediccion metereologica
  urlPrediccion <- "https://ai-weather-by-meteosource.p.rapidapi.com/hourly"
  
  queryStringPrediccion <- list(
    lat = "40.41936",
    lon = "-3.747345",
    timezone = "auto",
    language = "en",
    units = "auto"
  )
  
  responsePrediccion <- VERB("GET", urlPrediccion, query = queryStringPrediccion, add_headers('X-RapidAPI-Key' = 'a766e25b75msh1e42476c0192808p1e0bb8jsn2449f1c51f05', 'X-RapidAPI-Host' = 'ai-weather-by-meteosource.p.rapidapi.com'), content_type("application/octet-stream"))
  
  xPrediccion <- fromJSON(content(responsePrediccion, "text"))
  
  # Convertir a data frame
  prediccionMeteo <- xPrediccion[["hourly"]]$data %>% 
    data.frame()
    #select(date, temperature, cloud_cover, ozone,humidity)
  prediccionMeteoFiltro <- prediccionMeteo %>%
    select(date, temperature, cloud_cover, ozone,humidity)
  prediccionMeteoFiltro <- cbind(prediccionMeteoFiltro, 
        wind.speed = prediccionMeteo$wind$speed,
        wind.angle = prediccionMeteo$wind$angle,
        precipitation.total = prediccionMeteo$precipitation$total)
  
  prediccionMeteo$date <- strptime(prediccionMeteo$date, format = "%Y-%m-%dT%H:%M")
  prediccionMeteo <- prediccionMeteo %>%
    subset(date >= Sys.time())
  prediccionMeteo <- prediccionMeteo[1:24,]
  
  prediccionMeteo <- data.frame(prediccionMeteo)
  
  
}