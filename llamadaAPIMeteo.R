llamadaAPIMeteo <- function(){
  require("httr");require("jsonlite");require("dplyr")
  
  url <- "https://tomorrow-io1.p.rapidapi.com/v4/timelines"
  
  queryString <- list(
    startTime = "nowMinus24h",
    location = "40.41936, -3.747345",
    fields = "temperature,humidity,windSpeed,windDirection,rainAccumulation,rainIntensity,pressureSurfaceLevel",
    endTime = "nowPlus24h",
    timesteps = "1h",
    units = "metric"
  )
  
  response <- VERB("GET", url, query = queryString, add_headers('X-RapidAPI-Key' = 'a766e25b75msh1e42476c0192808p1e0bb8jsn2449f1c51f05', 'X-RapidAPI-Host' = 'tomorrow-io1.p.rapidapi.com'), content_type("application/octet-stream"))
  
  x <- fromJSON(content(response, "text"))
  
  meteo <- cbind(
    FECHA = x[["data"]][["timelines"]][["intervals"]][[1]][["startTime"]],
    x[["data"]][["timelines"]][["intervals"]][[1]][["values"]])
  
  meteo$FECHA <- as.POSIXct(meteo$FECHA, format="%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  
  # Cambiar horario a Madrid
  meteo$FECHA <- with_tz(meteo$FECHA, "Europe/Madrid")
  
  return(meteo)
}