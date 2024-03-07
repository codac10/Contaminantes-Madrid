require("httr");require("jsonlite");require("dplyr")

getWeatherPastData <- function(){
  
  
  ### Antigua API
  urlPasado <- "https://ai-weather-by-meteosource.p.rapidapi.com/time_machine"
  
  date <- seq(as.Date("2023/09/01"), by = "day", length.out = 5)
  queryStringPasado <- list()
  for(ii in 1: 1:length(date)){
    queryStringPasado[[ii]] <- list(date = date[ii],
                                    lat = "40.41936",
                                    lon = "-3.747345",
                                    units = "auto")
  }
  
  responsePasado <- list()
  xPasado <- list()
  for(ii in 1:length(queryStringPasado)){
    responsePasado[[ii]] <- VERB("GET", urlPasado, query = queryStringPasado[[ii]], add_headers('X-RapidAPI-Key' = 'a766e25b75msh1e42476c0192808p1e0bb8jsn2449f1c51f05', 'X-RapidAPI-Host' = 'ai-weather-by-meteosource.p.rapidapi.com'), content_type("application/octet-stream"))
    xPasado[[ii]] <- fromJSON(content(responsePasado[[ii]], "text"))
  }
  
  # Convertir a data frame
  pasadoMeteo <- lapply(xPasado,function(x) x[["data"]] %>% 
    data.frame())
  #select(date, temperature, cloud_cover, ozone,humidity)
  pasadoMeteoFiltro <- lapply(pasadoMeteo, function(x) x %>%
    select(date, temperature, cloud_cover, ozone,humidity))
  pasadoMeteoFiltro <- lapply(pasadoMeteo, function(x) cbind(x, 
                                 wind.speed = x$wind$speed,
                                 wind.angle = x$wind$angle,
                                 precipitation.total = x$precipitation$total))
  pasadoMeteoFiltro <- lapply(pasadoMeteoFiltro, function(x) x %>%
    select(date, temperature, cloud_cover, ozone,humidity, wind.speed, wind.angle, precipitation.total))
  
  date <- lapply(pasadoMeteoFiltro, function(x) x$date <- strptime(x$date, format = "%Y-%m-%dT%H:%M"))
  for(ii in 1:length(date)){
    
    pasadoMeteoFiltro[[ii]]$date <- pasadoMeteoFiltro[[ii]]$date %>% 
      as.POSIXct(format = "%Y-%m-%dT%H:%M") %>%
      data.frame()
  }
  
  pasadoMeteoData <- pasadoMeteoFiltro %>% list.rbind()
  
}