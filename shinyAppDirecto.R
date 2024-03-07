require("shiny");require("plotly");require("shinydashboard");require("corrplot");
require("rlist");require("forecast");require("leaflet");require("bslib")

setwd("/Users/pablocodina/Library/Mobile Documents/com~apple~CloudDocs/UPM/TFG/DatosTiempoReal")

# Funciones Meteo
source("./getInstantDataMeteo.R");source("./getStationMeteoData.R")
source("./llamadaAPIMeteo.R")

# Funciones Contaminantes
source("./getInstantDataCont.R");source("./getStationContData.R")

# Funciones Predicción
source("./getPrediction.R")

# Obtener todos los datos de las estaciones metereologicas con sus contaminantes
#dfMeteo <- getInstantDataMeteo()
#dfContEstaciones <- getStationContData(dfMeteo)

# Hacer lamada a la API para sacar los datos meteo
#meteo <- llamadaAPIMeteo()

# Obtener los datos de los contaminantes
dfCont <- getInstantDataCont()
dfContEstaciones <- getStationContData(dfCont)

# Hacer predicción de los contaminantes


ui <- dashboardPage(
  
  dashboardHeader(title = "Contaminación Madrid"),
  
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      menuItem("Estaciones", tabName = "estacion", icon = icon("location-dot")),
      menuItem("Predicción", tabName = "prediccion", icon = icon("chart-line")),
      menuItem("Modelos alternativos", tabName = "modelos", icon = icon("filter"))
    )
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "estacion",
              column(6,
                box(title = "Mapa de estaciones",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    leafletOutput("estacion.mapa", width = "100%", height = 660),
                    height = 720
                    ),
              ),
              column(6,
                box(title = "Datos",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    fluidRow(
                      column(6,selectInput("estacion.estacion", "Estación:", choices = c(unique(names(dfContEstaciones$dfCont))))),
                      column(6,selectInput("estacion.contaminante", "Contaminante:", choices = c()))
                    ),
                    plotOutput("estacion.grafico", height = 250),
                    height = 400
                    ),
                box(title = "Información contaminante",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    textOutput("estacion.info.contaminante"),
                    height = 300
                    ),
              )
              ),
      tabItem(tabName = "prediccion",
                column(4,
                       fluidRow(
                         box(title = "Elegir estación y contaminante",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 12,
                             selectInput("prediccion.estacion", "Estación:", choices =c(unique(names(dfContEstaciones$dfCont)))),
                             selectInput("prediccion.contaminante", "Contaminante:", choices = c()),
                         ),
                       ),
                       fluidRow(
                         #valueBox(0, h4("Nivel actual"), icon = icon("clock"), color = "blue", width = 6),
                         #valueBox(0, h4("Error del modelo (MAPE)"), icon = icon("xmark"), color = "red", width = 6),
                         valueBoxOutput("prediccion.actual", width = 6),
                         valueBoxOutput("prediccion.mape", width = 6)
                       ),
                       fluidRow(
                         box(title = "Próximos niveles de contaminación",
                             status = "primary",
                             solidHeader = TRUE,
                             width = 12,
                             textOutput("prediccion.datos")
                         ),
                       ),
                       ),
              column(8,
                box(title = "Modelo de predicción",
                    status = "primary",
                    solidHeader = TRUE,
                    width = 12,
                    verbatimTextOutput("prediccion.modelo"),
                    height = 300
                    ),
              box(title = "Gráfico de predicción",
                  status = "primary",
                  solidHeader = TRUE,
                  width = 12,
                  plotOutput("prediccion.grafico"),
                  height = 500
                  )
              )
      )
    )
  )
)

server<- function(input, output, session) {
  
  # Mapa con las estaciones a medir
  output$estacion.mapa <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addMarkers(
        lng = dfMeteoEstaciones$dfEstaciones$LONGITUD,
        lat = dfMeteoEstaciones$dfEstaciones$LATITUD,
        popup = paste("<h4>", dfMeteoEstaciones$dfEstaciones$ESTACION,"</h4>")
        )
    
  })
  
  # Contaminantes de la estacion seleccionada
  observeEvent(input$estacion.estacion, {
    updateSelectInput(session, "estacion.contaminante", choices = names(dfContEstaciones$dfCont[[paste0("",input$estacion.estacion,"")]]))
  })
  
  # Grafico de la estacion y contaminante seleccionados
  output$estacion.grafico <- renderPlot({
    ggplot() +
      geom_line(data = dfContEstaciones$dfCont[[paste0("",input$estacion.estacion,"")]][[paste0("",input$estacion.contaminante,"")]],
                aes(x = FECHA, y = VALOR)) +
      theme_minimal()
    
  })
  
  output$estacion.contaminantes.directo <- renderPlot({
    plot(dfContEstaciones$dfCont, type = "l", col = 1, lwd = 2, main = "Contaminantes en directo", xlab = "Hora", ylab = "Nivel")
  })
  
  output$estacion.meteo.directo <- renderPlot({
    plot(dfContEstaciones$dfCont$`28079004`, type = "l", col = 1, lwd = 2, main = "Meteorología en directo", xlab = "Hora", ylab = "Nivel")
  })
  
  output$estacion.info.contaminante <- renderText({
    paste("Contaminante: ", "28079004", "\n",
          "Estación: ", paste0("",input$estacion.estacion,""), "\n",
          "Fecha: ", Sys.Date(), "\n",
          "Hora: ", Sys.time())
  })
  
  # Contaminantes de la estacion seleccionada para prediccion
  observeEvent(input$prediccion.estacion, {
    updateSelectInput(session, "prediccion.contaminante", choices = names(dfContEstaciones$dfCont[[paste0("",input$prediccion.estacion,"")]]))
  })
  
  # Modelo de prediccion
  output$prediccion.grafico <- renderPlot({
    getPrediction(meteo,dfContEstaciones[[1]],paste0("",input$prediccion.estacion,""),paste0("",input$prediccion.contaminante,""))$plot
    
  })
  
  # Parámetros de la prediccion
  output$prediccion.modelo <- renderPrint({
    getPrediction(meteo,dfContEstaciones[[1]],paste0("",input$prediccion.estacion,""),paste0("",input$prediccion.contaminante,""))$modeloFuturo
  })
  
  # Error de la prediccion
  output$prediccion.mape <- renderValueBox({
    mapeValor <- getPrediction(meteo,dfContEstaciones[[1]],paste0("",input$prediccion.estacion,""),paste0("",input$prediccion.contaminante,""))$mape %>% 
          round(4)
    mapeValor <- paste0("",mapeValor*100," %")
    valueBox(
      mapeValor,
      h4("Error del modelo (MAPE)"),
      icon = icon("xmark"),
      color = "red"
      )
    
  })
  
  # Ultimo valor de contaminante
  output$prediccion.actual <- renderValueBox({
    actualValor <- dfContEstaciones$dfCont[[paste0("",input$prediccion.estacion,"")]][[paste0("",input$prediccion.contaminante,"")]]$VALOR %>%
      last() %>% 
      round(4)
    valueBox(
      actualValor,
      h4("Nivel actual"),
      icon = icon("clock"),
      color = "blue"
    )
    
  })
  
}

shinyApp(ui, server)
