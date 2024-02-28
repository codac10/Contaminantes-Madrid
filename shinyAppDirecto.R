require("shiny");require("plotly");require("shinydashboard");require("corrplot");
require("rlist");require("forecast");require("leaflet");require("bslib")

setwd("/Users/pablocodina/Library/Mobile Documents/com~apple~CloudDocs/UPM/TFG/DatosTiempoReal")

source("./getInstantDataMeteo.R");source("./getStationMeteoData.R")

# Obtener todos los datos de las estaciones metereologicas con sus contaminantes
dfMeteo <- getInstantDataMeteo()
dfMeteoEstaciones <- getStationMeteoData(dfMeteo)


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
                      column(6,selectInput("estacion.estacion", "Estación:", choices = c(unique(names(dfMeteoEstaciones$dfMeteo))))),
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
                             selectInput("prediccion.estacion", "Estación:", choices = c()),
                             selectInput("prediccion.contaminante", "Contaminante:", choices = c()),
                         ),
                       ),
                       fluidRow(
                         valueBox(0, h4("Nivel actual"), icon = icon("clock"), color = "blue", width = 6),
                         valueBox(0, h4("Error del modelo (MAPE)"), icon = icon("xmark"), color = "red", width = 6),
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
                  height = 400
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
    updateSelectInput(session, "estacion.contaminante", choices = names(dfMeteoEstaciones$dfMeteo[[paste0("",input$estacion.estacion,"")]]))
  })
  
  # Grafico de la estacion y contaminante seleccionados
  output$estacion.grafico <- renderPlot({
    ggplot() +
      geom_line(data = dfMeteoEstaciones$dfMeteo[[paste0("",input$estacion.estacion,"")]][[paste0("",input$estacion.contaminante,"")]],
                aes(x = FECHA, y = VALOR)) +
      theme_minimal()
    
  })
  
  output$estacion.contaminantes.directo <- renderPlot({
    plot(dfMeteoEstaciones$dfMeteo, type = "l", col = 1, lwd = 2, main = "Contaminantes en directo", xlab = "Hora", ylab = "Nivel")
  })
  
  output$estacion.meteo.directo <- renderPlot({
    plot(dfMeteoEstaciones$dfMeteo$`28079004`, type = "l", col = 1, lwd = 2, main = "Meteorología en directo", xlab = "Hora", ylab = "Nivel")
  })
  
  output$estacion.info.contaminante <- renderText({
    paste("Contaminante: ", "28079004", "\n",
          "Estación: ", "28079004", "\n",
          "Fecha: ", Sys.Date(), "\n",
          "Hora: ", Sys.time())
  })
  
  output$prediccion.estacion <- renderUI({
    selectInput("prediccion.estacion", "Estación", choices = dfMeteoEstaciones$dfEstaciones$ESTACION)
  })
  
  output$prediccion.contaminante <- renderUI({
    selectInput("prediccion.contaminante", "Contaminante", choices = names(dfMeteoEstaciones$dfMeteo))
  })
  
  output$prediccion.datos <- renderText({
    paste("Contaminante: ", "28079004", "\n",)
  })
  
}

shinyApp(ui, server)
