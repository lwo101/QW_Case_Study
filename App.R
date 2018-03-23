require(dplyr)
require(leaflet)
require(leaflet.extras)
require(shiny)
require(tidyverse)

data <- read.csv("prepared_data.csv", header = TRUE, sep = ",", dec = ",") %>%
  filter(!is.na(Breitengrad) & !is.na(Laengengrad))

dataSelected <- select(data, c("Laengengrad", "Breitengrad", "Zulassungstag", "Werksnummer_Fahrzeug", "ID_Fahrzeug", "Gemeinden", "Zulassungsdatum"))

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  headerPanel(title = "Zulassung der Autos"),
  leafletOutput("karte", width = "100%", height = "100%"),
  
  absolutePanel(
    top = 100,
    right = 50,
      
    fluidRow(
      column(
        width = 12,
          checkboxGroupInput(
            inputId = "kategorie",
            label = "Autotyp:",
            choices = list("Typ 11" = 11,
                           "Typ 12" = 12),
            selected = c(11, 12)
          )
      )
    ),
      
    sliderInput(
      "range",
      "Tag im März:",
      min = 1,
      max = 31,
      value = range(dataSelected$Zulassungstag, na.rm = TRUE),
      step = 1
    )
  ),
  
  absolutePanel(
    bottom = 10,
      left = 10,
      HTML('<a href="http://boxio.cc" target="_new"><span style="color:#0000CC">IDA Casestudy 2018 &copy; by Gruppe 6</span></a>')
  )
)

server <- function(input, output, session){
  filteredData <- reactive({
    dataSelected %>%
      filter(Zulassungstag >= input$range[1] & Zulassungstag <= input$range[2] & Werksnummer_Fahrzeug %in% input$kategorie)
  })
   
  output$karte <- renderLeaflet({
    # Statischer Teil: Karte wird nur einmal gezeichnet.
    leaflet(dataSelected) %>%
    addTiles() %>%
    fitBounds(
      ~min(Laengengrad), ~min(Breitengrad), 
      ~max(Laengengrad), ~max(Breitengrad))
    })
   
  # Dynamischer Teil: Karte reagiert in Echtzeit auf die angewandten Filter, der statische Teil, d.h. die Karte 
  # wird nicht neu geladen.
  observe({
    leafletProxy("karte", data = filteredData()) %>%
    
    # clear-Befehle als Reset vor jeder erneuten Filterveränderung
    clearShapes() %>%
    clearPopups() %>%
    clearMarkers() %>%
    clearMarkerClusters %>%
      
    addCircleMarkers(
      clusterOptions = markerClusterOptions(),
      lng = ~Laengengrad, 
      lat = ~Breitengrad,
      popup = ~paste(
        "<b>FahrzeugID: </b>", ID_Fahrzeug, "<br>",
        "<b>Zulassungstag: </b>", Zulassungsdatum, "<br>",
        "<b>Gemeinde: </b>", Gemeinden, "<br>",
        "<b>Fahrzeugtyp: </b>", Werksnummer_Fahrzeug, "<br>")
    )
  })
}

# Aufruf der Shiny-App
shinyApp(ui = ui, server = server)