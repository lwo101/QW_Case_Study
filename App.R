# Einbinden der Bibliotheken und falls nicht vorhanden, fehlende Packages installieren.
if(!require(shiny)){
  install.packages("shiny")
  require(shiny)
}

if(!require(leaflet)){
  install.packages("leaflet")
  require(leaflet)
}

if(!require(leaflet.extras)){
  install.packages("leaflet.extras")
  require(leaflet.extras)
}

if(!require(tidyverse)){
  install.packages("tidyverse")
  require(tidyverse)
}

if(!require(gplots)){
  install.packages("gplots")
  require(gplots)
}

# Einlesen des Datensatzes
data <- read.csv("prepared_data.csv", header = TRUE, sep = ",", dec = ",")
dataSelected <- select(data, c("Laengengrad", "Breitengrad", "Zulassungsdatum", "Zulassungstag", "Werksnummer_Fahrzeug", "ID_Fahrzeug", "Gemeinden"))

# Funktion zur Definition von Farben für den entsprechenden Autotyp. Diese werden in der Leaflet-Karte als Markerfarben genutzt.
getColor <- function(dataSelected) {
  sapply(dataSelected$Werksnummer_Fahrzeug, function(Werksnummer_Fahrzeug) {
    if(Werksnummer_Fahrzeug == 11) {
      "black"
    } else if(Werksnummer_Fahrzeug == 12) {
      "blue"
    }
  })
}

# Auswahl eines Icons als Marker auf der Karte
icons <- awesomeIcons(
  icon = 'ion-android-car',
  iconColor = 'white',
  library = 'ion',
  markerColor = getColor(dataSelected)
)

# Frontend: Design der Shiny-Oberfläche
ui <- fluidPage(
  titlePanel(title = "Zulassung der Autos"),
  
  sidebarLayout(
    sidebarPanel(
      fluidRow(
        column(
          width = 12,
          
          # Multiple Auswahlen zur Filterung nach Autotyp über Checkboxen möglich, standardmäßig wird alles ausgewählt.
          checkboxGroupInput(
            inputId = "kategorie",
            label = "Autotyp:",
            choices = list("Typ 11" = 11, "Typ 12" = 12),
            selected = c(11, 12)
          )
        )
      ),
      
      # Filterung mithilfe eines Schiebereglers nach Zulassungstag, standardmäßig wird der maximal verfügbare Zeitbereich aus dem Datensatz gewählt.
      sliderInput(
        inputId = "range", 
        label = HTML('Tag im M&auml;rz:'),
        min = 1, 
        max = 31,
        value = range(dataSelected$Zulassungstag, na.rm = TRUE),
        step = 1
      )
    ),
    
    # Im main-Panel wird die Leaflet-Karte dargestellt. Dabei wird die Höhe dynamisch nach Fenstergröße reguliert (beste Darstellung im Browser).
    mainPanel(
      tags$style(type = "text/css", "#karte {height: calc(100vh - 80px) !important;}"),
          title = "Karte",
          leafletOutput(
            outputId = "karte"
      ),
      
      # Copyright
      fluidRow(
        HTML('<div align = "right"><span style="color:#000000">IDA Casestudy 2018</span> <span style="color:#428bca"><b>&copy; by Gruppe 6</b>&nbsp;&nbsp;&nbsp;</span></div>')
      )  
    )
  )
)

# Backend: Der Applikation zugrundeliegende Funktionen.
server <- function(input, output, session){
  
  # Die aus den Filterkriterien gebildete Schnittmenge wird von einem reactive-Container umschlossen.
  dataFiltered <- reactive({
    dataSelected %>%
      filter(Zulassungstag >= input$range[1] & Zulassungstag <= input$range[2] & Werksnummer_Fahrzeug %in% input$kategorie)
  })
  
  # Statischer Teil der Leaflet-Karte wird nur einmal gezeichnet.
  output$karte <- renderLeaflet({
    leaflet(dataSelected) %>%
      addProviderTiles("Stamen.TonerLite", group = "Simpel (Standard)", options = providerTileOptions(minZoom = 5, maxZoom = 13)) %>%
      addTiles(group = "Open Street Map") %>%
      
      # Fokus bzw. Zentrierung auf die Randkoordinaten von Deutschland (Datensatz)
      fitBounds(
        ~min(Laengengrad)-1, ~min(Breitengrad)-1, 
        ~max(Laengengrad)+1, ~max(Breitengrad)+1)
  })
  
  # Dynamischer Teil der Karte, Marker und weitere Objekte werden je nach Filtereinstellung resettet und aktualisiert.
  observe({
    leafletProxy("karte", data = dataFiltered()) %>%
      setMaxBounds(
        ~min(Laengengrad)-1, ~min(Breitengrad)-1, 
        ~max(Laengengrad)+1, ~max(Breitengrad)+1) %>%
      
      # clear-Befehle als Reset vor jeder erneuten Filterveränderung
      clearShapes() %>%
      clearPopups() %>%
      clearMarkers() %>%
      clearMarkerClusters %>%
      clearHeatmap %>%
      
      # Einfügen einer Heatmap auf einer separaten Ebene (bzw. andere Gruppenzuordnung)
      addHeatmap(lng = ~Laengengrad, lat = ~Breitengrad, max = .6, blur = 60, group = "Heatmap") %>%
      
      # Verwendung von awesomeMarkers für eine individuelle, intuitive Darstellung von Markern bzw. Icons.
      addAwesomeMarkers(
        lng = ~Laengengrad, 
        lat = ~Breitengrad,
        icon = icons,
        
        # Popup, der alle erforderlichten, für den User relevanten Informationen gefiltert anzeigt.
        popup = ~paste(
          "<b>FahrzeugID: </b>", ID_Fahrzeug, "<br>",
          "<b>Zulassungsdatum: </b>", Zulassungsdatum, "<br>",
          "<b>Gemeinde: </b>", Gemeinden, "<br>",
          "<b>Fahrzeugtyp: </b>", Werksnummer_Fahrzeug, "<br>"),
        
        # Gruppierung der Marker in Cluster je nach Zoom-Level für eine bessere Übersicht.
        clusterOptions = markerClusterOptions(),
        group = "Detailliert"
      ) %>%
      
      # Feinere Einstellungsmöglichkeiten, u.a. Kartenstil, überlagende Ebenen,
      addLayersControl(
        baseGroups = c("Simpel (Standard)", "Open Street Map"),
        overlayGroups = c("Detailliert", "Heatmap"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)
      ) %>%
      
      
      # Heatmap-Layer bei Start der Applikation ausblenden.
      hideGroup(group = "Heatmap") %>%
      
      # Kartenausschnittin Form einer Mini-Map mit aktueller Position.
      addMiniMap(width = "80", height = "80", toggleDisplay = "TRUE", zoomAnimation = "TRUE", autoToggleDisplay = "TRUE", minimized = "FALSE")
  })
}

# Aufruf der Shiny-Applikation
shinyApp(ui = ui, server = server)