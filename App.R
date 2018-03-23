# Einbinden der Bibliotheken
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

if(!require(DT)){
  install.packages("DT")
  require(DT)
}

# Einlesen des Datensatzes
data <- read.csv("prepared_data.csv", header = TRUE, sep = ",", dec = ",") %>%
  filter(!is.na(Breitengrad) & !is.na(Laengengrad))
dataSelected <- select(data, c("Laengengrad", "Breitengrad", "Zulassungstag", "Werksnummer_Fahrzeug", "ID_Fahrzeug", "Gemeinden"))

getColor <- function(dataSelected) {
  sapply(dataSelected$Werksnummer_Fahrzeug, function(Werksnummer_Fahrzeug) {
    if(Werksnummer_Fahrzeug == 11) {
      "black"
    } else if(Werksnummer_Fahrzeug == 12) {
      "blue"
    }
  })
}

icons <- awesomeIcons(
  icon = 'ion-android-car',
  iconColor = 'white',
  library = 'ion',
  markerColor = getColor(dataSelected)
)

zulassungApp <- shinyApp(
  ui <- fluidPage(
    # headerPanel(
    titlePanel(
      title = "Zulassung der Autos"
    ),
    
    sidebarLayout(
      sidebarPanel( 
        fluidRow(
          column(
            width = 12,
            checkboxGroupInput(
              inputId = "kategorie",
              label = "Autotyp",
              choices = list("Typ 11" = 11, "Typ 12" = 12),
              selected = c(11, 12)
            )
          )
        ),
        
        sliderInput(
          inputId = "range", 
          label = "Tag im März:",
          min = 1, 
          max = 31,
          value = range(dataSelected$Zulassungstag, na.rm = TRUE),
          step = 1
        )
      ),
      
      mainPanel(
        tabsetPanel(
          tabPanel(
            title = "Karte",
            leafletOutput(
              outputId = "karte", width = "100%", height = 800
            )
          ),
          
          tabPanel(
            title = "Heatmap",
            leafletOutput(
              outputId = "heatmap", width = "100%", height = 800
            )
          ),
          
          tabPanel(
            title = "Datensatz",
            dataTableOutput("datensatz")
          )
        )
      )
    )
  ),
  
  server <- function(input, output, session){
    dataFiltered <- reactive({
      dataSelected %>%
        filter(Zulassungstag >= input$range[1] & Zulassungstag <= input$range[2] & Werksnummer_Fahrzeug %in% input$kategorie)
    })
    
    output$karte <- renderLeaflet({
      # Statischer Teil: Karte wird nur einmal gezeichnet.
      leaflet(dataSelected) %>%
        # addTiles() %>%
        addProviderTiles("Stamen.TonerLite", options = providerTileOptions(minZoom = 6, maxZoom = 13)) %>%
        fitBounds(
          ~min(Laengengrad), ~min(Breitengrad), 
          ~max(Laengengrad), ~max(Breitengrad))
    })
    
    # Dynamischer Teil: Karte reagiert in Echtzeit auf die angewandten Filter, der statische Teil, d.h. die Karte 
    # wird nicht neu geladen.
    observe({
      leafletProxy("karte", data = dataFiltered()) %>%
        setMaxBounds(
          ~min(Laengengrad), ~min(Breitengrad), 
          ~max(Laengengrad), ~max(Breitengrad)) %>%
        
        # clear-Befehle als Reset vor jeder erneuten Filterveränderung
        clearShapes() %>%
        clearPopups() %>%
        clearMarkers() %>%
        clearMarkerClusters %>%
        
        addAwesomeMarkers(
          lng = ~Laengengrad, 
          lat = ~Breitengrad,
          icon = icons,
          popup = ~paste(
            "<b>FahrzeugID: </b>", ID_Fahrzeug, "<br>",
            "<b>Zulassungstag: </b>", Zulassungstag, "<br>",
            "<b>Gemeinde: </b>", Gemeinden, "<br>",
            "<b>Fahrzeugtyp: </b>", Werksnummer_Fahrzeug, "<br>"),
          clusterOptions = markerClusterOptions()
        )
    })
    
    # Heatmap
    output$heatmap <- renderLeaflet({
      leaflet(dataSelected) %>%
        setMaxBounds(
          ~min(Laengengrad), ~min(Breitengrad), 
          ~max(Laengengrad), ~max(Breitengrad)) %>%
        addTiles(options = tileOptions(minZoom = 6, maxZoom = 13)) %>%
        fitBounds(
          ~min(Laengengrad), ~min(Breitengrad), 
          ~max(Laengengrad), ~max(Breitengrad)) %>%
        addHeatmap(lng = ~Laengengrad, lat = ~Breitengrad, max = .6, blur = 60)
    })
    
    observe({
      leafletProxy("heatmap", data = dataFiltered()) %>%
        # Reset Heatmap
        clearHeatmap %>%
        # Update Heatmap
        addHeatmap(lng = ~Laengengrad, lat = ~Breitengrad, max = .6, blur = 60)
    })
    
    # Datensatz (Tabelle)
    output$datensatz <- renderDataTable({
      DT::datatable(dataSelected)
    })
  }
)

runApp(zulassungApp)