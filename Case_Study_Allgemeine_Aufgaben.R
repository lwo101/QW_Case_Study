library(tidyverse)
library(lubridate)

# einlesen der Datei Komponente_K7 
komponente_k7 <- read.csv2("Komponente_K7.csv", header = TRUE, stringsAsFactors = FALSE)

# einlesen der Datei Logistikverzug_K7
logistikverzug_k7 <- read.csv2("Logistikverzug_K7.csv", header = TRUE, stringsAsFactors = FALSE)

# zusammenfügen der beiden Datensätze
logistikverzug_combined_df <- left_join(komponente_k7, logistikverzug_k7, by = "IDNummer")
# bei doppelt vorkommenden Spalten benennt R die Spalte des ersten Datasets mit .x und die Spalte mit gleichem Namen des zweiten Datasets mit .y

# mit dem Befehl str() können wir einen Überblick über den gejointen Datensatz erhalten
# str(logistikverzug_combined_df)
# der so gejointe Datensatz enthält 11 Merkmale (Spalten) mit 306490 Zeilen (observations)

# löschen der doppelten Spalten 
logistikverzug_selected_df <-  select(logistikverzug_combined_df, 1:6, 8)

# umbenennen der Spalten
column_names <- c( "X","IDNummer", "Produktionsdatum", "Herstellernummer","Werksnummer", "Fehlerhaft", "Wareneingang")
names(logistikverzug_selected_df) <- column_names

# umwandeln der Spalten "Produktionsdatum" und "Wareneingang" in POSIXct (Format für Datumsangaben)
logistikverzug_selected_df$Produktionsdatum <- as.Date(logistikverzug_selected_df$Produktionsdatum)
logistikverzug_selected_df$Wareneingang <- as.Date(logistikverzug_selected_df$Wareneingang, format = "%d.%m.%Y")


<<<<<<< HEAD
# erstellen einer neuen Spalte mit der Lieferzeit in Tagen
logistikverzug_mutated_df <- mutate(logistikverzug_selected_df, Produktionsdatum = as.Date(Produktionsdatum, origin = "%Y-%m-%d"), Lieferzeit = as.double(Wareneingang - Produktionsdatum), Farbe = "grey")

=======
# erstellen einer neuen Spalte mit der Lieferzeit. Damit die berechneten Angaben in ganzen Tagen angegeben werden runden wir das Ergebnis auf ganze Zahlen
logistikverzug_mutated_df <- mutate(logistikverzug_selected_df, Lieferzeit = round(as.double(Wareneingang - Produktionsdatum), digits = 0))
>>>>>>> 1c844eeec07a8ea33c3637dd7fff8306ad384532

# Bestimmung der minimalen und maximalen Lieferzeit
min(logistikverzug_mutated_df$Lieferzeit)
max(logistikverzug_mutated_df$Lieferzeit)

# # Bestimmung der verschiedenen Lieferzeiten
# lieferzeiten_unique <- unique(logistikverzug_mutated_df$Lieferzeit)
#  n_lieferzeiten<- NROW(lieferzeiten_unique)

<<<<<<< HEAD


# # Testplot (Histogram: ..density.. gibt normierte y-Achse aus)
# ggplot(logistikverzug_transformed_df, aes(x = Lieferzeit)) +
#   geom_histogram(binwidth = 1, aes(y = ..density..))
=======
# Plot der Häufigkeit dercLieferzeiten zur Übersicht (Histogram: ..density.. gibt normierte y-Achse aus)
ggplot(logistikverzug_transformed_df, aes(x = Lieferzeit)) +
  geom_histogram(binwidth = 1, aes(y = ..density..), bins = n_lieferzeiten)


# # AUfteilung der Spalte "IDNummer" in mehrere Spalten mit int Datentypen (möglicherweise nicht nötig)
# logistikverzug_separated_df <- separate(logistikverzug_mutated_df, col=IDNummer, into = c("ID_1", "ID_2", "ID_3", "ID_4"), sep = "-")
# logistikverzug_transformed_df <- transform(logistikverzug_separated_df, ID_2 = as.numeric(ID_2),ID_3 = as.numeric(ID_3), ID_4 = as.numeric(ID_4))
>>>>>>> 1c844eeec07a8ea33c3637dd7fff8306ad384532

# Testplot zur Erstellung eines Verteilungsmodells
# lin_mod <- lm( Lieferzeit ~ Produktionsdatum, data = logistikverzug_mutated_df)
# 
# ggplot(logistikverzug_mutated_df, aes(x = Produktionsdatum, y = Lieferzeit, col = as.factor(Werksnummer))) +
#   geom_point(alpha = 0.2) 




<<<<<<< HEAD
# Beginn der Shiny Applikation
library(shiny)

ui <- fluidPage(
# Überschrift hinzufügen
  headerPanel(title = "Allgemeine Aufgaben"),

# Erstellung des SidebarLayouts
  sidebarLayout(
# Erstellung eines SidebarPanels
    sidebarPanel(
# Implementierung eines Blocks zur Zeitraumwahl
      dateRangeInput(
        inputId = "date_range",
        label = "Zeitraum",
        min = min(logistikverzug_mutated_df$Produktionsdatum), # frühestes auswählbares Datum
        max = max(logistikverzug_mutated_df$Wareneingang), # spätestes auswählbares Datum
        start = min(logistikverzug_mutated_df$Produktionsdatum), # vorausgewähltes Startdatum
        end = max(logistikverzug_mutated_df$Wareneingang) # vorausgewähltes Enddatum
      ),
      
      #Radiobuttons zur Eingabe, würde aber eher richtung Actionbutton gehen, weil so eher schwer
        radioButtons("radio_month", label = h3("Month"),
             choices = list("January" = "1", "February" = "2", "March" = "3", "April" = "4", "May" = 05,
                            "June" = 06, "July" = 07, "August" = 08, "September" = 09, "Oktober" = 10,
                            "November" = 11, "December" = 12)
      ),

        radioButtons("radio_color", label = h3("Color"),
                     choices = list("grey", "black", "red", "green", "blue", "yellow")
      )
  ),

# Erstellung des Main-Panels mit zwei verschiedenen Reitern
    mainPanel(
      tabsetPanel(
# Erstellung des ersten Reiters
        tabPanel(
          title = "Produktionsmenge",
          # div("Balkendiagramm: Produktionsmenge") # Platzhalter für Diagramm
          plotOutput(
            # An dieser Stelle wird output$plot dargestellt
            outputId = "plot_produktionsmenge"
          )
        ),
# Erstellung des zweiten Reiters
        tabPanel(
          title = "Karosserien in Auslieferung",
          div("Balkendiagramm: Karosserien in Auslieferung") # Platzhalter für Diagramm
        )
      )
    )
  )
)
server <- function(session, input, output) {

# Erstellen des Zeitraums anhand des Inputs
  
  zeitraum_df <-  reactive({
    
    startdatum <- as.Date(input$date_range[1])
    enddatum <- as.Date(input$date_range[2])
    
    df <- color_month()
    filter(df, logistikverzug_mutated_df$Produktionsdatum <= enddatum & logistikverzug_mutated_df$Produktionsdatum >= startdatum)
  
  })
  
  # Ändert die Farbe anhand der Inputs
  #Funktioniert nich wie gewünscht
  color_month <- reactive({
   
    logistikverzug_mutated_df <- logistikverzug_mutated_df %>%
      mutate(Farbe = ifelse(month(Produktionsdatum) == input$radio_month, input$radio_color, Farbe))
    
    logistikverzug_mutated_df
    
  
    
  })
  

# Funktion zur Erzeugung des Diagramms im ersten Reiter
  output$plot_produktionsmenge <- renderPlot({
    #Achte auf korrekte Eingabe der Daten
    validate(
      need(input$date_range[1] < input$date_range[2], "Bitte auf Datum achten")
    )
    
    ggplot(zeitraum_df(), aes(x = format(Produktionsdatum, "%m %Y"), fill = Farbe), y = IDNummer) +
        geom_bar()+
        xlab("Produktionsdatum")+
        theme(axis.text.x = element_text(size = 10, color = "black", angle = 45))+
        ylab("Anzahl")# nicht finales Diagramm
  })

}

shinyApp(ui = ui, server = server)
=======
# # Beginn der Shiny Applikation
# library(shiny)
# 
# ui <- fluidPage(
# # Überschrift hinzufügen
#   headerPanel(title = "Allgemeine Aufgaben"),
# 
# # Erstellung des SidebarLayouts
#   sidebarLayout(
# # Erstellung eines SidebarPanels
#     sidebarPanel(
# # Implementierung eines Blocks zur Zeitraumwahl
#       dateRangeInput(
#         inputId = "date_range",
#         label = "Zeitraum",
#         min = min(logistikverzug_mutated_df$Produktionsdatum), # frühestes auswählbares Datum
#         max = max(logistikverzug_mutated_df$Wareneingang), # spätestes auswählbares Datum
#         start = min(logistikverzug_mutated_df$Produktionsdatum), # vorausgewähltes Startdatum
#         end = max(logistikverzug_mutated_df$Wareneingang) # vorausgewähltes Enddatum
#       )
#     ),
# 
# # Erstellung des Main-Panels mit zwei verschiedenen Reitern
#     mainPanel(
#       tabsetPanel(
# # Erstellung des ersten Reiters
#         tabPanel(
#           title = "Produktionsmenge",
#           # div("Balkendiagramm: Produktionsmenge") # Platzhalter für Diagramm
#           plotOutput(
#             # An dieser Stelle wird output$plot dargestellt
#             outputId = "plot_produktionsmenge"
#           )
#         ),
# # Erstellung des zweiten Reiters
#         tabPanel(
#           title = "Karosserien in Auslieferung",
#           div("Balkendiagramm: Karosserien in Auslieferung") # Platzhalter für Diagramm
#         )
#       )
#     )
#   )
# )
# 
# server <- function(session, input, output) {
# 
#   zeitraum_df <-  reactive({
#     startdatum <- input$date_range[[1]]
#     enddatum <- input$date_range[[2]]
#     filter(logistikverzug_mutated_df, logistikverzug_mutated_df$Produktionsdatum <= enddatum && logistikverzug_mutated_df$Produktionsdatum >= startdatum)
#   })
# 
# # Funktion zur Erzeugung des Diagramms im ersten Reiter
#   output$plot_produktionsmenge <- renderPlot({
#     ggplot(zeitraum_df(), aes(x = Produktionsdatum, y = Lieferzeit, col = as.factor(Werksnummer))) +
#         geom_point(alpha = 0.2)    # nicht finales Diagramm
#   })
# 
# }
# 
# shinyApp(ui = ui, server = server)
>>>>>>> 1c844eeec07a8ea33c3637dd7fff8306ad384532
