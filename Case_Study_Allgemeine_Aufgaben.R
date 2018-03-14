library(tidyverse)

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
logistikverzug_selected_df$Produktionsdatum <- as.POSIXct(logistikverzug_selected_df$Produktionsdatum, "", "%Y-%m-%d")
logistikverzug_selected_df$Wareneingang <- as.POSIXct(logistikverzug_selected_df$Wareneingang, "", "%d.%m.%Y")

# erstellen einer neuen Spalte mit der Lieferzeit. Damit die berechneten Angaben in ganzen Tagen angegeben werden runden wir das Ergebnis auf ganze Zahlen
logistikverzug_mutated_df <- mutate(logistikverzug_selected_df, Lieferzeit = round(as.double(Wareneingang - Produktionsdatum), digits = 0))

# Bestimmung der minimalen und maximalen Lieferzeit
min(logistikverzug_mutated_df$Lieferzeit)
max(logistikverzug_mutated_df$Lieferzeit)

# # Bestimmung der verschiedenen Lieferzeiten
# lieferzeiten_unique <- unique(logistikverzug_mutated_df$Lieferzeit)
#  n_lieferzeiten<- NROW(lieferzeiten_unique)

# Plot der Häufigkeit dercLieferzeiten zur Übersicht (Histogram: ..density.. gibt normierte y-Achse aus)
ggplot(logistikverzug_transformed_df, aes(x = Lieferzeit)) +
  geom_histogram(binwidth = 1, aes(y = ..density..), bins = n_lieferzeiten)


# # AUfteilung der Spalte "IDNummer" in mehrere Spalten mit int Datentypen (möglicherweise nicht nötig)
# logistikverzug_separated_df <- separate(logistikverzug_mutated_df, col=IDNummer, into = c("ID_1", "ID_2", "ID_3", "ID_4"), sep = "-")
# logistikverzug_transformed_df <- transform(logistikverzug_separated_df, ID_2 = as.numeric(ID_2),ID_3 = as.numeric(ID_3), ID_4 = as.numeric(ID_4))

# Testplot zur Erstellung eines Verteilungsmodells
# lin_mod <- lm( Lieferzeit ~ Produktionsdatum, data = logistikverzug_mutated_df)
# 
# ggplot(logistikverzug_mutated_df, aes(x = Produktionsdatum, y = Lieferzeit, col = as.factor(Werksnummer))) +
#   geom_point(alpha = 0.2) 




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
