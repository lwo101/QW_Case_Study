library(tidyverse)

# einlesen der Datei Komponente_K7 
komponente_k7 <- read.csv2("Komponente_K7.csv", header = TRUE, stringsAsFactors = FALSE)

# einlesen der Datei Logistikverzug_K7
logistikverzug_k7 <- read.csv2("Logistikverzug_K7.csv", header = TRUE, stringsAsFactors = FALSE)

# zusammenfügen der beiden Datensätze
logistikverzug_combined_df <- left_join(komponente_k7, logistikverzug_k7, by = "IDNummer")

# löschen der doppelten Spalten 
logistikverzug_selected_df <-  select(logistikverzug_combined_df, 1:6, 8)

# umbenennen der Spalten
column_names <- c( "X","IDNummer", "Produktionsdatum", "Herstellernummer","Werksnummer", "Fehlerhaft", "Wareneingang")
names(logistikverzug_selected_df) <- column_names

# umwandeln der Spalten "Produktionsdatum" und "Wareneingang" in POSIXct (Format für Datumsangaben)
logistikverzug_selected_df$Produktionsdatum <- as.POSIXct(logistikverzug_selected_df$Produktionsdatum, "", "%Y-%m-%d")
logistikverzug_selected_df$Wareneingang <- as.POSIXct(logistikverzug_selected_df$Wareneingang, "", "%d.%m.%Y")

# erstellen einer neuen Spalte mit den Werten des Logistikverzugs in Tagen
logistikverzug_mutated_df <- mutate(logistikverzug_selected_df, Logistikverzug = as.double(Wareneingang - Produktionsdatum)) 

# der minimale und maximale Logistikverzug lässt sich aus der summary für den Datensatz ablesen
summary(logistikverzug_mutated_df)

# Testplot
hist(logistikverzug_mutated_df$Logistikverzug)