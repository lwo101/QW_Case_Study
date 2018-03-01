library(plyr)
library(dplyr)
library(tidyr)

einzelteil_df <- read.csv("Einzelteil_T11.csv", header = TRUE, sep = "|")

einzelteil_df_right <- einzelteil_df[c("X11","IDNummer1", "Produktionsdatum1", "Herstellernummer1", "Werksnummer1", "Fehlerhaft1", "Origin1")]
einzelteil_df_left <- einzelteil_df[c("X1","IDNummer", "Produktionsdatum", "Herstellernummer", "Werksnummer", "Fehlerhaft", "Origin")]
rm(einzelteil_df)

einzelteil_df_right <- rename(einzelteil_df_right, c("X11"="X1","IDNummer1"="IDNummer", "Produktionsdatum1"="Produktionsdatum", "Herstellernummer1"="Herstellernummer", "Werksnummer1"="Werksnummer", "Fehlerhaft1"="Fehlerhaft", "Origin1"="Origin"))

einzelteil_df_combined <- bind_rows(einzelteil_df_left, einzelteil_df_right)
rm(einzelteil_df_left)
rm(einzelteil_df_right)


einzelteil_df_combined$IDNummerString <- einzelteil_df_combined$IDNummer
einzelteil_df_combined_splitted <- separate(einzelteil_df_combined, col=IDNummer, into = c("a", "b", "c", "d"), sep = "-")
einzelteil_df_combined_transformed <- transform(einzelteil_df_combined_splitted, a = as.numeric(a), b = as.numeric(b),c = as.numeric(c),d = as.numeric(d))
einzelteil_df_combined_filtered <- filter(einzelteil_df_combined_transformed, d <= 30100 & d >= 19000 & b == 213 & c == 2131 & a == 11)
einzelteil_df_combined_selected <- einzelteil_df_combined_filtered[c("X1", "Produktionsdatum", "Herstellernummer","Werksnummer","Fehlerhaft","Origin","IDNummerString")]

#L??schen alter Dfs
rm(einzelteil_df_combined)
rm(einzelteil_df_combined_splitted)
rm(einzelteil_df_combined_transformed)
rm(einzelteil_df_combined_filtered)
#fuer R markdown
summary(einzelteil_df_combined_filtered)
# Wie man sieht nur die geforderten ID Nummern...

#Naechste Tabelle Yay
bestandteil_K2LE1 <- read.csv("Bestandteile_Komponente_K2LE1.csv", header = TRUE, sep = ";")
new <- inner_join(einzelteil_df_combined_selected, bestandteil_K2LE1, by = c("IDNummerString" = "ID_T11"), copy = FALSE)
rm(bestandteil_K2LE1)


#N??chtes Tabelle Yay
bestandteil_K2ST1 <- read.csv("Bestandteile_Komponente_K2ST1.csv", header = TRUE, sep = ";")
new2 <- inner_join(einzelteil_df_combined_selected, bestandteil_K2ST1, by = c("IDNummerString" = "ID_T11"), copy = FALSE)
rm(bestandteil_K2ST1)
rm(einzelteil_df_combined_selected)


#Doppelte ID Spalte entfernt
new3 <- new2[c("X1", "Produktionsdatum", "Herstellernummer","Werksnummer","Fehlerhaft","Origin","IDNummerString", "X", "ID_T12", "ID_T13", "ID_K2ST1")]

#Umbennen der uninteressanten Komponenten und vorbereiten f?r Rowbind
new <- rename(new, c("ID_T14"="ID_zweitesBauteil", "ID_T15"="ID_drittesBauteil", "ID_K2LE1" = "ID_Komponente"))
new3 <- rename(new3, c("ID_T12"="ID_zweitesBauteil", "ID_T13"="ID_drittesBauteil", "ID_K2ST1" = "ID_Komponente"))

#Rowbind
new_combind <- bind_rows(new, new3)

#Arbeitsspeicher
rm(new)
rm(new2)
rm(new3)


#N?chste Tabelle
Komponente_K2LE1 <- read.csv("Komponente_K2LE1.csv", header = TRUE, sep = ";")
Komponente_K2ST1 <- read.csv("Komponente_K2ST1.csv", header = TRUE, sep = ";")

#Join Komponenten Daten mit Rest
Komponente_K2LE1_Join <- inner_join(new_combind, Komponente_K2LE1, by = c("ID_Komponente" = "IDNummer"), copy = FALSE)
Komponente_K2ST1_Join <- inner_join(new_combind, Komponente_K2ST1, by = c("ID_Komponente" = "IDNummer"), copy = FALSE)

#Rowbind
Komponeten_combined <- bind_rows(Komponente_K2LE1_Join, Komponente_K2ST1_Join)

#Arbeitsspeicher
rm(new_combind)
rm(Komponente_K2LE1)
rm(Komponente_K2LE1_Join)
rm(Komponente_K2ST1)
rm(Komponente_K2ST1_Join)

#n?chste Tabellen
Bestandteil_Typ11 <- read.csv("Bestandteile_Fahrzeuge_OEM1_Typ11.csv", header = TRUE, sep = ";")
Bestandteil_Typ12 <- read.csv("Bestandteile_Fahrzeuge_OEM1_Typ12.csv", header = TRUE, sep = ";")

#Zusammengef?gt
Bestandteil_Fahrzeuge <- bind_rows(Bestandteil_Typ11, Bestandteil_Typ12)
#alten rausgeworfen
rm(Bestandteil_Typ11)
rm(Bestandteil_Typ12)

#Fahrzeugbestandteile mit Komponenten joinen und rauswerfen der alten
Bestandteile_Fahrzeug_joined <- inner_join(Komponeten_combined, Bestandteil_Fahrzeuge, by = c("ID_Komponente"="ID_Sitze"), copy = FALSE)
rm(Komponeten_combined)
rm(Bestandteil_Fahrzeuge)

#Laden neuer Tabellen und zusammenf?gen und l?schen der alten
Fahrzeuge_Typ11 <- read.csv("Fahrzeuge_OEM1_Typ11.csv", header = TRUE, sep =";")
Fahrzeuge_Typ12 <- read.csv("Fahrzeuge_OEM1_Typ12.csv", header = TRUE, sep =";")
Fahrzeuge <- bind_rows(Fahrzeuge_Typ11, Fahrzeuge_Typ12)
rm(Fahrzeuge_Typ11)
rm(Fahrzeuge_Typ12)

#Joinen Fahrzeuge und ihre Bestandteile und l?schen der alten
Fahrzeuge_joined <- inner_join(Bestandteile_Fahrzeug_joined, Fahrzeuge, by = c("ID_Fahrzeug" = "IDNummer"), copy = FALSE)
rm(Fahrzeuge)
rm(Bestandteile_Fahrzeug_joined)

#Laden der Zulassungen, join mit den Fahrzeugdaten und l?schen der alten Tabellen
Zulassungen <- read.csv("Zulassungen_alle_Fahrzeuge.csv", header = TRUE, sep = ";")
Zulassungen_der_Fahrzeuge <- inner_join(Fahrzeuge_joined, Zulassungen, by = c("ID_Fahrzeug" = "IDNummer"), copy = FALSE)
rm(Fahrzeuge_joined)
rm(Zulassungen)

#Laden der Geodaten und joinen mit den Zulassungen
Geodaten <- read.csv("Geodaten_Gemeinden.csv", header = TRUE, sep = ";")

Ort_der_Zulassung <- inner_join(Zulassungen_der_Fahrzeuge, Geodaten, by = c("Gemeinden" = "Gemeinde"), copy = FALSE)

#Bereinigung der Tabelle um ?berfl?ssige Daten
Ort_filtered <- Ort_der_Zulassung[c("X1", "Produktionsdatum.x", "Herstellernummer.x", "Werksnummer.x",   "Fehlerhaft.x", "Origin", "IDNummerString", "ID_zweitesBauteil",  "ID_drittesBauteil",  "ID_Komponente", "Produktionsdatum.y", "Herstellernummer.y", "Werksnummer.y", "Fehlerhaft.y", "ID_Karosserie", "ID_Schaltung", "ID_Motor", "ID_Fahrzeug", "Produktionsdatum", "Herstellernummer", "Werksnummer", "Fehlerhaft", "Gemeinden", "Zulassung", "Postleitzahl", "L.ngengrad", "Breitengrad")]


#Umbenennug
Ort_filtered <- rename(Ort_filtered, c("Produktionsdatum.x" = "Produktionsdatum_T11", "Herstellernummer.x"="Herstellernummer_T11", "Werksnummer.x"="Werksnummer_T11", "Fehlerhaft.x"="Fehlerhaft_T11", "IDNummerString"="IDNummer_T11", "Produktionsdatum.y" = "Produktionsdatum_Sitze", "Herstellernummer.y"="Herstellernummer_Sitze", "Werksnummer.y" = "Werksnummer_Sitze", "Fehlerhaft.y" = "Fehlerhaft_Sitze", "Produktionsdatum"="Produktionsdatum_Fahrzeug", "Herstellernummer"="Herstellernummer_Fahrzeug", "Werksnummer" = "Werksnummer_Fahrzeug", "Fehlerhaft"= "Fehlerhaft_Fahrzeug", "L.ngengrad"="Laengengrad"))
rm(Geodaten)
rm(Ort_der_Zulassung)
rm(Zulassungen_der_Fahrzeuge)








