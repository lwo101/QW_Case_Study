library(dplyr)
library(plyr)
library(tidyr)

einzelteil_df <- read.csv("Einzelteil_T11.csv", header = TRUE, sep = "|")

einzelteil_df_right <- einzelteil_df[c("X11","IDNummer1", "Produktionsdatum1", "Herstellernummer1", "Werksnummer1", "Fehlerhaft1", "Origin1")]
einzelteil_df_left <- einzelteil_df[c("X1","IDNummer", "Produktionsdatum", "Herstellernummer", "Werksnummer", "Fehlerhaft", "Origin")]
rm(einzelteil_df)

einzelteil_df_right <- rename(einzelteil_df_right, c("X11"="X1","IDNummer1"="IDNummer", "Produktionsdatum1"="Produktionsdatum", "Herstellernummer1"="Herstellernummer", "Werksnummer1"="Werksnummer", "Fehlerhaft1"="Fehlerhaft", "Origin1"="Origin"))

einzelteil_df_combined <- bind_rows(einzelteil_df_left, einzelteil_df_right)

einzelteil_df_combined$IDNummerString <- einzelteil_df_combined$IDNummer
einzelteil_df_combined_splitted <- separate(einzelteil_df_combined, col=IDNummer, into = c("a", "b", "c", "d"), sep = "-")
einzelteil_df_combined_transformed <- transform(einzelteil_df_combined_splitted, a = as.numeric(a), b = as.numeric(b),c = as.numeric(c),d = as.numeric(d))
einzelteil_df_combined_filtered <- filter(einzelteil_df_combined_transformed, d <= 30100 & d >= 19000 & b == 213 & c == 2131 & a == 11)

#fuer R markdown
summary(einzelteil_df_combined_filtered)
# Wie man sieht nur die geforderten ID Nummern...

#Nächste Tabelle Yay
bestandteil_K2LE1 <- read.csv("Bestandteile_Komponente_K2LE1.csv", header = TRUE, sep = ";")
new <- inner_join(einzelteil_df_combined_filtered, bestandteil_K2LE1, by = c("IDNummerString" = "ID_T11"), copy = FALSE)

#Nächtes Tabelle Yay
bestandteil_K2ST1 <- read.csv("Bestandteile_Komponente_K2ST1.csv", header = TRUE, sep = ";")
new2 <- inner_join(einzelteil_df_combined_filtered, bestandteil_K2ST1, by = c("IDNummerString" = "ID_T11"), copy = FALSE)