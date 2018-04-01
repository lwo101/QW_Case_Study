# einbinden der benötigten Bibliotheken
if(!require(tidyverse)){
  install.packages("tidyverse")
  require(tidyverse)
}

if(!require(lubridate)){
  install.packages("lubridate")
  require(lubridate)
}

if(!require(shiny)){
  install.packages("shiny")
  require(shiny)
}

# Funktion, die einem Dataframe eine Spalte mit den bis zum jeweiligen Tag insgesamt produzierten Karosserien oder den bis zum jeweiligen Tag
# insgesamt eingegangenen Karosserien hinzufügt. Steht am Anfang der Datei, um die Funktion später nutzen zu können
calc_auslieferung <- function (df){
  k <- NROW(df)
  z <- rep(0, k)
  z[1] <- df$no[1]
  
  for (i in 2:k){
    z[i] <- df$no[i] + z[i-1]
  }
  
  df2 <- cbind(df, z)
  return(df2)
}

# einlesen der Datei Komponente_K7 
komponente_k7 <- read.csv2("Komponente_K7.csv", header = TRUE, stringsAsFactors = FALSE)

# einlesen der Datei Logistikverzug_K7
logistikverzug_k7 <- read.csv2("Logistikverzug_K7.csv", header = TRUE, stringsAsFactors = FALSE)

# Beim Zusammenfügen der beiden Datensätze mittels eines inner_joins sollte die zuvor bestimmte Zeilenzahl
# erhalten bleiben und, vorausgesetzt beide Datensätze enthalten Informationen zu jeder Produkt ID, sollte
# es im gejointen Datensatz keine NA's geben. Dazu benutzen wir die IDNummer als Primarykey
logistikverzug_combined_df <- inner_join(komponente_k7, logistikverzug_k7, by = "IDNummer")

# Zur Bearbeitung der vorliegenden AUfgabe, verkleinern wir den Datensatz um die doppelt vorkommenden Spalten.
# Zudem liefern uns die Angaben der Werks- und Herstellernummer, sowie die Spalte über die Fehlerhaftigkeit des Bauteils
# keine relevanten Informationen, wesahlb wir auch diese Spalten aus dem Datensatz löschen
logistikverzug_selected_df <-  select(logistikverzug_combined_df, 1:3, 8)

# Zur besseren Übersichtlichkeit benennen wir die mit .x benannten Spalten um
#column_names <- c( "X","IDNummer", "Produktionsdatum", "Herstellernummer","Werksnummer", "Fehlerhaft", "Wareneingang")
column_names <- c( "X","IDNummer", "Produktionsdatum", "Wareneingang")
names(logistikverzug_selected_df) <- column_names

# Mithilfe der as.Date Funktion wandeln wir die Angaben zum Produktionsdatum und dem Wareneingang in Datumsformate um.
# Dies ermöglicht uns später eine einfache Berechnung des Logistikverzuges
logistikverzug_selected_df$Produktionsdatum <- as.Date(logistikverzug_selected_df$Produktionsdatum)
logistikverzug_selected_df$Wareneingang <- as.Date(logistikverzug_selected_df$Wareneingang, format = "%d.%m.%Y")


# erstellen einer neuen Spalte mit der Lieferzeit in Tagen
logistikverzug_mutated_df <- mutate(logistikverzug_selected_df, Produktionsdatum = as.Date(Produktionsdatum, origin = "%Y-%m-%d"), Lieferzeit = round(as.double(Wareneingang - Produktionsdatum),0))

# Erstellung eines Dataframes für den Plot im zweiten Reiter der App
df <- logistikverzug_mutated_df

# Erstellung eines Dataframes mit den versandten Karosserien am jeweiligen Tag      
df_prodi <- df %>%
  group_by(Produktionsdatum) %>%
  summarise(no =n())

# Bestimmung aller Karosserien, die insgesamt bis zum jeweiligen Tag versand wurden
df_prodi2 <- calc_auslieferung(df_prodi)

# löschen der nicht benötigten Spalte und umbenennung
df_produziert <-  df_prodi2 %>%
  select(1,3)

colnames(df_produziert) <- c('Datum', 'n_produziert')

# erstellung eines Dataframes mit den Eingegangenen Karosserien am jeweiligen Tag  
df_eingang <- df %>%
  group_by(Wareneingang) %>%
  summarise(no = n())

# Bestimmung aller Karosserien, die insgesamt bis zum jeweiligen Tag eingegangen sind
df_eingang2 <- calc_auslieferung(df_eingang)

# löschen der nicht benötigten Spalte und umbenennung
df_eingegangen <- df_eingang2 %>%
  select(1,3)

colnames(df_eingegangen) <- c('Datum', 'n_eingegangen')    

# Zusammenfügen der beiden Dataframes, sodass diese vollständig zusammengefügt werden (fulljoin)       
df_joined <- full_join(df_produziert, df_eingegangen, by = 'Datum')

# wenn es NA's gibt werden diese ersetzt
if (anyNA(df_joined$n_eingegangen)){
  # ersetzten der NA's durch Nullen in der Spalte n_eingegangen, denn die NA's treten dort auf, wo noch keine Karosserien
  # eingegangen sind
  df_joined$n_eingegangen[is.na(df_joined$n_eingegangen)] <- 0
}

# wenn es NA's gibt werden diese ersetzt
if (anyNA(df_joined$n_produziert)){
  # ersetzten der NA's in dr Spalte n_produziert mit dem Maximum von n_produziert, denn es werden keine weiteren Karosserien
  # mehr losgeschickt. Der maximale Wert befindet sich eine Reihe über dem ersten NA der Spalte n_produziert. 
  index_erstes_NA <- min(which(is.na(df_joined$n_produziert)))
  df_joined$n_produziert[is.na(df_joined$n_produziert)] <- df_joined$n_produziert[index_erstes_NA -1]
}

# erstellen einer Spalte mit den am jeweiligen Tag in Auslieferung befindlichen Karosserien
df_mutated <- mutate(df_joined, n_auslieferung = n_produziert - n_eingegangen) 

# Beginn der Shiny Applikation
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
        end = logistikverzug_mutated_df$Produktionsdatum[10000] # vorausgewähltes Enddatum 
      ),
      
      #Radiobuttons zur Auswahl des zu färbenden Monats
      fluidRow(
        column(
          width = 4,
          radioButtons("radio_month", label = h3("Monat"),
                   choices = list("Januar" = "01", "Februar" = "02", "März" = "03", "April" = "04", "Mai" = "05",
                                  "Juni" = "06", "Juli" = "07", "August" = "08", "September" = "09", "Oktober" = "10",
                                  "November" = "11", "Dezember" = "12")
          )
        ),
      
      #Radiobuttons zur Auswahl der Farbe
        column(
          width = 4,
          offset = 2,
          radioButtons("radio_color", label = h3("Farbe"),
                   choices = list("Grau" = "grey", "Weiß" = "white", "Schwarz" = "black", "Rot" = "red", "Orange" = "orange", "Gelb" = "yellow",
                                  "Grün" = "green", "Cyan" = "cyan", "Blau" = "blue", "Violett" = "purple", "Pink" = "pink", "Kahki" = "khaki")
          )
        )
      ),
      
      fluidRow(
      # Actionbutton zum Ausführen
        column(
          width = 2,
          actionButton("submit", label = "Färben")
        ), 
        
        column(
          width = 2,
          offset = 3,
          actionButton(
            inputId = "reset",
            label = "Zurücksetzen"
          )
        )
      )
    ),
    
    # Erstellung des Main-Panels mit zwei verschiedenen Reitern
    mainPanel(
      tabsetPanel(
        # Erstellung des ersten Reiters
        tabPanel(
          title = "Produktionsmenge",
          plotOutput(
            # An dieser Stelle wird output$plot dargestellt
            outputId = "plot_produktionsmenge"
          )
        ),
        # Erstellung des zweiten Reiters
        tabPanel(
          title = "Karosserien in Auslieferung",
          plotOutput( 
            outputId  = "plot_auslieferung"
          )
        )
      ),
      
      # Copyright
      fluidRow(
        HTML('<div style ="padding-left: 14px;" align = "left"><b>IDA Casestudy 2018 <span style="color:#428bca">&copy; by Gruppe 6:</span></b><br />Jannis Brodmann, Timos Ioannou, Aron Rogmann, Lukas Wolff &amp; Bobby Xiong</div>')
      ) 
    )
  )
)

server <- function(session, input, output) {
  
  
  # Erstellen des Zeitraums für den ersten Reiter anhand des Inputs
  zeitraum <-  reactive({
    
    # Ladebalken für die Aktualisierung des ausgewählten Zeitraums
    withProgress(
      message = 'Bitte warten...',
      detail = 'Der ausgewählte Zeitraum wird geladen.', value = 0, {
        for (i in 1:10) {
          incProgress(1/10)
          Sys.sleep(0.15)
        }
      }
    )
    
    startdatum <- as.Date(input$date_range[1])
    enddatum <- as.Date(input$date_range[2])
    
    #Der zeitraum liegt zwischem den inputs, filtern anhand den informationen
    filter(logistikverzug_mutated_df, logistikverzug_mutated_df$Produktionsdatum <= enddatum & logistikverzug_mutated_df$Produktionsdatum >= startdatum)
    
    
  })
  
  #Für zweiten Reiter, filtert den gewünschten Zeitraum raus
  auslieferung <- reactive({
    
    startdatum <- as.Date(input$date_range[1])
    enddatum <- as.Date(input$date_range[2])
   
    # filterung des Dataframes nach dem gegebenen Zeitraum
    df_ready <-  filter(df_mutated, df_mutated$Datum <= enddatum & df_mutated$Datum >= startdatum)

    return(df_ready)
  })
  
  farbe <- reactiveValues(data = c("01" = "grey", "02" = "grey", "03" = "grey", "04" = "grey", "05" = "grey", "06" = "grey",
                                   "07" = "grey", "08" = "grey", "09" = "grey", "10" = "grey", "11" = "grey", "12" = "grey") )
  
  # Ändert die Farbe im Farbvektor anhand der Inputs, aber nur wenn der Actionbutton betätigt wird
  # Änderung bleibt permanent
  
  observeEvent(input$submit,{
    
    # Ladebalken für Aktualisierung der Färbung
    withProgress(
      message = 'Bitte warten...',
      detail = 'Die Färbung wird angewandt.', value = 0, {
        for (i in 1:10) {
          incProgress(1/10)
          Sys.sleep(0.15)
        }
      }
    )
    
    farbe$data[[input$radio_month]] <<- input$radio_color
  })
  
  observeEvent(input$reset, {
    
    # Ladebalken für die Zurücksetzung der Farben
    withProgress(
      message = 'Bitte warten...',
      detail = 'Färbungen werden zurückgesetzt.', value = 0, {
        for (i in 1:10) {
          incProgress(1/10)
          Sys.sleep(0.15)
        }
      }
    )
    
    farbe$data <- c("01" = "grey", "02" = "grey", "03" = "grey", "04" = "grey", "05" = "grey", "06" = "grey",
                    "07" = "grey", "08" = "grey", "09" = "grey", "10" = "grey", "11" = "grey", "12" = "grey")
    
    updateRadioButtons(session,
                       inputId = "radio_month", selected = "01")
    updateRadioButtons(session,
                       inputId = "radio_color", selected = "grey")
    
    })
  
   # Funktion zur Erzeugung des Diagramms im ersten Reiter
  output$plot_produktionsmenge <- renderPlot({
    
    # abfangen einer falschen Eingabe des Zeitintervalls
    validate(
      need(input$date_range[1] <= input$date_range[2], "Bitte auf Datum achten: Startdatum muss vor Enddatum liegen")
    )
    
    # Erstellung des benötigten Dataframes mit den nach dem gewählten Zeitintervall gefilterten Daten
    df <- zeitraum()
    
    # Barplot
    ggplot(df, aes(x = factor(format(Produktionsdatum, "%Y %m"), ordered = TRUE), fill = (format(Produktionsdatum, "%m"))))+
      geom_bar()+
      scale_fill_manual("Monat",values = farbe$data,
                        breaks = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"),
                        labels = c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember"))+
      xlab("Produktionsdatum")+
      theme(axis.text.x = element_text(size = 10, color = "black", angle = 75))+
      ylab("Anzahl")
  })
  
  output$plot_auslieferung <- renderPlot({
    
    # abfangen einer falschen Eingabe des Zeitintervalls
    validate(
      need(input$date_range[1] <= input$date_range[2], "Bitte auf Datum achten: Startdatum muss vor Enddatum liegen")
    )
    
    # Erstellung des benötigten Dataframes mit der Anzahl der Karosserien in Auslieferung für jeden Tag im gewählten
    # Zeitintervall
    df <- auslieferung()
    
    # Berechnung des Mittelwertes der in Auslieferung befindlichen Karosserien
    m <- round(mean(df$n_auslieferung),2)
    
    # Barplot der Karosserien in Auslieferung über das jeweilige Datum mit geom_col() und einzeichnen des Mittelwertes m
    # geom_hline()
    ggplot(df, aes(Datum, n_auslieferung, fill =(format(Datum, "%m"))))+
      geom_col()+
      scale_fill_manual("Monat",values = farbe$data,
                        breaks = c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"), 
                        labels = c("Januar", "Februar", "März", "April", "Mai", "Juni", "Juli", "August", "September", "Oktober", "November", "Dezember"))+
      geom_hline(aes(yintercept = m, linetype = "Mittelwert"))+
      scale_y_continuous(breaks = sort(c(round(seq(min(df$n_auslieferung), max(df$n_auslieferung), length.out = 5), -1), m)))+
      xlab("Produktionstag")+
      theme(axis.text.x = element_text(size = 10, color = "black", angle = 75))+
      ylab("Anzahl")
  })
  
}

shinyApp(ui = ui, server = server)

