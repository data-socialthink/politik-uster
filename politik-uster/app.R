library(shiny)
library(data.table)
library(igraph)
library(fmsb)
library(dplyr)

library(shinythemes)
library(tm)
library(wordcloud)
library(RColorBrewer)


## Daten-Import ======================

## Daten von Github
GR_Liste <- read.csv("https://raw.githubusercontent.com/data-socialthink/politik-uster/main/gr_liste.csv")
Geschafte_Liste <- read.csv("https://raw.githubusercontent.com/data-socialthink/politik-uster/main/geschafte.csv")
Stichwortliste <- read.csv("https://raw.githubusercontent.com/data-socialthink/politik-uster/main/stichwortliste.csv")

# Diverse Kalkulationen ================

# Auswahlliste GR Politikfeldanalyse
liste_gr_auswahl <- paste0(GR_Liste$Vorname," ",GR_Liste$Name," (",GR_Liste$Partei,")")
liste_gr_auswahl <- as.data.frame(cbind(liste_gr_auswahl,c(1:length(liste_gr_auswahl)),as.character(GR_Liste$aktiv)))
liste_gr_auswahl_sort <- dplyr::arrange(liste_gr_auswahl, liste_gr_auswahl)

liste_gr_auswahl_aktiv <- dplyr::filter(liste_gr_auswahl_sort, V3=="aktiv")
liste_gr_auswahl_ehemalig <- dplyr::filter(liste_gr_auswahl_sort, V3=="ehemalig")

gr_auswahl_aktiv <- as.list(as.vector(liste_gr_auswahl_aktiv$V2))
names(gr_auswahl_aktiv) <- as.character(liste_gr_auswahl_aktiv$liste_gr_auswahl)

gr_auswahl_ehemalig <- as.list(as.vector(liste_gr_auswahl_ehemalig$V2))
names(gr_auswahl_ehemalig) <- as.character(liste_gr_auswahl_ehemalig$liste_gr_auswahl)

gr_auswahl <- list(`aktive`=gr_auswahl_aktiv,
                   `ehemalige`=gr_auswahl_ehemalig
)



# UI ============
ui <- 
    
    # Application title
    navbarPage("Politikanalysen für Uster ZH",theme = shinytheme("journal"),
    
    # Sidebar with a slider input for number of bins 

               
                           
                           ### TAB: Übersicht ====   
                           #tabPanel("Übersicht",
                            #        fluidRow(
                            #            column(12,
                            #                   HTML("Hier kommen die Resultate")
                            #            )
                            #        ),
                            #        fluidRow(
                            #            column(8,wellPanel(
                            #                HTML("Hier kommt die Eingabe")
                            #            ))
                            #        )
                           #),
                           
                           ### TAB: Die einzelnen Gemeinderät:innen ====   
                           tabPanel("Gemeinderät:innen",
                                    fluidRow(
                                        column(3,HTML("<br>"),wellPanel(
                                            selectInput("politikfeld", h4("Gemeinderät:in auswählen"), 
                                                        choices = gr_auswahl),
                                            HTML("Du kannst den Namen im Feld löschen und mit der Tastatur nach Gemeinderät:innen suchen.")
                                        )),
                                        column(9,
                                               htmlOutput("gr_auswahl",container=tags$h2),
                                               fluidRow(
                                                   column(6,plotOutput("plot_politikfeld")),
                                                   column(6,plotOutput("plot_wordcloud"))
                                               )
                                        )
                                    ),
                                    fluidRow(
                                        column(3,
                                               HTML("<br>")
                                        ),
                                        column(9,
                                               HTML("<br>"),
                                               dataTableOutput('gr_gesch_table')
                                               
                                        )
                                    )
                           ),
                           ### TAB: Netzwerkanalye ====   
                           tabPanel("Netzwerkanalyse",
                                    fluidRow(
                                        column(3,HTML("<br>"),wellPanel(
                                            
                                            radioButtons("radio_sna_cluster", "Welche Methode zur Cluster-Bildung soll verwendet werden?",
                                                         choices = list("Louvain Community Detection" = 1, "Infomap" = 2,
                                                                        "Edge betweenness" = 3),selected = 1),
                                            HTML("<br>"),
                                            radioButtons("radio_sna_legislatur", "Zeitraum",
                                                         choices = list("Legislatur 2018-2022" = 1,
                                                                        "Legislatur 2014-2018" = 2,
                                                                        "Legislatur 2010-2014" = 3,
                                                                        "sonstiger Zeitraum:" = 4),selected = 1),
                                            
                                            dateRangeInput('dateRange',
                                                           label = NULL,
                                                           start = "2021-05-01" , end = "2022-05-01", format = "dd.mm.yyyy", language = "de", separator = "bis", weekstart = 1
                                            ),
                                            HTML("<br><b>Erklärung Analysemethode:</b><br>Es wird jeweils analysiert, welche Personen gemeinsam parlamentarische Instrumente einsetzen (z.B. eine Anfrage einreichen). Aus dieser Information kann ein gemeinsames politisches Interesse und Handeln abgeleitet werden, welches zur Netzwerkanalyse und Clusterbildung genutzt wird. Personen die ihre parlamentarischen Instrumente nur alleine nutzen, werden aus der Analyse ausgeschlossen.")
                                        )),
                                        column(9,
                                               plotOutput("plot_sna", width = "100%", height = "700px",)
                                        )
                                    )
                           ),
                           ### TAB: About ====   
                           tabPanel("About",
                                    fluidRow(
                                        column(9,HTML("<br>"),wellPanel(
                                               HTML("<b>About</b><br>Dieses kommunale Politikbarometer ist ein privates Projekt von <a href=\"https://socialthink.ch/\" target=_blank>Andreas Wyss</a>, es handelt sich um technische und statistische Spielereien anhand der auf der Webseite uster.ch verfügbaren Daten zu den Gemeinderatsgeschäften.
                    <br><br><b>Lizenzen</b><br>Die Daten stehen unter Public Domain (Verzeichnis Gemeinderatsgeschäfte) resp. CC BY 4.0 (Stichwortliste und Verzeichnis Gemeinderät:innen). Der Code für die Auswertung wird unter GPLv3 zur Verfügung gestellt. Details sowie die Datensätze und Quellcode findest du unter <a href=\"https://github.com/data-socialthink/politik-uster\" target=_blank>github.com/data-socialthink/politik-uster</a>.")
                                        ))

                                    )
                           )
                           
               )



# SERVER ==========
server <- function(input, output) {
    
    
    ## Politikfeld ==============       
    # Funktion laden
    `%likeic%` <- function (x, pattern) { 
        grepl(pattern, x, ignore.case=TRUE)
    }
    
    # Daten laden
    GR_Liste <- GR_Liste
    Geschafte_Liste <- Geschafte_Liste
    Geschafte_alle <- data.table(Geschafte_Liste[,4])
    Stichwortliste <- as.data.table(Stichwortliste)
    
    # Variablen anlegen
    anzahl_gr <- dim(GR_Liste)[1]
    anzahl_dimensionen <- dim(table(Stichwortliste$Politikfeld))
    
    data_dimensionen <- as.data.frame(matrix(0,anzahl_gr,anzahl_dimensionen))
    colnames(data_dimensionen) <- names(table(Stichwortliste$Politikfeld))
    rownames(data_dimensionen) <- paste0(GR_Liste$Vorname," ",GR_Liste$Name)
    
    suche <- matrix(0,anzahl_dimensionen,2)
    for(i in 1:anzahl_dimensionen){
        dimensions_name <- names(table(Stichwortliste$Politikfeld)[i])
        suche[i,1] <- dimensions_name
        resultat <- "xyzwirdnichtberücksichtigt"
        for(ii in 1:dim(Stichwortliste[Politikfeld %like% dimensions_name])[1]){
            resultat <- paste0(resultat,"|",as.character(Stichwortliste[Politikfeld %like% dimensions_name][ii,1]$Stichwort))
        }
        suche[i,2] <- resultat
    }
    
    # Loop
    for (i in 1:anzahl_gr){
        geschafte_loop <- Geschafte_alle[V1 %like% rownames(data_dimensionen)[i]]
        
        for(ii in 1:anzahl_dimensionen){
            dimensions_name <- suche[ii,1]
            dimensions_begriffe <- suche[ii,2]
            data_dimensionen[i,ii] <- dim(geschafte_loop[V1 %likeic% dimensions_begriffe])[1]
        }
    }
    
    # Plot
    
    output$gr_auswahl <- renderText({ 
        paste(liste_gr_auswahl[input$politikfeld,1])
    })
    
    output$plot_politikfeld <- renderPlot({
        par(mar=c(0,0,0,0))
        dfplot <- data_dimensionen[as.numeric(input$politikfeld),]
        dfplot <- cbind(dfplot[12],dfplot[10],dfplot[9],dfplot[1],dfplot[3],dfplot[6],dfplot[5],dfplot[7],dfplot[11],dfplot[8],dfplot[4],dfplot[2])
        radarchart(rbind(max(dfplot),0,dfplot),pfcol="red")
    })
    
    output$plot_wordcloud <- renderCachedPlot({
        gr_nummer_wordcloud <- as.numeric(input$politikfeld)
        gr_geschaft_wordcloud <- as.data.table(Geschafte_alle)[V1 %like% paste0(GR_Liste$Vorname," ",GR_Liste$Name)[gr_nummer_wordcloud]]
        
        filterbegriffe <- paste0(GR_Liste$Vorname," ",GR_Liste$Name)
        filterbegriffe <- c(filterbegriffe,GR_Liste[,4])
        filterbegriffe <- c(filterbegriffe,"antrag","betreffend","stadtrat","stadtrates","uster","anfrage","ratsmitglied","antwort","postulat","genehmigung","primarschulpflege","leistungsmotion","sekundarschulpflege","werner","weisung","motion","interpellation","ratsmitglieder","ratsmitgliedes","stadt","kessler","grüne","verordnung","geschäftsbericht","grünliberale","bdp","jfu","evp","svp","fdp","sp","grüne","partei")
        
        #Textanalyse
        doc <- VCorpus(VectorSource(gr_geschaft_wordcloud))
        toSpace <- content_transformer(function (x , pattern ) gsub(pattern, " ", x))
        doc <- tm_map(doc, toSpace, "/")
        doc <- tm_map(doc, toSpace, "\\|")
        
        doc <- tm_map(doc, removeWords, filterbegriffe)
        doc <- tm_map(doc, content_transformer(tolower))
        doc <- tm_map(doc, removeNumbers)
        doc <- tm_map(doc, removeWords, stopwords("german"))
        doc <- tm_map(doc, removeWords, filterbegriffe) 
        doc <- tm_map(doc, toSpace, "&amp")
        doc <- tm_map(doc, removePunctuation)
        doc <- tm_map(doc, toSpace, ":")
        doc <- tm_map(doc, toSpace, ",")
        doc <- tm_map(doc, stripWhitespace)
        #doc <- tm_map(doc, stemDocument)
        tdm <- TermDocumentMatrix(doc)
        m <- as.matrix(tdm)
        v <- sort(rowSums(m),decreasing=TRUE)
        df <- data.frame(word = names(v),freq=v)
        
        #Wörterwolke
        
        if (dim(df)[1]>0) {
            set.seed(1001)
            par(mar=c(0,0,0,0))
            wordcloud(words = df$word, freq = df$freq, min.freq = 1, scale=c(2,0.5),
                      max.words=80, random.order=FALSE, rot.per=0.35, 
                      colors=brewer.pal(8, "Dark2"))
        } else {
            set.seed(1001)
            par(mar=c(0,0,0,0))
            wordcloud(words = c("bisher","keine","Vorstösse"), freq = c(1,1,1), min.freq = 1, scale=c(4,0.4),
                      max.words=80, random.order=FALSE, rot.per=0.35, 
                      colors=brewer.pal(8, "Dark2"))
        }
        
    }, cacheKeyExpr = { list(input$politikfeld)}, sizePolicy = sizeGrowthRatio(width = 800, height = 800, growthRate = 1), res = 180,)
    
    output$gr_gesch_table <- renderDataTable({
        gr_nummer <- as.numeric(input$politikfeld)
        Geschafte_Liste_GR_anzeige <- arrange(Geschafte_Liste, desc(Date))
        Geschafte_Liste_GR_anzeige <- cbind(Geschafte_Liste_GR_anzeige[,1:4],paste0('<a href="',Geschafte_Liste_GR_anzeige[,5],'" target="_blank" class="btn btn-danger">zum Geschäft</a>'))
        colnames(Geschafte_Liste_GR_anzeige) <- c("Nummer","Datum","Art","Bezeichnung","")
        gr_geschaft_anzeige <- as.data.table(Geschafte_Liste_GR_anzeige)[Bezeichnung %like% paste0(GR_Liste$Vorname," ",GR_Liste$Name)[gr_nummer]]
        gr_geschaft_anzeige[,1:5]
    }, escape = FALSE,options=list(iDisplayLength=5,                    # initial number of records
                                   bLengthChange=0,                       # show/hide records per page dropdown
                                   bFilter=1,                                    # global search box on/off
                                   bInfo=0# information on/off (how many records filtered, etc)
    ))
    
    
    
    ## SNA ==============
    
    # Funktionen laden
    
    delete.isolates <- function(graph, mode = 'all') {
        isolates <- which(degree(graph, mode = mode) == 0)
        delete.vertices(graph, isolates)
    }
    
    
    # Daten laden
    GR_Liste$von <- as.Date(GR_Liste$von)
    GR_Liste$bis <- as.Date(GR_Liste$bis)
    
    
    output$plot_sna <- renderCachedPlot({

        #GR_Liste_aktiv <- GR_Liste %>% dplyr::filter(aktiv=="aktiv")
        #GR_Liste_aktiv <- GR_Liste %>% dplyr::filter(bis>="2018-05-2018") %>% dplyr::filter(von<="2022-05-07")
        GR_Liste_aktiv <- GR_Liste
        
        Geschafte_Liste$Date <- as.Date(Geschafte_Liste$Date)
        
        if (input$radio_sna_legislatur == 1) {
            datum_von <- as.Date("2018-05-07")
            datum_bis <- as.Date("2022-05-08")
        } else if (input$radio_sna_legislatur == 2) {
            datum_von <- as.Date("2014-04-14")
            datum_bis <- as.Date("2018-05-06")
        } else if (input$radio_sna_legislatur == 3){
            datum_von <- as.Date("2010-05-10")
            datum_bis <- as.Date("2014-04-13")
        } else if (input$radio_sna_legislatur == 4){
            datum_von <- input$dateRange[1]
            datum_bis <- input$dateRange[2]
        }
        
        Geschafte_Liste_filter <- Geschafte_Liste %>% dplyr::filter(Date >= datum_von) %>% dplyr::filter(Date <= datum_bis)
        
        Geschafte_alle <- data.table(Geschafte_Liste_filter[,4])
        
        
        
        # Variablen anlegen ====
        netzwerk_dimension <- dim(GR_Liste_aktiv)[1]
        netzwerk_matrix <- matrix(0,netzwerk_dimension,netzwerk_dimension)
        colnames(netzwerk_matrix) <- paste0(GR_Liste_aktiv$Vorname,"\n",GR_Liste_aktiv$Name)
        rownames(netzwerk_matrix) <- paste0(GR_Liste_aktiv$Vorname,"\n",GR_Liste_aktiv$Name)
        
        # Loop / Auswertung ======
        for (i in 1:netzwerk_dimension) {
            geschafte_loop <- Geschafte_alle[V1 %like% paste0(GR_Liste_aktiv$Vorname[i]," ",GR_Liste_aktiv$Name[i])]
            
            for (ii in 1:netzwerk_dimension) {
                netzwerk_matrix[i,ii] <- dim(geschafte_loop[V1 %like% paste0(GR_Liste_aktiv$Vorname[ii]," ",GR_Liste_aktiv$Name[ii])])[1]
            }
            
        }
        
        # igraph  ======
        network <- graph_from_adjacency_matrix(netzwerk_matrix , mode='upper', weighted = T, diag=F)
        
        
        # Plot
        
        
        
        if (input$radio_sna_cluster == 1)  {
            network_plot <- delete.isolates(network)
            cluster_louvain(network_plot) -> network_cluster
            set.seed(20)
            par(mar=c(0,0,0,0))
            print(plot(network_cluster, network_plot, vertex.label.cex=0.6))}
        if (input$radio_sna_cluster == 2)  {
            network_plot <- delete.isolates(network)
            cluster_infomap(network_plot) -> network_cluster
            set.seed(20)
            par(mar=c(0,0,0,0))
            print(plot(network_cluster, network_plot, vertex.label.cex=0.6))}
        if (input$radio_sna_cluster == 3)  {
            network_plot <- delete.isolates(network)
            cluster_edge_betweenness(network_plot) -> network_cluster
            set.seed(20)
            par(mar=c(0,0,0,0))
            print(plot(network_cluster, network_plot, vertex.label.cex=0.6))}
        
    }, cacheKeyExpr = { list(input$radio_sna_cluster, input$dateRange, input$radio_sna_legislatur, GR_Liste, Geschafte_Liste)}, sizePolicy = sizeGrowthRatio(width = 1600, height = 1400, growthRate = 1), res = 180,)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
