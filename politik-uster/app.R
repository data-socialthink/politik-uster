library(shiny)
library(data.table)
library(igraph)
library(fmsb)
library(dplyr)



## Daten-Import ======================

## Daten von Github
GR_Liste <- read.csv("https://raw.githubusercontent.com/data-socialthink/politik-uster/main/gr_liste.csv")
Geschafte_Liste <- read.csv("https://raw.githubusercontent.com/data-socialthink/politik-uster/main/geschafte.csv")
Stichwortliste <- read.csv("https://raw.githubusercontent.com/data-socialthink/politik-uster/main/stichwortliste.csv")

# Diverse Kalkulationen ================

# Auswahlliste GR Politikfeldanalyse
liste_gr_auswahl <- paste0(GR_Liste$Vorname," ",GR_Liste$Name," (",GR_Liste$Partei,")")
liste_gr_auswahl <- as.data.frame(cbind(liste_gr_auswahl,1:81,GR_Liste$aktiv))
liste_gr_auswahl_sort <- sort(liste_gr_auswahl)
liste_gr_auswahl_aktiv <- filter(liste_gr_auswahl_sort, V3=="aktiv")
liste_gr_auswahl_ehemalig <- filter(liste_gr_auswahl_sort, V3=="ehemalig")

gr_auswahl_aktiv <- as.list(liste_gr_auswahl_aktiv$V2)
names(gr_auswahl_aktiv) <- liste_gr_auswahl_aktiv$liste_gr_auswahl

gr_auswahl_ehemalig <- as.list(liste_gr_auswahl_ehemalig$V2)
names(gr_auswahl_ehemalig) <- liste_gr_auswahl_ehemalig$liste_gr_auswahl

gr_auswahl <- list(`aktive`=gr_auswahl_aktiv,
                   `ehemalige`=gr_auswahl_ehemalig
)



# UI ============
ui <- fluidPage(
    
    # Application title
    titlePanel("Politikanalysen für Uster ZH"),
    
    # Sidebar with a slider input for number of bins 
    fluidRow(
        
        
        # Show a plot of the generated distribution
        column(9,
               
               tabsetPanel(type = "tabs",
                           
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
                                        column(4,HTML("<br>"),wellPanel(
                                            selectInput("politikfeld", h4("Gemeinderät:in auswählen"), 
                                                        choices = gr_auswahl),
                                            HTML("Du kannst den Namen im Feld löschen und mit der Tastatur nach Gemeinderät:innen suchen.")
                                        )),
                                        column(6,
                                               htmlOutput("gr_auswahl",container=tags$h2),
                                               plotOutput("plot_politikfeld")
                                        )
                                    ),
                                    fluidRow(
                                        column(4,
                                               HTML("<br>")
                                        ),
                                        column(8,
                                               HTML("<br>"),
                                               dataTableOutput('gr_gesch_table')
                                               
                                        )
                                    )
                           ),
                           ### TAB: Netzwerkanalye ====   
                           tabPanel("Netzwerkanalyse",
                                    fluidRow(
                                        column(4,HTML("<br>"),wellPanel(
                                            #radioButtons("radio_sna_gr", h4("Gemeinderät:innen"),
                                            #             choices = list("nur aktuelle Gemeinderät:innen" = 1, "alle Gemeinderät:innen" = 2),selected = 1),
                                            #radioButtons("radio_sna_legislatur", h4("Legislatur"),
                                            #             choices = list("ALLE" = 1, "2018-2022" = 2,
                                            #                            "2014-2018" = 3, "2010-2014" = 4),selected = 1),
                                            radioButtons("radio_sna_cluster", h4("Cluster-Analysen"),
                                                         choices = list("Louvain Community Detection" = 1, "Infomap" = 2,
                                                                        "Edge betweenness" = 3),selected = 1)
                                        )),
                                        column(8,
                                               plotOutput("plot_sna")
                                        )
                                    )
                           )
                           
               )
               
        ),
        column(3,
               HTML("<b>About</b><br>Dieses kommunale Politikbarometer ist ein privates Projekt von Andreas Wyss, es handelt sich um technische und statistische Spielereien anhand der auf der Webseite uster.ch verfügbaren Daten zu den Gemeinderatsgeschäften.
                    <br><br><b>Lizenzen</b>")
        )
    )
)

# SERVER ==========
server <- function(input, output) {
    
    
    ## Politikfeld ==============       
    data_dimensionen <- read.csv("https://raw.githubusercontent.com/data-socialthink/politik-uster/main/politikfeldanalyse.csv")
    
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
    GR_Liste <- GR_Liste
    GR_Liste_aktiv <- dplyr::filter(GR_Liste, aktiv=="aktiv")
    #GR_Liste_aktiv <- GR_Liste
    Geschafte_Liste <- Geschafte_Liste
    Geschafte_alle <- data.table(Geschafte_Liste[,4])
    
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
    
    output$plot_sna <- renderPlot({
        
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
        
    }, width = "auto", height = 700)
    
}

# Run the application 
shinyApp(ui = ui, server = server)
