###################################################################
#Projet Tuteure

#Library
library("shiny")
###################################################################
library("readr")
library("dplyr")
library("purrr")
#Chargeons les library et packages qui peuvent etre utiles
#library("shinydashboard")

library("glue")
library("gt")
library("tidyverse")
#library("SnowballC")
library("wordcloud")
library("RColorBrewer")
library("magrittr")
library("lubridate")
library("chron")
library("sqldf")
library("shiny") #Permet de construire l'application web
library("shinydashboard") #Permet de construire une architecture dynamique de la page web
library("shinyWidgets") #Permet d'afficher un message d'alerte
library("DT") #Creer tableau colore
library("googleVis") #Realise aussi des graphiques
library("plotly")
library("rsconnect")
library("colourpicker")
library("miniUI")
library("shinyjs")
library("tidyverse")
library("ggplot2")
library("dplyr")
library("formattable")
library("htmltools")
library("htmlwidgets")
library("metricsgraphics")
library("RColorBrewer")
library("janitor")
library("shinycssloaders")

#Code de nettoyage

##########################################################################################

#1-IMPORTER LE FICHIER DANS UNE VARIABLE"
setwd("/home")
nettoyage<-read_csv('~/logs_StatsMEEF_20190326-1451.csv')
nettoyage <- janitor::clean_names(nettoyage)
#Visualisation du fichier afin de verifier que l'importation s'est bien deroulee


#extraire le numero d'identifiant 
nettoyage$user_ID <- str_extract(nettoyage$description, "\\d+")


#Renommer les variables
nettoyage <- rename(nettoyage, "Nom" = "nom_complet", Cible = "utilisateur_touche", 
                    "Context_Evenement" = "contexte_de_levenement", "Evenement_consulte" = "nom_de_levenement", 
                    "Description" = "description", "Adresse IP" = "adresse_ip" , "Origine" = "origine", Heure = "heure",
                    Composant = "composant"
)

#Supprimer des colonnes   
nettoyage <- select(nettoyage, -Description, -"Adresse IP", -Origine)

#Capitaliser les chaines de caracteres (Premiere lettre en Majuscule)
nettoyage$Nom <- str_to_title(nettoyage$Nom)
nettoyage$Cible <- str_to_title(nettoyage$Cible)


#Extraire des parties de la chaine de caractere 
nettoyage$Heure <- dmy_hm(nettoyage$Heure)

#Ordonner les variables de notre base de donnees
#Everything pour toutes les autres variables
nettoyage <-select(nettoyage, Heure, user_ID, Nom, Cible, Evenement_consulte, Composant, Context_Evenement, everything())

#Grouper la variable Context_Evenement afin de compter combien de fois elle apparait dans la base


#Supprimer les - par des NA dans la variables Cible
NA -> nettoyage$Cible[nettoyage$Cible == "-"]
##########################################################################################

bcl <- nettoyage


#########################################################################################
#Partie Utiliateur
#########################################################################################
ui <-  dashboardPage(
  dashboardHeader(title = "Shiny Master II IE", titleWidth = 450),
  #Menu de gauche
  dashboardSidebar(width = 330,
                   
                   #Le curseur de date de connexion
                   sliderInput("timeInput", "Periode", 
                               min = as.POSIXct("2018-01-01 01:00:00"), 
                               max= as.POSIXct(today()), 
                               value = c(as.POSIXct("2018-01-01 01:00:00"),
                                         as.POSIXct(today()))),
                   #Liste deroulante des Enseignant
                   uiOutput("EnseignantOutput"),
                   #Liste deroulante des cours
                   uiOutput("CoursOuput")
                   
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    #Creer des endroits pour afficher les graphiques et tableaux
    
    fluidRow(
      box(
        width = 6, status = "info", solidHeader = TRUE,
        title = "Etudiants connectes e ce cours",
        withSpinner( plotOutput("coolplot"), type = 1, color = "#0dc5c1", size = 1)
        
      ),
      
      
      box(
        width = 3, status = "info", solidHeader = TRUE,
        title = "Connexions", 
        withSpinner(tableOutput("resultat" ), type = 1, color = "#0dc5c1", size = 0.5, proxy.height = 121)),
      
      box(
        width = 3, status = "info", solidHeader = TRUE,
        title = "Etudiants", 
        withSpinner(tableOutput("etudiant"), type = 1, color = "#0dc5c1", size = 0.5, proxy.height = 115)),
      box(
        width =6, status = "info", solidHeader = TRUE,
        title = "Top 5 : Cours consultes",
        withSpinner(tableOutput("professeur"), type = 1, color = "#0dc5c1", size = 0.5, proxy.height = 232)),
      
      
      
      tableOutput("cible")
      
    )
    
  )
)



#########################################################################################
#Partie Server
#########################################################################################
server <- function(input, output){
  
  #Creation des sorties Enseignant
  output$EnseignantOutput <- renderUI({
    selectInput("EnseignantInput", "Liste Enseignant",
                sort(unique(bcl$Nom)),
                selected = "Emmanuel Peterle")
  })
  
  monfiltre <- reactive({
    if (is.null(input$EnseignantInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(Heure >= input$timeInput[1],
             Heure <= input$timeInput[2],
             Context_Evenement == input$CoursInput,
             Nom == input$EnseignantInput,
             Cible == input$cible
      )
  })
  
  #configurationde la selection des cours
  output$CoursOuput <- renderUI({
    selectInput("CoursInput", "Liste des Cours",
                sort(unique(bcl$Context_Evenement)),
                selected = "Cours: Statistiques")
  })
  
  monfiltre <- reactive({
    if (is.null(input$CoursInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(Heure >= input$timeInput[1],
             Heure <= input$timeInput[2],
             Context_Evenement == input$CoursInput,
             Nom == input$EnseignantInput
             
      )
  })
  
  
  #Plot de notre graphique avec renderplot
  output$coolplot <- renderPlot({
    if (is.null(monfiltre())) {
      return(NULL)
    }
    
    
    ggplot(monfiltre(), aes(Cible)) + 
      geom_bar(stat="count", fill = "#FF6666")+coord_flip() +
      scale_fill_manual("legend") + scale_fill_brewer(palette="Set1")
  })
  
  
  #Creation du rectangle box total connexion
  output$resultat <- renderValueBox({
    valueBox(
      
      value = formattable(monfiltre()%>% summarise(Evenement_consulte = n())
                          %>% rename( "Cours" = Evenement_consulte)),
      icon = icon("download"),
      subtitle = "Total connexion")
  })
  
  #Creation du rectangle box total des etudiants present dans notre baseS
  output$etudiant <- renderValueBox({
    valueBox(
      
      value = formattable(sqldf("SELECT  COUNT(DISTINCT Cible) FROM nettoyage")),
      icon = icon("book"),
      subtitle = "Total Etudaint")
  })
  
  
  
  
  monfiltre2 <- reactive({
    if (is.null(input$CoursInput)) {
      return(NULL)
    }    
    
    bcl %>%
      filter(Heure >= input$timeInput[1],
             Heure <= input$timeInput[2]
             
      )
  })
  #cette partie est en travaux : vous verrez une erreur apparaitre
  output$professeur <- renderTable({
    formattable( monfiltre2() %>% select(Context_Evenement) %>% count(Context_Evenement)  %>% top_n(5)
                 %>% rename(Connexions = n ))
    
  })
  
}

#########################################################################################
#Lancement de l'application
#########################################################################################
shinyApp(ui, server)

