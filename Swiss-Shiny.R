library(shiny)
library(ggplot2)
library(datasets)
library(dplyr)
library(apexcharter)

# Chargement des données "swiss"
data("swiss")

# Cette variable catégorise les régions en fonction du pourcentage de population catholique.
swiss <- swiss %>%
  mutate(
    Region = case_when(
      Catholic < 25 ~ "Faiblement catholique",
      Catholic >= 25 & Catholic < 75 ~ "Modérément catholique",
      Catholic >= 75 ~ "Fortement catholique"
    )
  )

# Serveur
server <- function(input, output) {
  # Calcul de la moyenne de la fertilité par catégorie
  output$fertilityByCategory <- renderUI({
    averages <- swiss %>%
      group_by(Region) %>%
      summarise(Moyenne_Fertility = round(mean(Fertility), 2))
    
    # boîtes pour chaque catégorie
    boxes <- lapply(1:nrow(averages), function(i) {
      div(
        class = "info-box",
        style = "background-color: #1abc9c; color: white; padding: 20px; border-radius: 8px; margin-bottom: 10px;",
        h4(paste("Catégorie :", averages$Region[i])),
        p(paste("Moyenne de Fertility :", averages$Moyenne_Fertility[i]))
      )
    })
    
    # Retourner toutes les boîtes
    do.call(tagList, boxes)
  })
  
  # Graphique
  output$plot <- renderPlot({
    g <- ggplot(swiss, aes(x = !!sym(input$regressor), y = !!sym(input$regressand))) +
      geom_point(aes(color = !!sym(input$color), size = !!sym(input$size))) +
      theme_minimal() +
      theme(
        panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "transparent", color = NA),
        plot.title = element_text(face = "bold", size = 16, hjust = 0.5, color = "#2c3e50"),
        axis.title = element_text(face = "bold", color = "#34495e"),
        axis.text = element_text(color = "#555555"),
        legend.background = element_rect(fill = "transparent", color = NA),
        legend.key = element_rect(fill = "transparent", color = "transparent")
      ) +
      labs(
        title = paste("Relation entre", input$regressor, "et", input$regressand),
        x = input$regressor,
        y = input$regressand
      )
    
    if (input$showRegression)
      g <- g + geom_smooth(method = "lm", se = TRUE, color = "#1abc9c", fill = "#a9dfbf")
    
    g
  })
  
  # Analyse exploratoire : résumé des données
  output$dataSummary <- renderPrint({
    summary(swiss)
  })
  
  # Corrélations entre les variables numériques uniquement
  output$correlationMatrix <- renderPrint({
    # Sélectionner uniquement les variables numériques
    numeric_data <- swiss %>% select_if(is.numeric)
    cor(numeric_data)
  })
  
  # Graphiques de distribution
  output$distPlot <- renderPlot({
    par(mfrow = c(2, 2))  # Affiche plusieurs graphiques sur une seule fenêtre
    plot(density(swiss$Fertility), main = "Distribution de la fertilité", xlab = "Fertility", 
         col = "#1abc9c", lwd = 2)
    rug(swiss$Fertility, col = "#34495e")
    hist(swiss$Fertility, freq = FALSE, add = TRUE, col = rgb(0.2, 0.7, 0.5, 0.5), border = NA)
    qqnorm(swiss$Fertility, main = "Normal Q-Q Plot (Fertility)", ylab = "Fertility", 
           col = "#34495e", pch = 16)
    qqline(swiss$Fertility, col = "#1abc9c", lwd = 2)
  })
  
  # Matrice de paires (seulement pour les variables numériques)
  output$pairsPlot <- renderPlot({
    numeric_data <- swiss %>% select_if(is.numeric)
    pairs(numeric_data, panel = panel.smooth, main = "Matrice de scatter-plots des données suisses",
          col = ifelse(swiss$Catholic > 50, "#e74c3c", "#3498db"), pch = 19)
  })
  # Graphique interactif pour les moyennes par catégorie
  output$categoryChart <- renderApexchart({
    averages <- swiss %>%
      group_by(Region) %>%
      summarise(Moyenne_Fertility = round(mean(Fertility), 2))
    
    # Apex chart
    apex(data = averages, aes(x = Region, y = Moyenne_Fertility)) %>%
      ax_chart(type = "bar") %>%
      ax_title(text = "Moyenne de Fertility par catégorie de région") %>%
      ax_xaxis(title = list(text = "Catégorie")) %>%
      ax_yaxis(title = list(text = "Moyenne de Fertility")) %>%
      ax_colors(c("#3498db", "#e74c3c", "#2ecc71")) %>%
      ax_tooltip(enabled = TRUE) %>%
      ax_dataLabels(enabled = TRUE)
  })
}
# Interface utilisateur
ui <- shinyUI(navbarPage(
  "Analyse des données Swiss",
  
  # Drapeau flottant
  tags$head(
    tags$style(HTML("
      #swiss-flag {
        position: fixed;
        top: 10px;
        right: 10px;
        z-index: 1000;
        width: 2px;
      }
      body {
        width: 100%;
        height: 100%;
        --color: #E1E1E1;
        background-color: #F3F3F3;
        background-image: linear-gradient(0deg, transparent 24%, var(--color) 25%, var(--color) 26%, transparent 27%,transparent 74%, var(--color) 75%, var(--color) 76%, transparent 77%,transparent),
            linear-gradient(90deg, transparent 24%, var(--color) 25%, var(--color) 26%, transparent 27%,transparent 74%, var(--color) 75%, var(--color) 76%, transparent 77%,transparent);
        background-size: 55px 55px;
      .navbar {
        background-color: #2c3e50 !important;
        color: white !important;
        border-bottom: 3px solid #1abc9c;
        box-shadow: 0 4px 6px rgba(0, 0, 0, 0.1);
        transition: background-color 0.3s ease, box-shadow 0.3s ease;
      }
      
      .navbar:hover {
        background-color: #34495e !important;
        box-shadow: 0 6px 10px rgba(0, 0, 0, 0.15);
      }
      
      .navbar .navbar-brand, 
      .navbar .navbar-nav > li > a {
        color: white !important;
        font-size: 1.1rem;
        transition: color 0.3s ease;
      }
      
      .navbar .navbar-brand:hover, 
      .navbar .navbar-nav > li > a:hover {
        color: #1abc9c !important;
        text-decoration: underline;
      }

      .btn {
        border-radius: 25px !important;
        background: linear-gradient(90deg, #1abc9c, #16a085) !important;
        color: white !important;
        font-weight: bold;
        padding: 10px 20px;
        transition: background 0.3s ease, transform 0.2s ease;
      }
      
      .btn:hover {
        background: linear-gradient(90deg, #16a085, #1abc9c) !important;
        transform: scale(1.05);
      }


    h3 {
      font-family: 'Roboto', sans-serif;
      color: black;
      text-transform: uppercase;
      font-weight: bold;
      letter-spacing: 1px;
      border-bottom: 2px solid #2ecc71;
      padding-bottom: 5px;
      margin-bottom: 15px;
    }
    .h3-alt {
      font-family: 'Roboto', sans-serif;
      color: #2c3e50; 
      font-weight: 500; 
      font-style: normal; 
      letter-spacing: 0.5px; 
      background: linear-gradient(135deg, #ecf0f1, #bdc3c7); 
      padding: 15px 20px; 
      border-radius: 15px; 
      margin-bottom: 20px;
      box-shadow: 0 6px 12px rgba(0, 0, 0, 0.15); 
      transition: transform 0.3s ease, box-shadow 0.3s ease; 
    }
    
    /* Ajout d'un effet de survol */
    .h3-alt:hover {
      transform: translateY(-3px); 
      box-shadow: 0 8px 16px rgba(0, 0, 0, 0.2); 
      background: linear-gradient(135deg, #bdc3c7, #ecf0f1); 



    
    "))
  ),
  
  # Ajout du drapeau en haut à droite
  tags$div(
    id = "swiss-flag",
    tags$img(src = "", alt = "")
  ),
  tabPanel("Présentation",
           sidebarLayout(
             sidebarPanel(
               h3("Bienvenue dans l'application Swiss"),
               p("Cette application explore les données socio-économiques et de fertilité en Suisse en 1888. \n L'objectif principal est d'offrir une vue d'ensemble des statistiques clés et d'analyser les relations entre les variables socio-économiques et démographiques."),
               p("Cette analyse s'appuie sur des données historiques et vous permet de :"),
               tags$ul(
                 tags$li("Analyser les moyennes des variables par catégories de régions géographiques."),
                 tags$li("Explorer les corrélations entre les variables telles que la fertilité, l'agriculture, l'éducation, et la religion."),
                 tags$li("Consulter des résumés statistiques, y compris des mesures comme la moyenne, la médiane, et les écart-types."),
                 tags$li("Visualiser des graphiques interactifs pour une exploration détaillée des tendances.")
               ),
               p("Que vous soyez intéressé par des analyses statistiques profondes ou simplement curieux des conditions socio-économiques historiques en Suisse, cette application vous fournira les outils nécessaires pour effectuer une exploration complète des données.")
             ),
             mainPanel(
               h3("Explorez les données historiques de la Suisse", class = "h3-alt"),
               tags$div(
                 style = "text-align: center; margin-top: 50px; padding: 30px; background-color: #2c3e50; border-radius: 10px; box-shadow: 0 -4px 8px rgba(0, 0, 0, 0.15);",
                 tags$img(src = "https://media4.giphy.com/media/v1.Y2lkPTc5MGI3NjExZGh3aXhrMTQybnkzcGFjNjM0enliZ2I5dnc4MjI4cXJrZ3B4NDJuMSZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/YSeD68nP2niYSAqSxM/giphy.gif", 
                          style = "width: 100%; height: auto; border-radius: 10px;", 
                          alt = "GIF de la Suisse")
               )
             )
           )
  ),
  
  tabPanel("Graphique interactif",
           sidebarLayout(
             sidebarPanel(
               h3("Paramètres du graphique" ),
               varSelectInput("regressand", "Variable dépendante :", swiss),
               varSelectInput("regressor", "Variable indépendante :", swiss, names(swiss)[2]),
               varSelectInput("color", "Variable pour la couleur :", swiss, names(swiss)[3]),
               varSelectInput("size", "Variable pour la taille :", swiss, names(swiss)[4]),
               checkboxInput("showRegression", "Afficher la ligne de régression", value = TRUE)
             ),
             mainPanel(
               h3("Graphique"),
               plotOutput("plot"),
               tags$div(
                 class = "tooltip-container",
                 tags$span(class = "tooltip", "Utulise les variables à gauche de l'écran pour"),
                 tags$span(class = "text", "Information !")
               )             )
           )
  ),
  
  tabPanel("Analyse exploratoire",
           sidebarLayout(
             sidebarPanel(
               h3("À propos des données"),
               p("Ce jeu de données explore les indicateurs socio-économiques et de fertilité en Suisse en 1888."),
               p("Les variables comprennent :"),
               tags$ul(
                 tags$li("Fertility : Taux de fertilité."),
                 tags$li("Agriculture : Pourcentage de la population masculine travaillant dans l'agriculture."),
                 tags$li("Examination : Proportion ayant réussi un examen militaire."),
                 tags$li("Education : Pourcentage ayant plus de six ans d'études."),
                 tags$li("Catholic : Pourcentage de population catholique."),
                 tags$li("Infant.Mortality : Mortalité infantile pour 1 000 naissances.")
               )
             ),
             mainPanel(
               h3("Résumé statistique"),
               verbatimTextOutput("dataSummary"),
               h3("Matrice de corrélation"),
               verbatimTextOutput("correlationMatrix"),
               h3("Visualisations"),
               plotOutput("distPlot"),
               plotOutput("pairsPlot")
             )
           )
  ),
  tabPanel("Analyse par Catégorie",
           sidebarLayout(
             sidebarPanel(
               h3("Résumé par catégories"),
               p("Cette page affiche les moyennes des taux de fertilité par catégorie basée sur le pourcentage de population catholique."),
               p("Les catégories incluent :"),
               tags$ul(
                 tags$li("Faiblement catholique : moins de 25% de catholiques."),
                 tags$li("Modérément catholique : entre 25% et 75%."),
                 tags$li("Fortement catholique : plus de 75% de catholiques.")
               )
             ),
             mainPanel(
               h3("Moyennes par catégorie"),
               uiOutput("fertilityByCategory"),
               h3("Graphique interactif"),
               apexchartOutput("categoryChart")
             )
           )
  )))

# Exécution de l'application
shinyApp(ui = ui, server = server)




