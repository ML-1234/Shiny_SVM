library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("flatly"),
  
  # Titre de l'application
  h1(strong("Support Vector Machine")),
  h2(em("Démonstrateur et Comparateur")),
  navbarPage("Sommaire",
  
  tabPanel("Préambule",
           tabsetPanel(
             tabPanel("Présentation de l'application"),
             tabPanel("Notice d'utilisation à télécharger",
                      sidebarLayout(
                        sidebarPanel(
                          #Ici vont les inputs réactifs du document rmarkdown
                          #sliderInput("slider", "Slider", 1, 100, 50),
                          downloadButton("Notice",
                                         "Télécharger la notice")),
                        
                        mainPanel("Notice d'utilisation",
                                  includeMarkdown("Notice.Rmd")) )
             ))),
  
  
  tabPanel("Base de données",
           tabsetPanel(
             tabPanel("Présentation"),
             tabPanel("Traitement"))),
  
  
  tabPanel("Support Vector Machine",
           tabsetPanel(
             tabPanel("Principe"),
             tabPanel("Démonstration et Comparaison",
             h1("Application de la méthode des SVM"),
             sidebarLayout(
               sidebarPanel(),
               
               mainPanel(
                      div(plotOutput("m_svm", height = 400, width = 500), align="center")
                      )),

             
             h1("Méthodes concurrentes"),
             h3("Régression logistique"),
             sidebarLayout(
               sidebarPanel(),
               
               mainPanel(
                 "La régression logitique permet de mesurer l’association entre la variable expliquée qualitative et les variables explicatives.",
                 
                 div(plotOutput("confusion_RL", height = 400, width = 500), align="center")
                 
               )),
             
             h3("Random Forest"),
             sidebarLayout(
               sidebarPanel(
                 helpText("Choisissez vos paramètres :"),
                 sliderInput("mtry","Nombre de feuilles finales",min=0, max=30, value=15), 
                 sliderInput("ntree","Nombre d'arbres dans la forêt",min=0, max=500, value=250) 
               ),
               mainPanel(
                 "La forêt aléatoire (ou Random Forest) est constituée d'un grand nombre d'arbres de décision individuels qui sont construits sur des échantillons différents.
                                     Chaque arbre individuel de la forêt prévoit le non-défaut ou le défaut (0 ou 1) et la classe majoritaire devient la prédiction de notre modèle.",
                 
                 textOutput("optimal"),
                 textOutput("selected_param"),
                 div(plotOutput("confusion_rf", height = 400, width = 500), align="center"),
                 textOutput("erreur_rf")
               )),
             
             h3("Gradient Boosting"),
             sidebarLayout(
               sidebarPanel(),
               mainPanel(
                 div(plotOutput("m_gb", height = 400, width = 500), align="center")
               )
             ),
             
             
             h1("Comparaison et conclusion"),
             div(plotOutput("roc", height=500, width=600), align="center"),
             div(tableOutput("ma_table"),align="center")
        ))
))))