library(shiny)
library(shinythemes)

shinyUI(fluidPage(theme = shinytheme("flatly"),
                  
                  # Titre de l'application
                  titlePanel(img(src="En-tête.png", width="100%")),
                  navbarPage("Sommaire",
                             
                             tabPanel("Préambule",
                                      tabsetPanel(
                                        
                                        tabPanel("Présentation de l'application",
                                                 div(htmlOutput("pre",align="justified",width=500,height = 400,style="margin:5%"))),
                                       
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
                                        tabPanel("Principe",
                                                 htmlOutput("intro",align="justified",width=500,height = 400,style="margin:5%"),
                                                 h4(em("CAS LINEAIREMENT SEPARABLE"),align="center"),
                                                 htmlOutput("intro2",align="justified",width=500,height = 400,style="margin:5%"),
                                                 
                                                 div(plotOutput("plot_linear", height = 300, width = 400), align="center"),
                                                 
                                                 htmlOutput("vs",align="justified",width=500,height = 400,style="margin:5%"),
                                                 div(plotOutput("plot_linear_SVM", height = 300, width = 400),align="center"),
                                                 
                                                 htmlOutput("cout",align="justified",width=500,height = 400,style="margin:5%"),
                                                 h4(em("CAS PRESQUE LINEAIREMENT SEPARABLE"),align="center"),
                                                 htmlOutput("cout2",align="justified",width=500,height = 400,style="margin:5%"),
                                                 div(plotOutput("plot_almostlinear_SVM", height = 300, width = 400), align="center"),
                                                 
                                                 htmlOutput("vr",align="justified",width=500,height = 400,style="margin:5%"),
                                                 h4(em("CAS NON LINEAIREMENT SEPARABLE"),align="center"),
                                                 htmlOutput("vr2",align="justified",width=500,height = 400,style="margin:5%"),
                                                 div(plotOutput("plot_radial_SVM", height = 300, width = 400), align="center"),
                                                 htmlOutput("fin",align="justified",width=500,height = 400,style="margin:5%")
                                        ),
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
                                                     
                                                     htmlOutput("optimal"),
                                                     htmlOutput("selected_param"),
                                                     div(plotOutput("confusion_rf", height = 400, width = 500), align="center"),
                                                     textOutput("erreur_rf")
                                                   )),
                                                 
                                                 h3("Gradient Boosting"),
                                                 sidebarLayout(
                                                   sidebarPanel(
                                                     sliderInput("max_prof","Profondeur maximale de l'arbre", min=1, max=20,value=5),
                                                     sliderInput("skrinkage", "Paramètre de lissage",min=0,max=1,value=0.5)
                                                   ),
                                                   mainPanel(
                                                     htmlOutput("optimal_gb"),
                                                     div(plotOutput("m_gb", height = 400, width = 500), align="center")
                                                   )
                                                 ),
                                                 
                                                 
                                                 
                                                 h1("Comparaison et conclusion"),
                                                 div(plotOutput("roc", height=500, width=600), align="center"),
                                                 div(tableOutput("ma_table"),align="center")
                                        ))
                             ))))
