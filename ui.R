library(shiny)

shinyUI(fluidPage(
  
  # Titre de l'application
  h1(strong("Support Vector Machine")),
  h2(em("Démonstrateur et Comparateur")),
  
  navbarPage("Préambule",
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
             )),
  
  
  navbarPage("Base de données",
             tabPanel("Présentation"),
             tabPanel("Traitement")),
  
  
  navbarPage("Support Vector Machine",
             tabPanel("Principe"),
             tabPanel("Démonstration",
                      plotOutput("m_svm"),
                      plotOutput("rocsvm")),
             tabPanel("Résultat d'analyse optimale")),
  
  
  navbarPage("Comparaison avec d'autres méthodes",
             
             tabPanel("Concurrents",
                      tabsetPanel(
                        tabPanel("Régression logistique",
                                 sidebarLayout(
                                   sidebarPanel(
                                   ),
                                   
                                   mainPanel(
                                     "La régression logitique permet de mesurer l’association entre la variable expliquée qualitative et les variables explicatives.",
                                     
                                     plotOutput("confusion_RL", height = 400, width = 500),
                                     plotOutput("rocrl", height = 400, width = 500)
                                     
                                   ))),
                        
                        tabPanel("KNN",
                        sidebarLayout(
                          sidebarPanel(
                            helpText("Choisir vos paramètres : par défaut, ils sont à l'optimal."),
                            sliderInput("k","Nombre de voisins les plus proches",min=0, max=20,value=17)
                          ),
                          mainPanel(
                            "La méthode KNN (K-Nearest Neighbors) consiste à prédire la classe d’une observation dans l’échantillon test en identifiant les K observations qui lui sont le plus proche.",
                            
                            textOutput("txtknn"),
                            plotOutput("confusion_knn", height = 400, width = 500),
                            plotOutput("rocknn", height = 400, width = 500)
                          ))),
                        tabPanel("Random Forest",
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
                                     plotOutput("confusion_rf", height = 400, width = 500),
                                     textOutput("erreur_rf"),
                                     plotOutput("roc_rf", height = 400, width = 500) 
                                   ))),
                        tabPanel("Gradient Boosting",
                        
                                  sidebarLayout(
                                    sidebarPanel(),
                                    mainPanel(
                                      plotOutput("m_gb", height = 400, width = 500),
                                      plotOutput("roc_gb", height = 400, width = 500)
                                    )
                                  )
                  
                                  ))),
             
             tabPanel("Comparaison",
                      tabsetPanel(
                        tabPanel("Comparaison réactive"),
                        tabPanel("Comparaison optimale"),
                        tabPanel("Conclusion"))))
  
  
))
