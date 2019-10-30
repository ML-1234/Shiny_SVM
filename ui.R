
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
                            plotOutput("m_svm")),
                   tabPanel("Résultat d'analyse optimale")),
    
    
        navbarPage("Comparaison avec d'autres méthodes",
    
                   tabPanel("Concurrents",
                       tabsetPanel(
                           tabPanel("Régression logistique"),
                           tabPanel("KNN"),
                           tabPanel("Random Forest",
                                    sidebarLayout(
                                        sidebarPanel(
                                            helpText("Choisir vos paramètres : par défaut, ils sont à l'optimal."),
                                            sliderInput("mtry","Nombre de feuilles finales",min=0, max=30,value=15), #Remplacer ensuite value par les valeurs optimales du code
                                            sliderInput("ntree","Nombre d'arbres dans la forêt",min=0, max=500,value=150)
                                            ),
                                        mainPanel(
                                        "La forêt aléatoire (ou Random Forest) est constituée d'un grand nombre d'arbres de décision individuels qui fonctionnent comme un ensemble. 
                                         Le but est d'avoir un ensemble d'arbres qui soient moins corrélés car ils sont construits sur des échantillons différents.
                                         Chaque arbre individuel de la forêt prévoit le non-défaut ou le défaut (0 ou 1) et la classe majoritaire devient la prédiction de notre modèle.",
                                        "Nous vous présentons ici le modèle optimal obtenu par le Random Forest. Utilisez le curseur de défilement pour modifier les paramètres.",
                                        
                                        textOutput("selected_mtry"),
                                        tableOutput("confusion_rf")
                                    ))),
                           tabPanel("Gradient Boosting"))),
                   
                   tabPanel("Comparaison",
                        tabsetPanel(
                           tabPanel("Comparaison réactive"),
                           tabPanel("Comparaison optimale"),
                           tabPanel("Conclusion"))))

    
))
