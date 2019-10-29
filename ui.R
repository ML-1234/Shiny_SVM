
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
                   tabPanel("Démonstration"),
                   tabPanel("Résultat d'analyse optimale")),
    
    
        navbarPage("Comparaison avec d'autres méthodes",
    
                   tabPanel("Concurrents",
                       tabsetPanel(
                           tabPanel("Régression logistique"),
                           tabPanel("KNN"),
                           tabPanel("Random Forest"),
                           tabPanel("Gradient Boosting"))),
                   
                   tabPanel("Comparaison",
                        tabsetPanel(
                           tabPanel("Comparaison réactive"),
                           tabPanel("Comparaison optimale"),
                           tabPanel("Conclusion"))))

    
))
