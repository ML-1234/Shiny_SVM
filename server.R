library(shiny)
library(ROSE)
library(randomForest)
library(caret)


shinyServer(function(input, output) {
    output$report <- downloadHandler(
        filename = "Notice.html",
        content = function(file) {
            # Document temporaire crée par copie, modifié avec les nouvelles valeurs des paramètres et replacé
            tempReport <- file.path(tempdir(), "Notice.Rmd")
            file.copy("Notice.Rmd", tempReport, overwrite = TRUE)
            
            # Ici on met les paramètres à rendre réactifs dans le document 
            # params <- list(n = input$slider)
            
            rmarkdown::render(tempReport, output_file = file,
                              params = params,
                              envir = new.env(parent = globalenv())
            )
        }
    )
    
    
    
    #output$confusion_rf <- renderTable(taux_erreuropt=mean(rf.pred.opt!=test$Class))
    
    
    output$selected_mtry <- renderText({ 
       paste( "Vous avez choisi le nombre de feuilles égales à", input$mtry, "et un nombre d'arbres égal à", input$ntree,".")
    })
    
})
