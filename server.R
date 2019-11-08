library(shiny)
library(randomForest)
library(unbalanced)
library(e1071)
library(caret)
library(class)
library(pROC)
library(ggplot2)
library(corrplot)
library(ROCR)
library(gbm)

####Base de données####
bdd <- read.csv("creditcard.csv")

bdd$Class <- as.factor(bdd$Class)
form <- Class~.

idx <- sample(1:nrow(bdd), as.integer(0.75*nrow(bdd)))
train <- bdd[idx, ]
test <- bdd[-idx, ]

X <- train[,-ncol(train)]
Y <- train$Class
newData <- ubBalance(X, Y, type="ubUnder", positive=1, perc=0.3, method="percUnder")
train_ub <- as.data.frame(cbind(newData$X, newData$Y))
colnames(train_ub)[colnames(train_ub)=="newData$Y"] <- "Class"


####Fonctions#####
### Couleur des modèles ###
### Couleur des modèles ###
cols <- c('Support Vector Machine'= '#6A4A3C', 'Régression Logistique'= '#00A0B0', 'KNN' = '#CC333F', 'Random Forest'= '#EB6841', 'Gradient Boosting' = '#EDC951')

###Création Matrice de confusion en Plot###
draw_confusion_matrix <- function(cm, color) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('MATRICE DE CONFUSION', cex.main=2)
  
  # Création de la matrice
  rect(150, 430, 240, 370, col=color)
  text(195, 435, 'Pas de défaut', cex=1.2)
  rect(250, 430, 340, 370, col='white')
  text(295, 435, 'Défaut', cex=1.2)
  text(125, 370, 'Observation', cex=1.3, srt=90, font=2)
  text(245, 450, 'Prédiction', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='white')
  rect(250, 305, 340, 365, col=color)
  text(140, 400, 'Pas de défaut', cex=1.2, srt=90)
  text(140, 335, 'Défaut', cex=1.2, srt=90)
  
  # Ajout des informations de la matrice 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='black')
  text(295, 400, res[3], cex=1.6, font=2, col='black')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # Ajout des statistiques : sensitivité, spécificité, précision
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "STATISTIQUES", cex.main=1.8, xaxt='n', yaxt='n')
  text(10, 85, "Sensitivité", cex=1.2, font=2)
  text(10, 70, paste(round(as.numeric(cm$byClass[1])*100, 3), '%'), cex=1.2)
  text(50, 85, "Spécificité", cex=1.2, font=2)
  text(50, 70, paste(round(as.numeric(cm$byClass[2])*100, 3), '%'), cex=1.2)
  text(90, 85, "Précision", cex=1.2, font=2)
  text(90, 70, paste(round(as.numeric(cm$byClass[5])*100, 3), '%'), cex=1.2)
  
  
  # Ajout des taux d'exactitude et d'erreur
  text(30, 35, "Taux d'exactitude", cex=1.5, font=2)
  text(30, 20, paste(round(as.numeric(cm$overall[1])*100, 3), '%'), cex=1.4)
  text(70, 35, "Taux d'erreur", cex=1.5, font=2)
  text(70, 20, paste(round((1 - sum(diag(cm$table))/sum(cm$table))*100, 3),"%"), cex=1.4)
}

#Fonction taux d'erreur
erreur<-function(matrice){
  paste(round((matrice$table[[2]]+matrice$table[[3]])/sum(matrice$table)*100, 3),"%")

}


#Optimisation du Random Forest
TrainData <- train_ub[,-31] 
TrainClasses <- train_ub[,31] 
rf.fcttrain <- train(TrainData, TrainClasses, method = "rf", trControl = trainControl(method = "cv"))
mtry_opt <- as.integer(rf.fcttrain$bestTune)
taux_erreur_ntree <- vector()
ntr <- c(1,seq(10,500,by=10))
for(j in ntr){
  rf.datant <- randomForest(form, train_ub, mtry=mtry_opt, ntree=j)
  rf.datant.pred <- predict(rf.datant,newdata=test)
  txerreur <- mean(rf.datant.pred!=test$Class)
  taux_erreur_ntree <- rbind(taux_erreur_ntree,txerreur)
}
ntree_opt <- ntr[which.min(taux_erreur_ntree)]







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
  
  
  # SVM
  ##Modèle

  svm.fit <- reactive({svm(form, data=train_ub, type="C-classification", kernel ="linear", probability = TRUE)})
  
  svm.pred <- reactive({predict(svm.fit(), test, probability=TRUE)})
  
  cmsvm <- reactive({pred <- svm.pred()
                     confusionMatrix(test$Class, pred)})
  
  ##Matrice de confusion
  output$m_svm <- renderPlot({
    draw_confusion_matrix(cmsvm(), cols[1])
  })
  
  
  # Régression logistique
  ##Modèle
  glm.fit <- reactive({glm(Class~.,data=train_ub,family="binomial")})
  glm.prob <- reactive({predict(glm.fit(), test, type="response")})
  
  cmrl <- reactive({glm.pred <- factor(ifelse(glm.prob()>0.5, 1,0))
                    confusionMatrix(test$Class, glm.pred)})
  
  ##Matrice de confusion
  output$confusion_RL <- renderPlot({
    draw_confusion_matrix(cmrl(), cols[2])
  })
  
  
  
  # RandomForest
  ## Modèle et matrice simple
  rf.fit <- reactive({randomForest(form,train_ub, mtry=input$mtry,ntree=input$ntree)})
  rf.pred <- reactive({predict(rf.fit(),test,type="response")})
  cmrf <- reactive({confusionMatrix(test$Class, rf.pred())})
  
  output$selected_param <- renderText({ 
    paste( "Vous avez choisi le nombre de feuilles égales à", input$mtry, "et un nombre d'arbres égal à", input$ntree,".")
  })
  
  output$optimal <- renderText({ 
    paste( "Les paramètres optimaux qui permettent de minimiser le taux d'erreur sont de", mtry_opt, "pour le nombres de feuilles et",ntree_opt, "pour le nombres d'arbres dans la forêt.")
  })
  
  ##Matrice de confusion
  output$confusion_rf <- renderPlot({draw_confusion_matrix(cmrf(), cols[4])})
  
  output$erreur_rf <- renderText({
    taux_erreur <- paste(round((1 - sum(diag(cmrf()$table))/sum(cmrf()$table))*100, 3),"%")
    paste( "L'erreur est de", taux_erreur,"%.")
  })
  
  
  # Gradient Boosting
  ##Modèle
  boost.fit <- reactive({train_ub$Class <- ifelse(train_ub$Class==1, 1,0)
                     test$Class <- ifelse(test$Class==1, 1,0)
                     gbm(form, data=train_ub, distribution="bernoulli", n.trees=5000)})
  
  boost.pred <- reactive({predict(boost.fit(), newdata=test, type="response", n.trees=5000)})
  
  cmgb <- reactive({
    boost.pred.class <- factor(ifelse(boost.pred()>0.5, 1,0))
    test$Class <- as.factor(test$Class)
    train_ub$Class <- as.factor(train_ub$Class)
    confusionMatrix(test$Class, boost.pred.class)})
  
  ##Matrice de confusion
  output$m_gb <- renderPlot({draw_confusion_matrix(cmgb(), cols[5])})
  
  
  
  #Courbe ROC
  output$roc <-renderPlot({
    
    ##SVM
    svm.fit.prob <-attr(svm.pred(),"probabilities")
    ROCRpred_svm <- prediction(svm.fit.prob[,2], test$Class)
    perf_svm <- ROCR::performance(ROCRpred_svm, 'tpr','fpr')
    roc_svm.data <- data.frame(fpr=unlist(perf_svm@x.values),
                               tpr=unlist(perf_svm@y.values), model="Support Vector Machine")
    
    ##Régression logistique
    ROCRpred_glm <- prediction(glm.prob(), test$Class)
    perf_glm <- ROCR::performance(ROCRpred_glm, 'tpr','fpr')
    roc_glm.data <- data.frame(fpr=unlist(perf_glm@x.values),
                               tpr=unlist(perf_glm@y.values), model="Régression logistique")
    
    ##Gradient Boosting
    ROCRpred_gb <- prediction(boost.pred(), test$Class)
    perf_gb <- ROCR::performance(ROCRpred_gb, 'tpr','fpr') 
    roc_gb.data <- data.frame(fpr=unlist(perf_gb@x.values),
                              tpr=unlist(perf_gb@y.values), model="Gradient Boosting")
    
    ##Random Forest 
    train_ub$Class <- ifelse(train_ub$Class==1, 1,0)
    test$Class <- ifelse(test$Class==1, 1,0)
    rf.prob <- predict(rf.fit(),test,type="prob")
    ROCRpred_rf <- prediction(rf.prob[,2], test$Class)
    perf_rf <- ROCR::performance(ROCRpred_rf, 'tpr','fpr') 
    roc_rf.data <- data.frame(fpr=unlist(perf_rf@x.values),
                              tpr=unlist(perf_rf@y.values), model="Random Forest")

    ##Ensemble
    ggplot() + 
      geom_line(data = roc_glm.data, aes(x=fpr, y=tpr, colour = "Régression Logistique")) + 
      geom_line(data = roc_rf.data, aes(x = fpr, y=tpr, colour = "Random Forest")) +
      geom_line(data = roc_gb.data, aes(x = fpr, y=tpr, colour = "Gradient Boosting")) +
      geom_line(data = roc_svm.data, aes(x = fpr, y=tpr, colour = "Support Vector Machine")) +
      
      geom_abline(color = "red", linetype=2) + theme_bw() + 
      scale_colour_manual(name = "Modèles", values = cols) + 
      xlab("Taux de Faux positifs") +
      ylab("Taux de Vrais positifs") +
      theme(legend.position = c(0.8, 0.2), 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15))

  })
  output$ma_table <- renderTable({
    #Régression logistique
    ROCRpred_glm <- prediction(glm.prob(), test$Class)
    AUC_glm <- ROCR::performance(ROCRpred_glm, 'auc')
    AUC_glm <- paste(round(unlist(AUC_glm@y.values)*100, 3),"%")
    
    #Random Forest
    train_ub$Class <- ifelse(train_ub$Class==1, 1,0)
    test$Class <- ifelse(test$Class==1, 1,0)
    rf.prob <- predict(rf.fit(),test,type="prob")
    ROCRpred_rf <- prediction(rf.prob[,2], test$Class)
    AUC_rf <- ROCR::performance(ROCRpred_rf, measure='auc')
    AUC_rf <- paste(round(unlist(AUC_rf@y.values)*100, 3),"%")
    
    #SVM
    svm.fit.prob <-attr(svm.pred(),"probabilities")
    ROCRpred_svm <- prediction(svm.fit.prob[,2], test$Class)
    AUC_svm <- ROCR::performance(ROCRpred_svm,'auc')
    AUC_svm <- paste(round(unlist(AUC_svm@y.values)*100, 3),"%")
    
    #Gradient Boosting
    ROCRpred_gb <- prediction(boost.pred(), test$Class)
    AUC_gb <- ROCR::performance(ROCRpred_gb, 'auc')
    AUC_gb <- paste(round(unlist(AUC_gb@y.values)*100, 3),"%")
    
    
    
    df <- data.frame(SVM=c(erreur(cmsvm()),AUC_svm), RL=c(erreur(cmrl()),AUC_glm), GB=c(erreur(cmgb()),AUC_gb), RF=c(erreur(cmrf()),AUC_rf))
    rownames(df)<-c("Taux d'erreur", "AUC")
    colnames(df) <- c("Support Vector Machine","Régression Logistique","Gradient Boosting","Random Forest")
    df
    
    
  },digits=4,rownames=TRUE,width=600) 
  
})