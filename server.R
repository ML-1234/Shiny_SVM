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
train_ub<-as.data.frame(cbind(newData$X, newData$Y))
colnames(train_ub)[colnames(train_ub)=="newData$Y"] <- "Class"


####Fonctions#####
###Création Matrice de confusion en Plot###
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('MATRICE DE CONFUSION', cex.main=2)
  
  # Création de la matrice
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Pas de défaut', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Défaut', cex=1.2)
  text(125, 370, 'Observation', cex=1.3, srt=90, font=2)
  text(245, 450, 'Prédiction', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Pas de défaut', cex=1.2, srt=90)
  text(140, 335, 'Défaut', cex=1.2, srt=90)
  
  # Ajout des informations de la matrice 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # Ajout des détails
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, "Sensitivité", cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, "Spécificité", cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, "Précision", cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # Ajout de l'Accuracy et Kappa
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}



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
  
  output$m_svm <- renderPlot({
    svm_model <- svm(form, data=train_ub, tpe="C-classification", kernel ="linear", scale=F)
    pred_test <- predict(svm_model, test)
    cmtrx <- confusionMatrix(test$Class, pred_test)
    draw_confusion_matrix(cmtrx)
    
  })
  
   output$rocsvm <- renderPlot({
    svm_model <- svm(form, data=train_ub, tpe="C-classification", kernel ="linear", scale=F)
    pred_test <- predict(svm_model, test)
    pred=as.numeric(pred_test)
    roc=roc(test$Class,pred,plot=TRUE, print.auc=TRUE, col="red", main= "Courbe ROC")

  })
  
# RandomForest
  
  output$selected_mtry <- renderText({ 
    paste( "Vous avez choisi le nombre de feuilles égales à", input$mtry, "et un nombre d'arbres égal à", input$ntree,".")
  })
  
  
# KNN
  
  output$txtknn<- renderText({ 
    paste( "Vous avez choisi un nombre de voisins égales à", input$k,".")
  })
  
   output$confusion_knn <- renderPlot({
    pred = knn(train_ub[,1:30], test[,1:30], train_ub[,31], k=input$k)
    cmknn <- confusionMatrix(test$Class, pred)
    draw_confusion_matrix(cmknn)
  })
  
  output$rocknn <- renderPlot({
    pred = knn(train_ub[,1:30], test[,1:30], train_ub[,31], k=input$k)
    predknn=as.numeric(pred)
    rocknn=roc(test$Class,predknn,plot=TRUE, print.auc=TRUE, col="red", main= "Courbe ROC")

  })
 
  
  # Gradient Boosting
  
  output$m_gb <- renderPlot({
    train_ub$Class <- ifelse(train_ub$Class==1, 1,0)
    test$Class <- ifelse(test$Class==1, 1,0)
    boost <- gbm(form, data=train_ub, distribution="bernoulli", n.trees=5000)
    pred_test <- predict(boost, newdata=test, type="response", n.trees=5000)
    pred_test_class <- factor(ifelse(pred_test>0.5, 1,0))
    
    test$Class <- as.factor(test$Class)
    train_ub$Class <- as.factor(train_ub$Class)
    cmtrx <- confusionMatrix(test$Class, pred_test_class)
    draw_confusion_matrix(cmtrx)
    
  })
  
  output$roc_gb <- renderPlot({
    train_ub$Class <- ifelse(train_ub$Class==1, 1,0)
    test$Class <- ifelse(test$Class==1, 1,0)
    boost <- gbm(form, data=train_ub, distribution="bernoulli", n.trees=5000)
    pred_test <- predict(boost, newdata=test, type="response", n.trees=5000)
    
    ROCRpred_gb <- prediction(pred_test, test$Class)
    perf_gb <- ROCR::performance(ROCRpred_gb, 'tpr','fpr') 
    roc_gb.data <- data.frame(fpr=unlist(perf_gb@x.values),
                              tpr=unlist(perf_gb@y.values), model="Gradient Boosting")
    
    # Couleurs du graphique
    cols <- c("Gradient Boosting" = "#3DB7E4")
    
    # Création de la courbe ROC
    ggplot() + 
      geom_line(data = roc_gb.data, aes(x=fpr, y=tpr, colour = "Gradient Boosting")) +
      geom_abline(color = "red", linetype=2) + theme_bw() + 
      scale_colour_manual(name = "Modèle", values = cols) + 
      xlab("Taux de Faux positifs") +
      ylab("Taux de Vrais positifs") +
      theme(legend.position = c(0.8, 0.2), 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15))
    
  })
  
  # Régression logistique
  
  output$confusion_RL <- renderPlot({
    glm.fit=glm(Class~.,data=train_ub,family="binomial")
    
    glm.prob=predict(glm.fit, test, type="response")
    
    glm.pred=rep(0,nrow(test))
    glm.pred[glm.prob<=.5]=0
    glm.pred[glm.prob>.5]=1
    
    glm.pred=as.factor(glm.pred)
    
    cmrl <- confusionMatrix(test$Class, glm.pred)
    draw_confusion_matrix(cmrl)
  })
  
   output$rocrl <- renderPlot({
    glm.fit=glm(Class~.,data=train_ub,family="binomial")
    
    glm.prob=predict(glm.fit, test, type="response")
    
    glm.pred=rep(0,nrow(test))
    glm.pred[glm.prob<=.5]=0
    glm.pred[glm.prob>.5]=1
    
    predlr=as.numeric(glm.pred)
    roclr=roc(test$Class,predlr,plot=TRUE, print.auc=TRUE, col="red", main= "Courbe ROC")

  })
  
  
})
