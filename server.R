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
  
  output$m_svm <- renderPlot({
    svm_model <- svm(form, data=train_ub, tpe="C-classification", kernel ="linear", scale=F)
    pred_test <- predict(svm_model, test)
    cmtrx <- confusionMatrix(test$Class, pred_test)
    draw_confusion_matrix(cmtrx, cols[1])
    
  })
  
   
# Régression logistique
   
   output$confusion_RL <- renderPlot({
     glm.fit <- glm(Class~.,data=train_ub,family="binomial")
     
     glm.prob <- predict(glm.fit, test, type="response")
     
     glm.pred <- rep(0,nrow(test))
     glm.pred[glm.prob<=.5]=0
     glm.pred[glm.prob>.5]=1
     
     glm.pred <- as.factor(glm.pred)
     
     cmrl <- confusionMatrix(test$Class, glm.pred)
     draw_confusion_matrix(cmrl, cols[2])
   })

   
  
# RandomForest
  
  output$selected_param <- renderText({ 
    paste( "Vous avez choisi le nombre de feuilles égales à", input$mtry, "et un nombre d'arbres égal à", input$ntree,".")
  })
  
  output$optimal <- renderText({ 
    paste( "Les paramètres optimaux qui permettent de minimiser le taux d'erreur sont de", mtry_opt, "pour le nombres de feuilles et",ntree_opt, "pour le nombres d'arbres dans la forêt.")
  })
  
  output$confusion_rf <- renderPlot({
    rf.data <- randomForest(form,train_ub, mtry=input$mtry,ntree=input$ntree)
    rf.pred <- predict(rf.data,test,type="response")
    m_rf <- table(rf.pred,test$Class)
    cmrf <- confusionMatrix(test$Class, rf.pred)
    draw_confusion_matrix(cmrf, cols[4])
  })
  
  output$erreur_rf <- renderText({
    rf.data <- randomForest(form,train_ub, mtry=input$mtry,ntree=input$ntree)
    rf.pred <- predict(rf.data,test,type="response")
    taux_erreur <- mean(rf.pred!=test$Class)
    paste( "L'erreur est de", round(taux_erreur,4)*100,"%.")
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
    draw_confusion_matrix(cmtrx, cols[5])
    
  })

  
  output$roc <-renderPlot({
    
    #Régression logistique
    glm.fit=glm(Class~.,data=train_ub,family="binomial")
    glm.prob=predict(glm.fit, test, type="response")
    ROCRpred_glm <- prediction(glm.prob, test$Class)
    perf_glm <- ROCR::performance(ROCRpred_glm, 'tpr','fpr')
    roc_glm.data <- data.frame(fpr=unlist(perf_glm@x.values),
                               tpr=unlist(perf_glm@y.values), model="Régression logistique")
    #Random Forest 
    rf.data=randomForest(form,train_ub, mtry=input$mtry,ntree=input$ntree)
    train_ub$Class <- ifelse(train_ub$Class==1, 1,0)
    test$Class <- ifelse(test$Class==1, 1,0)
    rf.pred=predict(rf.data,test,type="prob")
    ROCRpred_rf <- prediction(rf.pred[,2], test$Class)
    perf_rf <- ROCR::performance(ROCRpred_rf, 'tpr','fpr') 
    roc_rf.data <- data.frame(fpr=unlist(perf_rf@x.values),
                              tpr=unlist(perf_rf@y.values), model="Random Forest")
    #SVM
    train_ub$Class <- ifelse(train_ub$Class==1, 1,0)
    test$Class <- ifelse(test$Class==1, 1,0)
    svm_model <- svm(form, data=train_ub, tpe="C-classification", kernel ="linear",scale=T)
    svm.prob <- predict(svm_model, test)
    ROCRpred_svm <- prediction(as.numeric(svm.prob), as.numeric(test$Class))
    ROCRpred_svm
    perf_svm <- ROCR::performance(ROCRpred_svm, 'tpr','fpr')
    roc_svm.data <- data.frame(fpr=unlist(perf_svm@x.values),
                               tpr=unlist(perf_svm@y.values), model="Support Vector Machine")
  
    
    
    #Gradient Boosting
    train_ub$Class <- ifelse(train_ub$Class==1, 1,0)
    test$Class <- ifelse(test$Class==1, 1,0)
    boost <- gbm(form, data=train_ub, distribution="bernoulli", n.trees=5000)
    pred_test <- predict(boost, newdata=test, type="response", n.trees=5000)
    
    ROCRpred_gb <- prediction(pred_test, test$Class)
    perf_gb <- ROCR::performance(ROCRpred_gb, 'tpr','fpr') 
    roc_gb.data <- data.frame(fpr=unlist(perf_gb@x.values),
                              tpr=unlist(perf_gb@y.values), model="Gradient Boosting")
    
    
    ggplot() + 
      geom_line(data = roc_svm.data, aes(x = fpr, y=tpr, colour = "Support Vector Machine")) +
      geom_line(data = roc_glm.data, aes(x=fpr, y=tpr, colour = "Régression Logistique")) + 
      geom_line(data = roc_rf.data, aes(x = fpr, y=tpr, colour = "Random Forest")) +
      geom_line(data = roc_gb.data, aes(x = fpr, y=tpr, colour = "Gradient Boosting")) +
      
      #set LR roc curve
      geom_abline(color = "red", linetype=2) + theme_bw() + 
      scale_colour_manual(name = "Modèles", values = cols) + 
      xlab("Taux de Faux positifs") +
      ylab("Taux de Vrais positifs") +
      theme(legend.position = c(0.8, 0.2), 
            legend.text = element_text(size = 15), 
            legend.title = element_text(size = 15))
  })
  
})
