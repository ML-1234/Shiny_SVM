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
  text(125, 370, 'Prédiction', cex=1.3, srt=90, font=2)
  text(245, 450, 'Observation', cex=1.3, font=2)
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
  
  output$pre <- renderText({
    paste( "<br> <br> Dans le cadre de notre cursus universitaire, nous avons mis en place un démonstrateur sous R Shiny afin de montrer l'implémentation 
           et les performances des machines à vecteurs de support dans la détection des transactions frauduleuses commises sur les cartes de crédit.<br> <br>
           Avant de commencer, il est important pour nous de remercier M. HURLIN, créateur de ce projet et professeur de SVM, M. DELSOL, professeur de R Shiny ainsi que  M. DUDEK pour son intervention sur le déploiement d'applications Shiny sous Github.<br> <br>
           Dès à présent, afin de comprendre le fonctionnement de ce démonstrateur, nous vous invitons à télécharger la notice située dans l'onglet suivant.")
  })
  
  
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
  
  
  output$intro <- renderText({
    paste( " Les <b>Support Vector Machines</b> (SVM) représentent une méthode statistique développée dans les années 1990.<br> 
           Cette méthode est destinée à résoudre des problèmes de classification puisqu’elle va permettre de déterminer si un élément appartient ou non à une classe.
           ")
  })
  
  output$intro2 <- renderText({
    paste( "Pour mieux comprendre son fonctionnement, il est utile de s’intéresser à sa représentation graphique.
           Pour cela, on dispose d’un ensemble de données. <br>
           Notre but va être de chercher à les séparer en deux groupes distincts. <br>
           Un groupe représente ainsi la survenance de l’évènement (prévision 1) et l’autre la non-survenance (prévision 0). <br>
           Cette séparation linéaire va se faire à l’aide d’une frontière appelée <b>hyperplan</b>.")
  })
  #Bdd linéairement séparable
  n <- 500
  delta <- 0.13
  df_linear <- data.frame(x1 = runif(n), x2 = runif(n))
  df_linear$y <- factor(ifelse(df_linear$x2 - 1.4*df_linear$x1 < 0, -1, 1), levels = c(-1, 1))
  df_linear<- df_linear[abs(1.4*df_linear$x1 - df_linear$x2) > delta, ]
  
  
  output$plot_linear <- renderPlot({
    
    plot_margins <- ggplot(data = df_linear, aes(x = x1, y = x2, color = y)) + geom_point() + 
      scale_color_manual(values = c("red", "blue")) + theme_bw() +
      geom_abline(slope = 1.4, intercept = 0)+
      geom_abline(slope = 1.4, intercept = delta, linetype = "dashed") +
      geom_abline(slope = 1.4, intercept = -delta, linetype = "dashed")
    
    
    plot_margins
  })
  
  output$vs <- renderText({
    paste( "Il existe de nombreux hyperplans séparateurs.
           L’algorithme SVM va nous aider à trouver l'optimal, celui qui maximise la séparation en classant correctement toutes les observations. <br> 
           Pour le trouver, il suffit de chercher l’hyperplan pour lequel la distance entre la frontière des deux groupes et l’observation la plus proche est maximale. <br>
           
           Le double de cette distance est appelée <b>marge</b>. On parlera donc de maximisation de la marge. <br>
           Il en résulte que les observations les plus proches de la frontière, appelées <b> vecteurs de supports </b>, sont les points situés sur les extrémités de la marge.")
  })
  
  output$plot_linear_SVM <- renderPlot({
    
    svm_model<- svm(y ~ .,data = df_linear,type = "C-classification", kernel = "linear", scale = FALSE)
    
    SVM_plot <- ggplot(data = df_linear, aes(x = x1, y = x2, color = y)) + 
      geom_point() +
      scale_color_manual(values = c("red", "blue")) + geom_point(data = df_linear[svm_model$index, ], aes(x = x1, y = x2), color = "purple", size = 4, alpha = 0.2) + theme_bw()
    
    SVM_plot
    
  })
  
  
  #Bdd presque linéairement séparable
  
  output$cout <- renderText({
    paste( " Cependant, il arrive souvent que l’on soit face à des échantillons non linéairement séparables.
           Dans cette situation, deux cas de figure apparaissent. ")
  })
  
  output$cout2 <- renderText({
    paste( "Le premier est que la séparation optimale reste linéaire malgré le fait que quelques observations ne puissent pas être correctement classées.")
  })
  
  output$plot_almostlinear_SVM <- renderPlot({
    delta <- 0.03
    df_linear <- data.frame(x1 = runif(n), x2 = runif(n))
    df_linear$y <- factor(ifelse(df_linear$x2 - 1.4*df_linear$x1 < 0, -1, 1), levels = c(-1, 1))
    df_linear<- df_linear[abs(1.4*df_linear$x1 - df_linear$x2) > delta, ]
    
    plot_margins_almostlinear <- ggplot(data = df_linear, aes(x = x1, y = x2, color = y)) + geom_point() + 
      scale_color_manual(values = c("red", "blue")) + theme_bw() +
      geom_abline(slope = 1.4, intercept = 0)+
      geom_abline(slope = 1.4, intercept = delta, linetype = "dashed", colour="orange") +
      geom_abline(slope = 1.4, intercept = -delta, linetype = "dashed", colour="orange") +
      geom_abline(slope = 1.4, intercept = 0, colour="orange") +
      geom_abline(slope = 1.4, intercept = delta*3, linetype = "dashed") +
      geom_abline(slope = 1.4, intercept = -delta*3, linetype = "dashed")
    
    plot_margins_almostlinear
  })
  
  output$vr <- renderText({
    paste( "Pour définir le nombre d'observations mal classées autorisé <b>(variable ressort)</b>, on fait appel à un <b>paramètre de pénalisation</b> qui est le <b>coût</b>.<br>
           On l'utilise car les performances des SVMs y sont très sensibles. <br>
           Ce paramètre permet l’acceptation d'un certain nombre de variables ressorts dans le but de maximiser la marge. <br>
           
           Cependant, il faut être prudent car lorsqu'on choisit un coût élevé, cela signifie que peu d’erreurs de classification sont acceptées et donc que la marge sera plus petite.
           Dans ce cas, on fait face à un risque de <b>sur-apprentissage</b>. <br>
           Dans la situation inverse, lorsque le coût est faible, la priorité est donnée à la maximisation de la marge, au préjudice de la minimisation du nombre d’erreurs de classification. 
           On est alors face à un risque de <b>sous-apprentissage</b>.  <br>
           L'objectif est alors de trouver un arbitrage entre l’optimisation de la marge et le nombre d'erreurs de classification. ")
  })
  
  output$vr2 <- renderText({
    paste( "
           Le deuxième cas de figure apparaît lorsque l’échantillon n’est pas linéairement séparable.")
  })
  #Bdd radialement séparable
  
  output$plot_radial_SVM <- renderPlot({
    df <- data.frame(x1 = runif(n, min = -1, max = 1), 
                     x2 = runif(n, min = -1, max = 1))
    
    radius <- 0.8
    radius_squared <- radius^2
    
    df$y <- factor(ifelse(df$x1^2 + df$x2^2 < radius_squared, -1, 1), levels = c(-1, 1))
    
    scatter_plot <- ggplot(data = df, aes(x = x1, y = x2, color = y)) + 
      geom_point() + theme_bw() +
      scale_color_manual(values = c("red", "blue"))
    
    scatter_plot
    
  })
  output$fin <- renderText({
    paste( "Ici on constate que la séparation linéaire n’est pas possible.<br> 
           Afin de trouver la séparation optimale on va alors chercher à <b>transformer l’espace de représentation des données d’entrée</b> en un espace de plus grandes dimensions en rajoutant des variables explicatives créées à partir de la transformation des variables initiales.<br>
           Cette transformation se fait à l’aide des <b>fonctions kernels</b>. Elles sont très utile puisque l’on n’a pas besoin de connaître la transformation à appliquer.<br>
           Dans ce nouvel espace de plus grande dimension, il sera alors plus probable de trouver une séparation linéaire. <br> 
           ")
    
  })
  
  # SVM
  ##Modèle
  
  svm.fit <- reactive({svm(form, data=train_ub, type="C-classification", kernel ="linear", probability = TRUE)})
  
  svm.pred <- reactive({predict(svm.fit(), test, probability=TRUE)})
  
  cmsvm <- reactive({pred <- svm.pred()
  confusionMatrix(pred, test$Class)})
  
  ##Matrice de confusion
  output$m_svm <- renderPlot({
    draw_confusion_matrix(cmsvm(), cols[1])
  })
  
  
  # Régression logistique
  ##Modèle
  
  RL <- ubBalance(X, Y, type="ubUnder", positive=1, perc=2, method="percUnder")
  dataRL<-as.data.frame(cbind(RL$X, RL$Y))
  colnames(dataRL)[colnames(dataRL)=="RL$Y"] <- "Class"
  
  glm.fit <- reactive({glm(form,data=dataRL,family="binomial")})
  glm.prob <- reactive({predict(glm.fit(), test, type="response")})
  
  cmrl <- reactive({glm.pred <- factor(ifelse(glm.prob()>0.5, 1,0))
  confusionMatrix(glm.pred, test$Class)})
  
  ##Matrice de confusion
  output$confusion_RL <- renderPlot({
    draw_confusion_matrix(cmrl(), cols[2])
  })
  
  
  
  # RandomForest
  ## Modèle et matrice simple
  rf.fit <- reactive({randomForest(form,train_ub, mtry=input$mtry,ntree=input$ntree)})
  rf.pred <- reactive({predict(rf.fit(),test,type="response")})
  cmrf <- reactive({confusionMatrix(rf.pred(), test$Class)})
  
  output$selected_param <- renderText({ 
    paste( "Vous avez choisi le nombre de feuilles égales à", input$mtry, "et un nombre d'arbres égal à", input$ntree,". <br> <br>")
  })
  
  output$optimal <- renderText({ 
    paste( "<br> Les paramètres optimaux qui permettent de minimiser le taux d'erreur sont de", mtry_opt, "pour le nombre de feuilles et",ntree_opt, "pour le nombre d'arbres dans la forêt.")
  })
  
  ##Matrice de confusion
  output$confusion_rf <- renderPlot({draw_confusion_matrix(cmrf(), cols[4])})
  
  output$erreur_rf <- renderText({
    taux_erreur <- paste(round((1 - sum(diag(cmrf()$table))/sum(cmrf()$table))*100, 3),"%")
    paste( "L'erreur est de", taux_erreur,".")
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
    confusionMatrix(boost.pred.class, test$Class)})
  
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
      
      geom_abline(color = "darkgrey", linetype=2) + theme_bw() + 
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
    
    
  },digits=4, striped = TRUE, bordered = TRUE, rownames=TRUE,width=600) 
  
  })
