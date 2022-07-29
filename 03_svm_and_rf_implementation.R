library(caret)
library(class)
library(cvTools)
library(e1071)
library(iknnr)
library(ggplot2)
library(gridExtra)
library(reshape2)
library(randomForest)



set.seed(1)
plotagrafico <- function(media1,media2){
  
  # GERACAO DO DATASET
  x<-rbind(matrix(rnorm(300,mean=media1,sd=1),ncol=2),
           matrix(rnorm(300,mean=media2,sd=1),ncol=2))
  x = data.frame(x)
  y <- c(rep(1,150),rep(2, 150))
  
  dataset <- cbind(x,y)
  colnames(dataset) <- c("X1", "X2", "Y")
  dataset
  x[1:150,]
  m1 <- "media1"
  assign(m1,media1)
  m1 <- "1="
  m2 <- "2="
  
  colnames(x) <- c("col1","col2")
  dataset[1:150,1]
  x[1:150,1]
  
  
  p <- ggplot(x[1:150,],aes(x= col1,y = col2)) + geom_point(aes(color = "Class1")) + geom_point(data = x[151:300,],aes(x=col1, y=col2, color = "Class2"))+ xlab(bquote(mu*.(m1) ~ .(media1) ~ mu*.(m2) ~ .(media2)))
  a <- p +theme(
    aspect.ratio = 0.3,
    plot.title = element_text(color = "black", size = 14, face = "bold", hjust = 0.5),
    axis.title.x = element_text(color = "black", size = 12, hjust = 0.5, vjust = 0),
    axis.line=element_blank(),axis.text.x=element_blank(),
    axis.text.y=element_blank(),axis.ticks=element_blank(),
    axis.title.y=element_blank()) + 
    scale_color_manual(name = element_blank(),values = c("red", "blue"))
  a
  assign("a",a,envir= globalenv())
  print(a)
  class1 = stack(data.frame(x[1:150,]))
  class1 <- class1[,1]
  class2 <- stack(data.frame(x[151:300,]))
  class2 <- class2[,1]
  assign("class1", class1, envir = globalenv())
  assign("class2", class2, envir = globalenv())
  
  
  '
  ##############################################################################
  ### EXPERIMENTOS COMPARATIVOS PARA ANALISES DE DESEMPENHO
  ##############################################################################
  
  '
  #Fisher s discriminant radio(F1)
  fisherratio <- function(class1,class2){
    return ((mean(class1) - mean(class2))^2)/(var(class1)+var(class2))}
  #Volume of overlap region(F2)
  F2 <- function(class1,class2){
    minmax = min(max(class1), max(class2))
    maxmin = max(min(class1), min(class2))
    maxmax = max(max(class1), max(class2))
    minmin = min(min(class1), min(class2))
    return (minmax - maxmin)/(maxmax-minmin)
  }
  vetorf1 <- c(vetorf1,fisherratio(class1, class2)/10)
  assign("vetorf1", vetorf1, envir = globalenv())
  
  #---------------------------------------------------------
  
  
  
  #---------------------------------------------------------
  
  
  
  
  
  pastas <- 5
  folds <- cvFolds(300,K=pastas,type=c("random"))
      acuraciarf <- NULL
      acuraciasvm <- NULL
      f1rf <- NULL
      f1svm <- NULL
  
   
      for (pastinha in 1:pastas){
        
        
        i_treino<-folds$subsets[which(folds$which != pastinha)]
        i_teste<- folds$subsets[which(folds$which == pastinha)]
        #TREINO
        X_treino<-dataset[i_treino,1:2]
        Y_treino<-dataset[i_treino,3]
        
        #TESTE
        X_teste<-dataset[i_teste,1:2]
        Y_teste<-dataset[i_teste,3]
        
        dim(X_treino)
        length(Y_treino)
        dim(X_teste)
        length(Y_teste)
        
        Y_treino
        
        
        ###################################
        #SVM
        ###################################
        
        
        Y_treino <- as.factor(Y_treino)
        trainset <- cbind(X_treino,Y_treino)
        start.time <- Sys.time()
        svm.model <- svm(Y_treino ~ ., data = trainset)
        end.time <- Sys.time()
        temposvmmodelo <- as.numeric(difftime(end.time,start.time), units = "secs")
        assign("temposvmmodelo", temposvmmodelo, envir = globalenv())
        
        iniciosvm <- Sys.time()
        svm.pred <- predict(svm.model, X_teste)
        fimsvm <- Sys.time()
        temposvmpred <- as.numeric(difftime(fimsvm,iniciosvm), units = "secs")
        assign("temposvmpred", temposvmpred, envir = globalenv())
        
        
        cm <- table(svm.pred,Y_teste)
        
        acuracia <- sum(diag(cm)) / 60
        precision <- cm[1,1]/sum(cm[1,1:2])
        recall <- cm[1,1]/sum(cm[1:2,1])
        f1 <- 2*((precision*recall)/(precision + recall))
        
        acuraciasvm <- c(acuraciasvm,acuracia)
        f1svm <- c(f1svm,f1)
        assign("acuraciasvm",acuraciasvm,envir = globalenv())
        assign("f1svm", f1svm, envir = globalenv())
        
        
        
        ###################################
        #Random Forest
        ###################################
        start.time <- Sys.time()
        model.rf <- randomForest(Y_treino ~ ., trainset, ntree=50, norm.votes=FALSE)
        end.time <- Sys.time()
        temporfmodelo <- as.numeric(difftime(end.time,start.time), units = "secs")
        
        start.time <- Sys.time()
        predict.rf <- predict(model.rf,X_teste)
        end.time <- Sys.time()
        temporfpred <- as.numeric(difftime(end.time,start.time), units = "secs")
        
        assign("temporfmodelo", temporfmodelo, envir = globalenv())
        assign("temporfpred", temporfpred, envir = globalenv())
        
        
        
        cm <- table(predict.rf,Y_teste)
        acuracia <- sum(diag(cm)) / 60
        precision <- cm[1,1]/sum(cm[1,1:2])
        recall <- cm[1,1]/sum(cm[1:2,1])
        f1 <- 2*((precision*recall)/(precision + recall))
        
        acuraciarf <- c(acuraciarf,acuracia)
        f1rf <- c(f1rf,f1)
        amedia <- (media1-media2)
        
        assign("acuraciarf", acuraciarf,envir = globalenv())
        assign("f1rf", f1svm, envir = globalenv())
        
      }
  mediasvm <- c(mediasvm,(mean(acuraciasvm)))
  mediaf1svm <- c(mediaf1svm,mean(f1svm))
  assign("mediasvm", mediasvm, envir = globalenv())
  assign("mediaf1svm", mediaf1svm, envir = globalenv())
  
  mediarf <- c(mediarf,(mean(acuraciarf)))
  mediaf1rf <- c(mediaf1rf,mean(f1rf))
  assign("mediarf", mediarf, envir = globalenv())
  assign("mediaf1rf", mediaf1rf, envir = globalenv())
  
  vetormedia <- c(vetormedia,media2-media1)
  assign("vetormedia", vetormedia,envir = globalenv())
 
 
    
  }

  
  

acuraciaknn <- NULL
acuraciaiknn <- NULL
acuraciasvm <- NULL
acuraciarf <- NULL
mediaknn <- NULL
mediasvm <- NULL
mediarf <- NULL
mediaiknn <- NULL
mediaf1knn <- NULL
mediaf1iknn <- NULL
mediaf1svm <- NULL
mediaf1rf <- NULL
f1knn <- NULL
f1iknn <- NULL
f1svm <- NULL
f1rf <- NULL
listaelk <- NULL
listaelki <- NULL



mediafinal <- NULL
vetormedia <- NULL
vetoracuracia <- NULL
vetoracuracia3 <- NULL
vetoracuracia5<- NULL
vetoracuracia7<- NULL
vetoracuracia9 <- NULL
vetorbestacuracia <- NULL
vetorbestk <- NULL
vetortempknn <- NULL
vetmodelbestknn <- NULL
vetmodelbestrf <- NULL
vetmodelbestsvm <- NULL
vetorf1scorerf <- NULL
vetorf1scoresvm <- NULL
tempobestknn = tempobestrf = tempobestsvm <- 0
#vetorinn <- NULL
vetorf1 <- NULL
eaex <- NULL
b<- NULL
tail(vetorbestk)
b <- 0
pltlist <- list()
pltlist[]
media = 60
for (i in 1:29){
  plotagrafico(media,60)
  media <- media-0.25
  pltlist[[i]] = a
  
}
#MATRIZ DE COMPARACAO
f1knnm <- mean(bestf1knn)
f1iknnm <- mean(bestf1iknn) 
f1rfm <- mean(mediaf1rf)
f1svmm <- mean(mediaf1svm)
f1knnsd <- sd(bestf1knn)
f1iknnsd <- sd(bestf1iknn)   
f1rfsd <- sd(mediaf1rf)
f1svmsd <- sd(mediaf1svm)
acknnm <- mean(bestknn)
aciknnm <- mean(bestiknn)
acrf <- mean(mediarf)
acsvm <- mean(mediasvm)
sdknn <- sd(bestknn)
sdiknn <- sd(bestiknn)
sdrf <- sd(mediarf)
sdsvm <- sd(mediasvm)




graficomparae <- c(bestiknn, bestknn,mediarf,mediasvm,vetormedia,vetorf1/vetorf1[length(vetorf1)])
mgraficomparae <- matrix(graficomparae, nrow = 6, byrow = TRUE, dimnames = list(c("Best IKNN","Best KNN","Random Forest","SVM","Media","F1"),c(1:i)))
mgraficomparaedf <- data.frame(t(mgraficomparae))
mgraficomparaedf.melt <- melt(mgraficomparaedf,c("Media"))
p4 <- ggplot(mgraficomparaedf.melt,aes(x = Media,y = value,colour = variable)) + geom_line() + 
  geom_point() + ggtitle("Comparacao entre Best KNN e Best INN") +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),legend.title=element_blank()) + scale_color_hue(labels = c("Best INN","Best KNN","Random Forest","SVM", "F1"))+
  xlab(bquote(mu*2 - mu*.(1))) + ylab("Classification Accuracy(%)") + 
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "F1"))


print(p4)


exp<- matrix(c(f1knnm,f1iknnm,f1rfm,f1svmm,f1knnsd,f1iknnsd,f1rfsd,f1svmsd,acknnm,aciknnm,acrf,acsvm,sdknn,sdiknn,sdrf,sdsvm,tempoknn,tempoiknn,temposvmpred,temporfpred),nrow = 4,ncol = 5
             ,byrow = FALSE)
dimnames(exp) = list(c("KNN", "IKNN","RANDOM FOREST", "SVM"),c("F1-Score(Media)","F1-Score(Desvio)", "Acuracia(Media)", "Acuracia(Desvio)", "Tempo"))
print(exp)

