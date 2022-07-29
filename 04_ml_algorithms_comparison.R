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
  
  
  p <- ggplot(x[1:150,],aes(x= col1,y = col2)) + geom_point(aes(color = "Class1")) + geom_point(data = x[151:300,],aes(x=col1, y=col2, color = "Class2"))+ xlab(bquote(mu*.(m1) ~ .(60) ~ mu*.(m2) ~ .(51)))
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
  ### EXPERIMENTOS COMPARATIVOS PARA ANALISES DE DESEMPENHO (não são para ajustar parametros)
  ##############################################################################
  
  c
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
  
  
  
  
  # #KFOLD AND IkNN
  # library(cvTools)
  # N<-dim(datanorm)[1]
  # pastas<-5
  # folds <- cvFolds(N,K=pastas,type=c("random"))
  # acuraciaacerto_knn <- rep(0,pastas)
  # 
  
  pastas <- 5
  folds <- cvFolds(300,K=pastas,type=c("random"))
  
  
for(elk in  seq(1,15,2)) {
  for(elki in 1:elk){
    acuraciasvm <- NULL
    acuraciarf <- NULL
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
      
      #---------------------------------------------------------
      
      ###################################
      #BEST KNN
      ###################################
      start.time <- Sys.time()
      modelbestknn<- knn(as.matrix(X_treino), as.matrix(X_teste), as.matrix(Y_treino),k = elk)
      end.time <- Sys.time()
      tempoknn <- as.numeric(difftime(end.time,start.time), units = "secs")
      assign("tempoknn", tempoknn,envir = globalenv())
      
      cm <- table(modelbestknn,Y_teste)
      cm
      assign("modelbestknn", modelbestknn, envir = globalenv())
      assign('cm1', cm, envir = globalenv())
      acuracia <- sum(diag(cm)) / 60
      precision <- cm[1,1]/sum(cm[1,1:2])
      recall <- cm[1,1]/sum(cm[1:2,1])
      f1 <- 2*((precision*recall)/(precision + recall))
      #acuraciaknn <- NULL
      acuraciaknn <- c(acuraciaknn,acuracia)
      f1knn <- c(f1knn,f1)
      
      
      assign("acuraciaknn",acuraciaknn,envir = globalenv())
      assign("f1knn", f1knn, envir = globalenv())
      
      
      #---------------------------------------------------------
      
      ###################################
      #BEST IKNN
      ###################################
      start.time <- Sys.time()
      modelbestiknn <- iknn(as.matrix(X_treino),as.matrix(Y_treino),as.matrix(X_teste),as.matrix(Y_teste),elk,elki)
      end.time <- Sys.time()
      tempoiknn <- as.numeric(difftime(end.time,start.time), units = "secs")
      
      assign("tempoiknn", tempoiknn, envir = globalenv())
      modelbestiknn <- as.factor(modelbestiknn)
      cm <- table(as.matrix(Y_teste),modelbestiknn)
      cm
      assign("modelbestiknn", modelbestiknn, envir = globalenv())
      assign("cm2", cm,envir = globalenv())
      acuracia <- sum(diag(cm))/ 60
      precision <- cm[1,1]/sum(cm[1,1:2])
      recall <- cm[1,1]/sum(cm[1:2,1])
      f1 <- 2*((precision*recall)/(precision + recall))
      #acuraciaknn <- NULL
      acuraciaiknn <- c(acuraciaiknn,acuracia)
      f1iknn <- c(f1iknn,f1)
      #colnameiknn <- c(colnameiknn,paste(toString(el),toString(elki),sep = ""))
      
      assign("acuraciaiknn",acuraciaiknn,envir = globalenv())
      assign("f1iknn", f1iknn, envir = globalenv())
      
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
      
      start.time <- Sys.time()
      svm.pred <- predict(svm.model, X_teste)
      end.time <- Sys.time()
      temposvmpred <- as.numeric(difftime(end.time,start.time), units = "secs")
      
      
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
      assign("acuraciarf",acuraciarf,envir = globalenv())
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
    
    
    
    mediaiknn <- c(mediaiknn,(mean(acuraciaiknn)))
    mediaf1iknn <- c(mediaf1iknn,mean(f1iknn))
    assign("mediaiknn", mediaiknn, envir = globalenv())
    assign("mediaf1iknn", mediaf1iknn, envir = globalenv())
    acuraciaiknn <- NULL
    listaelk <- c(listaelk,elk)
    listaelki <- c(listaelki,elki)
    assign("listaelk", listaelk, envir = globalenv())
    assign("listaelki", listaelki, envir = globalenv())
    
   
 
    
    
    
  }
  mediaknn <- c(mediaknn,(mean(acuraciaknn)))
  mediaf1knn <- c(mediaf1knn,mean(mean(f1knn)))
  assign("mediaknn", mediaknn, envir = globalenv())
  assign("mediaf1knn", mediaf1knn, envir = globalenv())
  acuraciaknn <- NULL
 


  
  
}
  
 
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
for (i in 1:2){
  plotagrafico(media,60)
  media <- media-0.25
  pltlist[[i]] = a
  
}




#grid.arrange(grobs = pltlist[1:10], nrow = 5, ncol = 2, top = "Exemplos de conjuntos de dados sobrepostos ")
#grid.arrange(grobs = pltlist[11:20], nrow = 5, ncol = 2,top = "Exemplos de conjuntos de dados levemente sobrepostos ")
#grid.arrange(grobs = pltlist[21:29], nrow = 5, ncol = 2,top = "Exemplos de conjuntos de dados nao sobrepostos ") 
listaelk <- listaelk[1:64]
listaelki <- listaelki[1:64]
mknn <- matrix(mediaknn,nrow = i,ncol = 8, byrow = TRUE)
colnames(mknn) <- c("K = 1", "K = 3", "K = 5", "K = 7", "K = 9", "K = 11", "K = 13", "K = 15")
row.names(mknn) <- c(1:i)
miknn <- matrix(mediaiknn,i,ncol = 64, byrow = TRUE)
colnames(miknn) <- paste(listaelk,listaelki)
row.names(miknn) <- c(1:i)

mf1knn <- matrix(mediaf1knn,nrow = i,ncol = 8, byrow = TRUE)
colnames(mf1knn) <- c("K = 1", "K = 3", "K = 5", "K = 7", "K = 9", "K = 11", "K = 13", "K = 15")
row.names(mf1knn) <- c(1:i)
mf1iknn <- matrix(mediaf1iknn,nrow = i,ncol = 64, byrow = TRUE)
colnames(mf1iknn) <- paste(listaelk,listaelki)
row.names(mf1iknn) <- c(1:i)



#GRAFICO COMPARANDO A ACURACIA DE K = 1, K= 3, K= 5 E K = 7
graficocompacuracia <- c(mknn[,1],mknn[,2],mknn[,3],mknn[,4],mknn[,5],mknn[,6],mknn[,7],mknn[,8], vetormedia,vetorf1/vetorf1[length(vetorf1)])
graficomp <- matrix(graficocompacuracia, nrow = 10, byrow = TRUE, dimnames = list(c("K = 1","K = 3","K = 5","K = 7","K = 9", "K = 11", "K = 13", "K = 15","Media","F1"),c(1:(length(graficocompacuracia)/10))))
grafiquedf <- data.frame(t(graficomp),check.names = FALSE)
grafique.melt <- melt(grafiquedf, id = c("Media","F1"))
p1 <- ggplot(data = grafique.melt, aes(x = Media,y  = value, color = variable, fill = variable)) + geom_line() + geom_point()+ ggtitle("Comparacao das acuracias de kNN com K = 1, 3, 5, 7, 9, 11, 13, 15") +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),legend.title=element_blank()) + scale_colour_manual(values=c("red","green","blue","black","brown", "purple", "white", "yellow"))+
  xlab(bquote(mu*2 - mu*.(1))) + ylab("Classification Accuracy(%)")
print(p1)

#GRAFICO DE NN, BEST KNN E F1
graficoacuracia <- c(vetormedia,mknn[,8],mknn[,1],vetorf1/vetorf1[length(vetorf1)])
grafico <- matrix(graficoacuracia, nrow = 4, byrow = TRUE, dimnames = list(c("Media","Best KNN","NN","F1"),c(1:(length(graficoacuracia)/4))))
graficodf <- data.frame(t(grafico))
graficodf.melt <- melt(graficodf,c("Media"))
p2 <- ggplot(data = graficodf.melt, aes(x = Media,y  = value, color = variable, fill = variable)) + geom_line() + geom_point()+ ggtitle("Comparacao das acuracias de NN e kNN com F1") +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),legend.title=element_blank()) + scale_colour_manual(values=c("yellow","red","blue"))+
  xlab(bquote(mu*2 - mu*.(1))) + ylab("Classification Accuracy(%)")
print(p2)

colSums(mknn)
max(colSums(mknn))

##----------------------------------------------------------
##GRAFICOS COMPARATIVOS IKNN E MATRIZ DE CONFUSAO
##----------------------------------------------------------

#PRIMERO MOMENTO
graficoacuraciainn <- c(miknn[,"5 1"],miknn[,"11 1"],miknn[,"15 1"],vetormedia,vetorf1/vetorf1[length(vetorf1)])
graficoacuraciainn
graficoinn <- matrix(graficoacuraciainn, nrow = 5, byrow = TRUE, dimnames = list(c("K = 5","K = 11","K = 15","Media","F1"),c(1:i)))
graficoinndf <- data.frame(t(graficoinn))
length(graficoacuraciainn)
graficoinndf.melt <- melt(graficoinndf,c("Media"))

p1 <- ggplot(graficoinndf.melt,aes(x = Media,y = value,colour = variable)) + geom_line() + 
  geom_point() + ggtitle("Algoritmo IKNN com ki = 1 e K = 5,11,15") +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),legend.title=element_blank()) + scale_color_hue(labels = c("K = 5", "K = 10", "K = 15","F1"))+
  xlab(bquote(mu*2 - mu*.(1))) + ylab("Classification Accuracy(%)")+ 
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "F1")) 
print(p1)

#SEGUNDO MOMENTO
length(graficoinn)
graficovariainn <- c(miknn[,"15 1"],miknn[,"15 3"],miknn[,"15 5"],miknn[,"15 7"],miknn[,"15 9"],miknn[,"15 11"],miknn[,"15 13"],miknn[,"15 15"],vetormedia,vetorf1/vetorf1[length(vetorf1)])
graficovariainn
length(graficovariainn)
graficoiknn <- matrix(graficovariainn, nrow = 10, byrow = TRUE, dimnames = list(c("IkNN = 1","IkNN = 3","IkNN = 5", "Iknn = 7", " IKNN = 9","IKNN = 11","IKNN = 13","IKNN = 15","Media","F1"),c(1:i)))
graficoiknndf <- data.frame(t(graficoiknn))
graficoiknndf.melt <- melt(graficoiknndf,c("Media")) 

p2 <- ggplot(graficoiknndf.melt,aes(x = Media,y = value,colour = variable)) + geom_line() + 
  geom_point() + ggtitle("Algoritmo IKNN com ki = 1,3,5,7,9,11,13,15 e 17 e K = 15") +
  theme(plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),legend.title=element_blank()) + scale_color_hue(labels = c("ki = 1", "ki = 3", "ki = 5","ki = 7","ki = 9", "ki = 11", "ki = 13", "ki = 15","F1"))+
  xlab(bquote(mu*2 - mu*.(1))) + ylab("Classification Accuracy(%)") + 
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "F1")) 
print(p2)
# #COMPARACAO FINAL ENTRE KNN E IKNN
# graficoknneinn <- c(vetoriknn1,vetoriknn3,vetoriknn5,vetoriknn7,vetoriknn9,vetoriknn11,vetoriknn13,vetoriknn15,vetorinn15,vetormedia,vetorf1/vetorf1[length(vetorf1)])
# matriziknn<- matrix(graficoknneinn, nrow = 11, byrow = TRUE, 
#                     dimnames = list(c("IkNN = 1","IkNN = 3","IkNN = 5", "Iknn = 7", " IKNN = 9","IKNN = 11",
#                                       "IKNN = 13","IKNN = 15","K = 15","Media","F1"),c(1:29)))
# matriziknndf <- data.frame(t(matriziknn))
# matriziknndf.melt <- melt(matriziknndf,c("Media"))
# p3 <- ggplot(matriziknndf.melt,aes(x = Media,y = value,colour = variable)) + geom_line() + 
#   
#   geom_point() + ggtitle("Comparacao entre kNN com K = 15 e IKNN com diversos valores de ki") +
#   theme(plot.title = element_text(color = "black", size = 12, face = "bold", hjust = 0.5),legend.title=element_blank()) + scale_color_hue(labels = c("ki = 1","ki = 3","ki = 5", "ki = 7", " ki = 9","ki = 11",
#                                                                                                                                                      "ki = 13","ki = 15","K = 15", "F1"))+
#   xlab(bquote(mu*2 - mu*.(1))) + ylab("Classification Accuracy(%)") + 
#   scale_y_continuous(sec.axis = sec_axis(~.*5, name = "F1")) 
# print(p3)

cknn <- which(colMeans(mknn) == max(colMeans(mknn)))
bestknn <- mknn[,cknn]
bestf1knn <-mf1knn[,cknn]
ciknn <- which(colMeans(miknn) == max(colMeans(miknn)))
bestiknn <- miknn[,ciknn]
bestf1iknn <- mf1iknn[,ciknn]


#MATRIZ DE COMPARACAO
f1knnm <- mean(bestf1knn)
f1iknnm <- mean(bestf1iknn)  #CERTO
f1rfm <- mean(mediaf1rf)
f1svmm <- mean(mediaf1svm)
f1knnsd <- sd(bestf1knn)
f1iknnsd <- sd(bestf1iknn)   #CERTO
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


#EAE MATRIX DE CONFUSAO                                                                                                      tempoknn,tempoiknn,temporf,temposvm   
exp<- matrix(c(f1knnm,f1iknnm,f1rfm,f1svmm,f1knnsd,f1iknnsd,f1rfsd,f1svmsd,acknnm,aciknnm,acrf,acsvm,sdknn,sdiknn,sdrf,sdsvm,tempoknn,tempoiknn,temposvmpred,temporfpred),nrow = 4,ncol = 5
             ,byrow = FALSE)
dimnames(exp) = list(c("KNN", "IKNN","RANDOM FOREST", "SVM"),c("F1-Score(Media)","F1-Score(Desvio)", "Acuracia(Media)", "Acuracia(Desvio)", "Tempo"))
print(exp)




