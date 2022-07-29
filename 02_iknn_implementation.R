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
        
       
      }
      
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
for (i in 1:29){
  plotagrafico(media,60)
  media <- media-0.25
  pltlist[[i]] = a
  
}

listaelk <- listaelk[1:64]
listaelki <- listaelki[1:64]

miknn <- matrix(mediaiknn,i,ncol = 64, byrow = TRUE)
colnames(miknn) <- paste(listaelk,listaelki)
row.names(miknn) <- c(1:i)
mf1iknn <- matrix(mediaf1iknn,nrow = i,ncol = 64, byrow = TRUE)
colnames(mf1iknn) <- paste(listaelk,listaelki)
row.names(mf1iknn) <- c(1:i)

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