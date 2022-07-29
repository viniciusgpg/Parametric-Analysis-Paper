library(caret)
library(class)
library(cvTools)
library(e1071)
#library(iknnr)
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
    acuraciaknn <- NULL
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
        
        
      
        
      }
      
    
    mediaknn <- c(mediaknn,(mean(acuraciaknn)))
    mediaf1knn <- c(mediaf1knn,mean(mean(f1knn)))
    assign("mediaknn", mediaknn, envir = globalenv())
    assign("mediaf1knn", mediaf1knn, envir = globalenv())
    }
  
  
  vetormedia <- c(vetormedia,media2-media1)
  assign("vetormedia", vetormedia,envir = globalenv())
  
  
  
}
acuraciaknn <- NULL
acuraciaiknn <- NULL
mediaknn <- NULL
mediaf1knn <- NULL
f1knn <- NULL



mediafinal <- NULL
vetormedia <- NULL
vetoracuracia <- NULL
vetorf1scorerf <- NULL
vetorf1scoresvm <- NULL
tempobestknn = tempobestrf = tempobestsvm <- 0

#vetorinn <- NULL
vetorf1 <- NULL
eaex <- NULL
b<- NULL
b <- 0
pltlist <- list()
pltlist[]
media = 60
for (i in 1:29){
  plotagrafico(media,60)
  media <- media-0.25
  pltlist[[i]] = a
  
}
grid.arrange(grobs = pltlist[1:10], nrow = 5, ncol = 2, top = "Exemplos de conjuntos de dados sobrepostos ")
grid.arrange(grobs = pltlist[11:20], nrow = 5, ncol = 2,top = "Exemplos de conjuntos de dados levemente sobrepostos ")
grid.arrange(grobs = pltlist[21:29], nrow = 5, ncol = 2,top = "Exemplos de conjuntos de dados nao sobrepostos ")
mknn <- matrix(mediaknn,nrow = i,ncol = 8, byrow = TRUE)
colnames(mknn) <- c("K = 1", "K = 3", "K = 5", "K = 7", "K = 9", "K = 11", "K = 13", "K = 15")
row.names(mknn) <- c(1:i)

mf1knn <- matrix(mediaf1knn,nrow = i,ncol = 8, byrow = TRUE)
colnames(mf1knn) <- c("K = 1", "K = 3", "K = 5", "K = 7", "K = 9", "K = 11", "K = 13", "K = 15")
row.names(mf1knn) <- c(1:i)


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




