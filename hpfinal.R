hpTerm1 <- read.csv(file.choose(), header= TRUE)
hpTermPron <- read.csv(file.choose(), header= TRUE)
View(hpTerm1)
library(ggplot2)
dfhpcolor<-data.frame(hpTermColor$RonRelFreq,hpTermColor$Term)
ggplot(dfhpcolor, aes(y=dfhpcolor$hpTermColor.Term,x=dfhpcolor$hpTermColor.RonRelFreq))+geom_point(size=3)
ggplot(iris, aes(Petal.Length,Petal.Width, color=Species))+geom_point(size=4)
data_Matrix<- matrix(hpTermColor[,8:15])
View(data_Matrix)
data_Matrix[1]

p <- ggplot(nba.m, aes(variable, Name)) + geom_tile(aes(fill = rescale),
    +     colour = "white") + scale_fill_gradient(low = "white",
    +     high = "steelblue")

help(ddply)

install.packages("randomForest")
library(randomForest)

set.seed(100)
train <- sample(nrow(hpTerm1), 0.7*nrow(hpTerm1), replace = FALSE)
TrainSet <- hpTerm1[train,]
ValidSet <- hpTerm1[-train,]
summary(TrainSet)
summary(ValidSet)

hpTermgenR_Tree <- randomForest(Type ~ ., data = TrainSet,ntree=500, mtry=3, importance = TRUE)
varImpPlot(hpTermgenR_Tree, sort=TRUE, scale=FALSE)
hpTermgenR_Tree

library(party)
output.tree <- ctree(
  Type ~ ., 
  data = hpTermPron)

plot(output.tree)
help(ctree)


set.seed(100)
trainPron <- sample(nrow(hpTermPron), 0.7*nrow(hpTermPron), replace = FALSE)
TrainSetPron <- hpTermPron[trainPron,]
ValidSet <- hpTermPron[-trainPron,]
summary(TrainSet)
summary(ValidSet)

hpTermgenPron <- randomForest(Type ~ ., data = TrainSetPron,ntree=1000, mtry=6, importance = TRUE)
varImpPlot(hpTermgenPron, sort=TRUE, scale=FALSE, main= "pronoun use of SUPCs by gender")
hpTermgenPron
library(rpart)
library(rpart.plot)
rp <- rpart(Type ~ ., data = hpTermPron,  method="anova")
rpart.plot(rp,box.palette="blue")
text(rp, use.n=TRUE, all=TRUE, cex=.8)
help(rpart)

dist(hpTermPron, method = "euclidean") # distance matrix

install.packages("factoextra")

library(factoextra)
hplong <- hpTermPron[1:23, 1:9]
res.pca <- prcomp(hplong, Scale=TRUE)
fviz_eig(res.pca)
View(decathlon2)
View(hpTermPron)
head(decathlon2)
decathlon2[,1]
hpTermPron[,1]
head(hpTermPron)
hpTermPron2<- hpTermPron[,-1]
rownames(hpTermPron2)<- hpTermPron[,1]
install.packages("tidyverse")
install.packages("broom")
library(tidyverse)
library(dplyr)
rep <-hpTermPron %>% remove_rownames %>% column_to_rownames(var="names")
View(rep)
lost <- data.frame(hpTermPron)
lot <- hpTermPron$X
hpTermPron2 <- hpTermPron[,-1]
rownames(hpTermPron2)<- NULL
attr(hpTermPron2,"row.names")
rownames(hpTermPron2)<- lot
hpTermPron2

set.seed(10111)
x = matrix(rnorm(40), 20, 2)
y = rep(c(-1, 1), c(10, 10))
x[y == 1,] = x[y == 1,] + 1
plot(x, col = y + 3, pch = 19)
install.packages("e1071")
library(e1071)
svmfit <- svm(Type ~ ., data= hpTermPron, kernel = "linear", cost = 10, scale = FALSE)
print(svmfit)
plot(svmfit, hpTermPron)

# K-Means Clustering with 5 clusters
View(hpTermPron)
j<- na.rm(i)
k<- nan.rm(j)
fit <- kmeans(hpTermPron[,-1:-2], 5)
is.nan(hpTermPron)


help(kmeans)
# Cluster Plot against 1st 2 principal components

# vary parameters for most readable graph
library(cluster)
clusplot(hpTermPron, fit$cluster, color=TRUE, shade=TRUE,
         labels=4, lines=0)
library(fpc)
plotcluster(mydata, fit$cluster)



