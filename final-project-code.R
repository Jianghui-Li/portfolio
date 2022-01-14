# IST 707 Final Project
# Loading Packages
library(arules)
library(arulesViz)
library(ggplot2)
library(corrplot)
library(ggpubr)
require(caret)
library(tidyverse)    
library(kernlab)      
library(e1071)        
library(RColorBrewer) 
require(rpart)
require(dplyr)
require(stringr)
require(randomForest)
library(rsample)
library(cluster)
library(factoextra)
library(gridExtra)
library(dendextend)
library(pROC)
# Visualization
pokemon<-read.csv("~/R/707/project/All_Pokemon.csv")
dim(pokemon)
summary(pokemon)
pokemon$Generation<-as.character(pokemon$Generation)


pokemon$LegendaryChar<-as.character(pokemon$Legendary)
g2<-ggplot(pokemon, aes(Legendary,fill = LegendaryChar)) + geom_text(stat='count', aes(label=..count..), vjust=-0.1) +
  geom_bar(show.legend = FALSE) + theme_classic() + labs(title = "Legendary Pokemon Count")

g1<-ggplot(pokemon, aes(x=Type.1,fill = LegendaryChar))+geom_bar(show.legend = TRUE) + theme_classic() +
  geom_text(stat='count', aes(label=..count..), vjust=-1) + labs(x="Types", title = "Pokemon Count by Types")
ggarrange(g2,g1, nrow=2)

t.hp<-ggplot(pokemon, aes(HP, fill = LegendaryChar)) +
  geom_histogram(show.legend = TRUE)+ theme_classic() + ggtitle("HP Distribution")
t.atk<-ggplot(pokemon, aes(Att, fill = LegendaryChar)) +
  geom_histogram(show.legend = TRUE)+ theme_classic() + ggtitle("Attack Distribution")
t.def<-ggplot(pokemon, aes(Def, fill = LegendaryChar)) +
  geom_histogram(show.legend = TRUE)+ theme_classic() + ggtitle("Defence Distribution")
t.spa<-ggplot(pokemon, aes(Spa, fill = LegendaryChar)) +
  geom_histogram(show.legend = TRUE)+ theme_classic() + ggtitle("Special Attack Distribution")
t.spd<-ggplot(pokemon, aes(Spd, fill = LegendaryChar)) +
  geom_histogram(show.legend = TRUE)+theme_classic() + ggtitle("Special Defence Distribution")
t.spe<-ggplot(pokemon, aes(Spe, fill = LegendaryChar)) +
  geom_histogram(show.legend = TRUE)+theme_classic() + ggtitle("Speed Distribution")
ggarrange(t.hp, t.atk,t.def,t.spa,t.spd,t.spe, ncol=2, nrow = 3)
t.bst<-ggplot(pokemon, aes(BST, fill = LegendaryChar)) +
  geom_histogram(show.legend = TRUE)+theme_classic() + ggtitle("Base Stats Distribution")
t.bst
######
pokemonData<-subset(pokemon,select= c(Type.1, Type.2, BST, Catch.Rate, Generation, Legendary, Height, Weight))


#Correlation
pokemon$Generation<-as.numeric(pokemon$Generation)
pokemonData1<-subset(pokemon,select= c(BST, Generation, Final.Evolution, Catch.Rate, Legendary, Height, Weight))
num.cols <- sapply(pokemonData1, is.numeric)
cor.data <- cor(pokemonData1[,num.cols])
par(mfrow=c(1,1))
corrplot.mixed(cor.data,upper = 'square')
############

bins <- 3
max_BST<-max(pokemonData$BST)
min_BST<-min(pokemonData$BST)
width<-(max_BST - min_BST)/bins;
min_BST+width
pokemonData$BST <- cut(pokemonData$BST, breaks=c(174.9, 376.6667, 578.3333, 780)
                 ,labels=c("low","mediocre","high"))
max_Catch.Rate<-max(pokemonData$Catch.Rate)
min_Catch.Rate<-min(pokemonData$Catch.Rate)
width<-(max_Catch.Rate - min_Catch.Rate)/bins;
min_Catch.Rate+width+width
pokemonData$Catch.Rate <- cut(pokemonData$Catch.Rate, breaks=c(2.9, 87, 171, 255)
                       ,labels=c("low","average","high"))
max_Height<-max(pokemonData$Height)
min_Height<-min(pokemonData$Height)
quantile(pokemonData$Height, probs = c(0.33, 0.67))
pokemonData$Height <- cut(pokemonData$Height, breaks=c(0.09, 0.7, 1.4, 20)
                              ,labels=c("short","average","tall"))
max_Weight<-max(pokemonData$Weight)
min_Weight<-min(pokemonData$Weight)
quantile(pokemonData$Weight, probs = c(0.33, 0.67))
pokemonData$Weight <- cut(pokemonData$Weight, breaks=c(0.09, 14.346, 53, 999.9)
                          ,labels=c("light","average","heavy"))
pokemonData$Type.1<-as.factor(pokemonData$Type.1)
pokemonData$Type.2<-as.factor(pokemonData$Type.2)
pokemonData$BST<-as.factor(pokemonData$BST)
pokemonData$Catch.Rate<-as.factor(pokemonData$Catch.Rate)
pokemonData$Generation<-as.factor(pokemonData$Generation)
pokemonData$Legendary<-as.factor(pokemonData$Legendary)
pokemonData$Height<-as.factor(pokemonData$Height)
pokemonData$Weight<-as.factor(pokemonData$Weight)
bG<-ggplot(pokemon, aes(Generation, fill = LegendaryChar,)) +
  geom_bar(show.legend = TRUE) + theme_classic() + labs(x="Types", title = "Pokemon Count by Generation")
bE<-ggplot(pokemon, aes(Experience.type, fill = LegendaryChar,)) +
  geom_bar(show.legend = TRUE) + theme_classic() + labs(x="Types", title = "Pokemon Count by Experience Gaining Speed")
bC<-ggplot(pokemonData, aes(Catch.Rate, fill = Legendary,)) +
  geom_bar(show.legend = TRUE) + theme_classic() + labs(x="Types", title = "Pokemon Count by Catch Rate")
ggarrange(bG, bE, bC, nrow=3)
bW<-ggplot(pokemonData, aes(Weight, fill = Legendary,)) +
  geom_bar(show.legend = TRUE) + theme_classic() + labs(x="Types", title = "Pokemon Count by Weight")
bH<-ggplot(pokemonData, aes(Height, fill = Legendary,)) +
  geom_bar(show.legend = TRUE) + theme_classic() + labs(x="Types", title = "Pokemon Count by Height")
ggarrange(bW, bH, nrow=2)


rules <- apriori(pokemonData, parameter=list(support=0.5, confidence=0.8, minlen=3))
inspect(rules)
rulesLeg <- apriori(pokemonData, parameter=list(support=0.05, confidence=0.60, minlen=3),
                    appearance=list(default="lhs",rhs=("Height=tall")))
inspect(rulesLeg)
plot(rulesLeg,method = "graph")
rulesLeg2 <- apriori(pokemonData, parameter=list(support=0.013, confidence=0.9, minlen=3),
                    appearance=list(default="lhs",rhs=("Legendary=1")))
inspect(rulesLeg2)
plot(rulesLeg2,method = "graph")
rulesLeg3 <- apriori(pokemonData, parameter=list(support=0.01, confidence=0.3, minlen=3),
                     appearance=list(default="lhs",rhs=("Type.1=Psychic")))
inspect(rulesLeg3)
plot(rulesLeg3,method = "graph")
#Decision Tree
pokemonData2<-subset(pokemon,select= c(HP, Att, Def, Spa, Spd, Spe, BST))
pokemonData2 <- scale(pokemonData2)
d <- dist(pokemonData2, method = "euclidean")
hc1 <- hclust(d, method = "complete" )
plot(hc1, cex = 0.6, hang = -1, labels = pokemon$Name)
hc2 <- agnes(pokemonData2, method = "complete")
hc2$ac

hc3 <- agnes(pokemonData2, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes", labels = pokemon$Name)
hc3$ac
rect.hclust(hc3, k = 3, border = 2:5)

hc_a <- agnes(pokemonData2, method = "ward")
hc_a_label <- cutree(as.hclust(hc_a), k = 3)

fviz_nbclust(pokemonData2, FUN = hcut, method = "wss") #It indicates that K value is either 2 or 3s

fviz_nbclust(pokemonData2, FUN = hcut, method = "silhouette") #It indicates that K value should be 3
#generation
hc_final1 <- agnes(pokemonData2, method = "ward")
hc_final_label1 <- cutree(as.hclust(hc_final1), k = 3)
model<-kmeans(pokemonData2, centers = 8)
fviz_cluster(model, data = pokemonData2, cluster=pokemon$Generation, main="Generation")
#Legendary
hc_final2 <- agnes(pokemonData2, method = "ward")
hc_final_label2 <- cutree(as.hclust(hc_final2), k = 3)
model<-kmeans(pokemonData2, centers = 3)
kmCluster<-model$cluster
row.names(pokemonData2)<-paste(pokemon$Legendary, 1:dim(pokemon)[1],sep = "_")
fviz_cluster(list(data = pokemonData2, cluster=kmCluster, main="Legendary"),main="Legendary Cluster")

#Type
hc_final <- agnes(pokemonData2, method = "ward")
hc_final_label <- cutree(as.hclust(hc_final), k = 3)
model<-kmeans(pokemonData2, centers = 19)
fviz_cluster(model, data = pokemonData2, cluster=pokemon$Type.1, main="Type")
fviz_cluster(object=list(data = pokemonData2, cluster = pokemon$Type.1),geom = "point",repel=T,show.clust.cent=T,main="clusters of df")
#FinalEvo
hc_final <- agnes(pokemonData2, method = "ward")
hc_final_label <- cutree(as.hclust(hc_final), k = 3)
model<-kmeans(pokemonData2, centers = 2)
fviz_cluster(model, data = pokemonData2, cluster=pokemon$Fianl.Evoluation, main="Final Evoluation")
#

# SVM Models (Legendary)

###############

myData <- subset(pokemon,select= c(HP, Att, Def, Spa, Spd, Spe, BST, Legendary))
myData$Legendary = as.factor(myData$Legendary)

dt = sort(sample(nrow(myData), nrow(myData)*.7)) # Split dataset into train 70% train and 30% test.
pokemon_train<-myData[dt,]
pokemon_test<-myData[-dt,]
set.seed(123)
### SVM with Linear Kernel
search_grid = expand.grid(C = seq(0, 2, length = 20))

# set up 3-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 3
)

# more advanced option, run 5 fold cross validation 10 times
train_control_adv <- trainControl(
  method = "repeatedcv", 
  number = 5,
  repeats = 10
)


svm.m1 = train(Legendary ~., data = pokemon_train, 
               method = "svmLinear", 
               trControl = train_control_adv,
               tuneGrid = search_grid)

# top 5 modesl
svm.m1$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

# results for best model
confusionMatrix(svm.m1)

pred <- predict(svm.m1, newdata = pokemon_test)
###################
search_grid = expand.grid(sigma = seq(0.1, 2, length=20),
                          C = seq(0.1, 2, length = 20))

# set up 3-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 3
)

# more advanced option, run 5 fold cross validation 10 times
train_control_adv <- trainControl(
  method = "repeatedcv", 
  number = 5,
  repeats = 10
)


svm.m1 = train(Legendary ~., data = pokemon_train, 
               method = "svmRadial", 
               trControl = train_control_adv,
               tuneGrid = search_grid)

# top 5 modesl
svm.m1$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

# results for best model
confusionMatrix(svm.m1)

pred <- predict(svm.m1, newdata = pokemon_test)
################
search_grid = expand.grid(degree=c(1,2,3),
                          scale = c(0.001, 0.01, 0.1, 1.0),
                          C = seq(0.1, 2, length = 20))

# set up 3-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 3
)

# more advanced option, run 5 fold cross validation 10 times
train_control_adv <- trainControl(
  method = "repeatedcv", 
  number = 5,
  repeats = 10
)


svm.m1 = train(Legendary ~., data = pokemon_train, 
               method = "svmPoly", 
               trControl = train_control_adv,
               tuneGrid = search_grid)

# top 5 modesl
svm.m1$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

# results for best model
confusionMatrix(svm.m1)

pred <- predict(svm.m1, newdata = pokemon_test)
## compute AUC and plot ROC curve


pred_numeric <- predict(svm.m1, newdata = pokemon_test, type="prob")

head(pred_numeric)

# plot ROC and get AUC
roc <- roc(predictor=pred_numeric$"1",
           response=pokemon_test$Legendary,
           levels=rev(levels(pokemon_test$Legendary)))

roc$auc
#Area under the curve
plot(roc,main="ROC")
#knn
search_grid = expand.grid(k = c(5, 7, 9, 11, 13, 15, 17, 19, 21, 23, 25))

# set up 3-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 3
)

# more advanced option, run 5 fold cross validation 10 times
train_control_adv <- trainControl(
  method = "repeatedcv", 
  number = 5,
  repeats = 10
)

# train model
knn <- train(Legendary ~ .,
             data = pokemon_train,
             method = "knn",
             trControl = train_control_adv,
             tuneGrid = search_grid
)
# top 5 modesl
knn$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

# results for best model
confusionMatrix(knn)

pred <- predict(knn, newdata = pokemon_test)
#AUC
#library(pROC)

pred_numeric = predict(knn, newdata = pokemon_test, type="prob")

head(pred_numeric)

# plot ROC and get AUC
roc <- roc(predictor=pred_numeric$"1",
           response=pokemon_test$Legendary,
           levels=rev(levels(pokemon_test$Legendary)))

roc$auc
#Area under the curve
plot(roc,main="ROC")
#rf
search_grid = expand.grid(.mtry = (1:5)) 

# set up 3-fold cross validation procedure
train_control <- trainControl(
  method = "cv", 
  number = 3
)

# more advanced option, run 5 fold cross validation 10 times
train_control_adv <- trainControl(
  method = "repeatedcv", 
  number = 5,
  repeats = 10
)

rf.m1 = train(Legendary ~., data = pokemon_train, 
              method = "rf",
              metric = 'Accuracy',
              trControl = train_control_adv,
              tuneGrid = search_grid)

# top 5 modesl
rf.m1$results %>% 
  top_n(5, wt = Accuracy) %>%
  arrange(desc(Accuracy))

# results for best model
confusionMatrix(rf.m1)

pred <- predict(rf.m1, newdata = pokemon_test)
## compute AUC and plot ROC curve
#install.packages("pROC")
#library(pROC)

pred_numeric = predict(rf.m1, newdata = pokemon_test, type="prob")

head(pred_numeric)

# plot ROC and get AUC
roc <- roc(predictor=pred_numeric$"1",
           response=pokemon_test$Legendary,
           levels=rev(levels(pokemon_test$Legendary)))

roc$auc
#Area under the curve
plot(roc,main="ROC")

