# Author: Jianghui Li
# Plots about Starbucks' food & drinks
# Source: https://www.kaggle.com/swoolfeek/starbucks-nutrition-with-sugar-and-etc
# Loading packages
library(ggplot2)
library(corrplot)
library(dplyr)
library(ggpubr)
library(ggrepel)
library(RColorBrewer)
library(reshape2)
library(treemap)
#loading data
starbucksDrinks<-read.csv("~/R/719/starbucks_drink.csv")
starbucksFood<-read.csv("~/R/719/starbucks_food.csv")
starbucksDrinks<-na.omit(starbucksDrinks)
starbucksFood<-na.omit(starbucksFood)
starbucksDrinks$Caffeine.mg.<-as.numeric(starbucksDrinks$Caffeine.mg.)
#hist
#hist(starbucksDrinks$Calories, xlab = "Calories", main = "Starbucks Drinks Calories Frequency",cex.sub=0.6
#     , sub = "Source: https://www.kaggle.com/swoolfeek/starbucks-nutrition-with-sugar-and-etc")
#hist(starbucksFood$Calories, xlab = "Calories", main = "Starbucks Food Calories Frequency",cex.sub=0.6
#     , sub = "Source: https://www.kaggle.com/swoolfeek/starbucks-nutrition-with-sugar-and-etc")
#hist(starbucksDrinks$Caffeine.mg., xlab = "Caffeine mg", main = "Starbucks Drinks Caffeine Frequency",cex.sub=0.6
#     , sub = "Source: https://www.kaggle.com/swoolfeek/starbucks-nutrition-with-sugar-and-etc")
#gghist
h1<-ggplot(starbucksDrinks, aes(Calories, fill = ..x..)) +  xlim(0, 700)+
        geom_histogram(show.legend = FALSE) +
        scale_fill_gradientn(colours = c("darkgreen","tan","chocolate"),
                             breaks=c(0,25,50,75,Inf),
                             guide = "colorbar") + theme_classic() + ggtitle("Calories Distribution for Drinks")
h2<-ggplot(starbucksFood, aes(Calories, fill = ..x..)) +  xlim(0, 700)+
        geom_histogram(show.legend = FALSE) +
        scale_fill_gradientn(colours = c("darkgreen", "tan","chocolate"),
                             breaks=c(0,25,50,75,Inf),
                             guide = "colorbar") + theme_classic() + ggtitle("Calories Distribution for Food")
ggarrange(h1,h2, nrow=2)

# high c
dataC <- starbucksDrinks[with(starbucksDrinks,order(-Calories)),]
dataC <- dataC[1:20,]
ggplot(data = dataC, aes(x=Sugars.g., y = Calories, label = Name))+theme_classic()+
        ggtitle("Top20 Highest Calories Drinks with Sugar in Gram")+ylim(550,650)+
        geom_point(aes(size = Calories), alpha = 0.8, color='chocolate')+
        geom_label_repel(aes(label = Name),
                         box.padding   = 0.5, 
                         point.padding = 0.5,
                         segment.color = 'darkgreen', color='black')
dataC2 <- starbucksFood[with(starbucksFood,order(-Calories)),]
dataC2 <- dataC2[1:20,]
ggplot(data = dataC2, aes(x=Portion.g., y = Calories, label = Name))+theme_classic()+
        ggtitle("Top20 Highest Calories Food with Portion per Serving")+ ylim(450,700)+
        geom_point(aes(size = Calories), alpha = 0.8, color='chocolate')+
        geom_label_repel(aes(label = Name),
                         box.padding   = 0.35, 
                         point.padding = 0.5,
                         segment.color = 'darkgreen', color='black')
dataC3 <- starbucksFood[with(starbucksFood,order(-Calories/Portion.g.)),]
dataC3 <- dataC3[1:20,]
ggplot(data = dataC3, aes(x=Portion.g., y = Calories/Portion.g., label = Name))+theme_classic()+
        ggtitle("Top20 Highest Calories per Gram Food with Portion per Serving")+
        geom_point(aes(size = Calories), alpha = 0.8, color='chocolate')+
        geom_label_repel(aes(label = Name),
                         box.padding   = 0.35, 
                         point.padding = 0.5,
                         segment.color = 'darkgreen', color='black')
#high S

dataS2 <- starbucksFood[with(starbucksFood,order(-Sodium.mg.)),]
dataS2 <- dataS2[1:20,]
ggplot(data = dataS2, aes(x=Portion.g., y = Sodium.mg., label = Name))+theme_classic()+
        ggtitle("Top20 Highest Sodium Food with Portion per Serving")+
        geom_point(aes(size = Sodium.mg.), alpha = 0.8, color='darkgreen')+
        geom_label_repel(aes(label = Name),
                         box.padding   = 0.5, 
                         point.padding = 0.5,
                         segment.color = 'darkgreen', color='black')
#high Cho
dataCh2 <- starbucksFood[with(starbucksFood,order(-Cholesterol.mg.)),]
dataCh2 <- dataCh2[1:20,]
ggplot(data = dataCh2, aes(x=Portion.g., y = Cholesterol.mg.))+theme_classic()+
        ggtitle("Top20 Highest Cholesterol Food with Portion per Serving")+
        geom_point(aes(size = Cholesterol.mg.), alpha = 0.8, color='orange')+
        geom_label_repel(aes(label = Name),
                         box.padding   = 0.5, 
                         point.padding = 0.5,
                         segment.color = 'darkgreen', color='black')
#high sug
dataSu <- starbucksDrinks[with(starbucksDrinks,order(-Sugars.g.)),]
dataSu <- dataSu[1:20,]
ggplot(data = dataSu, aes(x=Whipped.Cream, y = Sugars.g.))+theme_classic()+
        ggtitle("Top20 Highest Sugar Drinks with Fluid Ounce")+ylim(87,93)+
        geom_point(aes(size = Portion.fl.oz.), alpha = 0.8, color='grey')+
        geom_label_repel(aes(label = Name),
                         box.padding   = 0.5, 
                         point.padding = 0.5,
                         segment.color = 'darkgreen', color='black')
#
dataCa <- starbucksDrinks[with(starbucksDrinks,order(-Caffeine.mg.)),]
dataCa <- dataCa[1:20,]
ggplot(data = dataCa, aes(x=Portion.fl.oz., y = Caffeine.mg.))+theme_classic()+
        ggtitle("Top20 Highest Caffeine Drinks with Fluid Ounce")+ylim(340,500)+
        geom_point(aes(size = Caffeine.mg.), alpha = 0.8, color='grey')+
        geom_label_repel(aes(label = Name),
                         box.padding   = 0.6, 
                         point.padding = 0.5,
                         max.overlaps = 100,
                         segment.color = 'darkgreen', color='black')
#Correlation
par(mfrow=c(1,1))
col<- colorRampPalette(c("chocolate", "white", "darkgreen"))(20)
starbucksDrinksC<-subset(starbucksDrinks,select= c(Portion.fl.oz., Calories, Cholesterol.mg., Sodium.mg., Total.Fat.g., Sugars.g., Protein.g.))
num.cols <- sapply(starbucksDrinksC, is.numeric)
cor.data <- cor(starbucksDrinksC[,num.cols])
corrplot.mixed(cor.data, order = 'AOE', upper= "square", tl.cex = 0.8)
corrplot(cor.data, type="upper", order="hclust", col=col, tl.cex = 0.8)
starbucksFoodC<-subset(starbucksFood,select= c(Portion.g., Calories, Cholesterol.mg., Sodium.mg.,Total.Fat.g., Sugars.g., Protein.g.))
num.cols <- sapply(starbucksDrinksC, is.numeric)
cor.data2 <- cor(starbucksFoodC[,num.cols])
corrplot.mixed(cor.data2, order = 'AOE', upper= "square", tl.cex = 0.8)
corrplot(cor.data2, type="upper", order="hclust", col=col, tl.cex = 0.8)
#Total cholesterol levels less than 200 milligrams per deciliter (mg/dL) are considered desirable for adults(https://www.medicalnewstoday.com/articles/315900).
#Americans eat on average about 3,400 mg of sodium per day. However, the Dietary Guidelines for Americans recommends adults limit sodium intake to less than 2,300 mg per day¡ªthat's equal to about 1 teaspoon of table salt(https://www.fda.gov/food/nutrition-education-resources-materials/sodium-your-diet)!
#boxplot
#boxplot(Calories~Category, data=starbucksFood
#        , main = "Calories Distribution by Starbucks Food Category",cex.sub=0.6
#        , sub = "Source: https://www.kaggle.com/swoolfeek/starbucks-nutrition-with-sugar-and-etc")
#boxplot(Calories~Category, data=starbucksDrinks, cex.axis=0.6
#        , main = "Calories Distribution by Starbucks Drink Categroy",cex.sub=0.6
#        , sub = "Source: https://www.kaggle.com/swoolfeek/starbucks-nutrition-with-sugar-and-etc")
dat.m <- melt(starbucksDrinks,id.vars='Category', measure.vars=c('Calories','Sugars.g.','Caffeine.mg.'))
#ggplot(dat.m) +
#        geom_boxplot(aes(x=Category, y=value, color=variable))
dat.m2 <- melt(starbucksFood,id.vars='Category', measure.vars=c('Calories','Sodium.mg.','Cholesterol.mg.'))
#ggplot(dat.m2) +
#        geom_boxplot(aes(x=Category, y=value, color=variable))
ggplot(dat.m2, aes(x=Category, y=value, color=variable, fill=variable)) +
        geom_boxplot(alpha = 0.8,outlier.colour="darkgreen", outlier.shape=15,outlier.size=2) +
        labs(title="Calorie, Sodium & Cholesterol Distribution by Starbucks Food Category", x ="Food Category", y = "Calories") + theme_classic()+
        scale_fill_manual(values=c("chocolate", "darkgreen", "orange"))+ylab("Calorie (g),Sodium (mg),Cholesterol (mg)")
ggplot(dat.m, aes(x=Category, y=value, color=variable, fill=variable)) +
        geom_boxplot(alpha = 0.8,outlier.colour="darkgreen", outlier.shape=15,outlier.size=2) +
        labs(title="Calorie, Sugar & Caffeine Distribution by Starbucks Drink Category", x ="Drink Category", y = "Calories") + theme_classic()+
        scale_fill_manual(values=c("chocolate", "yellow", "grey"))+ylab("Calorie (g),Sodium (mg),Cholesterol (mg)")


######################################
G1<-aggregate(starbucksFood$Calories, list(starbucksFood$Category), FUN=mean)
barplot(G1$x,names.arg = G1$Group.1, col=c("dark green","light grey")
        , xlab = "Types of Food"
        , ylab = "Average Calories"
        , main = "Starbucks Food Average Calories by Food Category",cex.sub=0.6
        , sub = "Source: https://www.kaggle.com/swoolfeek/starbucks-nutrition-with-sugar-and-etc")
G2<-aggregate(starbucksFood$Portion.g., list(starbucksFood$Category), FUN=mean)

G1$Calories_per_Gram<-G1$x/G2$x
barplot(G1$Calories_per_Gram,names.arg = G1$Group.1, col=c("dark green","light grey")
        , xlab = "Types of Food"
        , ylab = "Average Calories"
        , main = "Starbucks Food Average Calories/Gram by Food Category",cex.sub=0.6
        , sub = "Source: https://www.kaggle.com/swoolfeek/starbucks-nutrition-with-sugar-and-etc")

G3<-aggregate(starbucksDrinks$Calories, list(starbucksDrinks$Category), FUN=mean)
barplot(G3$x, names.arg = G3$Group.1, cex.names=0.6,cex.sub=0.6
        , col=c("dark green","chocolate")
        , xlab = "Types of Drinks"
        , ylab = "Average Calories"
        , main = "Starbucks Drinks Average Calories by Food Category"
        , sub = "Source: https://www.kaggle.com/swoolfeek/starbucks-nutrition-with-sugar-and-etc")
#
f1<-ggplot(starbucksFood,aes(x=Calories,y=Cholesterol.mg.)) + theme_classic()+
        geom_point(color="chocolate")+ggtitle("Food - Calories vs Cholesterol")+
        geom_smooth(method='lm',color="darkgreen")
f2<-ggplot(starbucksFood,aes(x=Calories,y=Sodium.mg.)) + theme_classic()+
        geom_point(color="chocolate")+ggtitle("Food - Calories vs Sodium")+
        geom_smooth(method='lm',color="darkgreen")
f3<-ggplot(starbucksFood,aes(x=Cholesterol.mg.,y=Sodium.mg.)) + theme_classic()+
        geom_point(color="chocolate")+ggtitle("Food - Cholesterol vs Sodium")+
        geom_smooth(method='lm',color="darkgreen")
ggarrange(f1,f2,f3, ncol=3)
d1<-ggplot(starbucksDrinks,aes(x=Calories,y=Cholesterol.mg.)) + theme_classic()+
        geom_point(color="chocolate")+ggtitle("Drinks - Calories vs Cholesterol")+
        geom_smooth(method='lm',color="darkgreen")
d2<-ggplot(starbucksDrinks,aes(x=Calories,y=Sodium.mg.)) + theme_classic()+
        geom_point(color="chocolate")+ggtitle("Drinks - Calories vs Sodium")+
        geom_smooth(method='lm',color="darkgreen")
d3<-ggplot(starbucksDrinks,aes(x=Cholesterol.mg.,y=Sodium.mg.)) + theme_classic()+
        geom_point(color="chocolate")+ggtitle("Drinks - Cholesterol vs Sodium")+
        geom_smooth(method='lm',color="darkgreen")
ggarrange(d1,d2,d3, ncol=3)

#tree

G4<-aggregate(starbucksFood$Category, list(starbucksFood$Category), FUN=length)
treemap(G4,
        index="Group.1",
        vSize="x",
        type="index",
        title="Food Types",
        fontsize.title = 18,
        fontsize.labels = 18
)
G5<-aggregate(starbucksDrinks$Category, list(starbucksDrinks$Category), FUN=length)
treemap(G5,
        index="Group.1",
        vSize="x",
        type="index",
        title="Drink Types",
        fontsize.title = 18,
        fontsize.labels = 18
)
