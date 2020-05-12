# Activate dataset
dataset = read.csv('desktop/Player_Stats.csv')
dataset = dataset[-1]
dataset = dataset[-3:-4]
dataset = na.omit(dataset)
dataset$G = dataset$G > 30
dataset = dataset[dataset$G == TRUE,]
dataset = dataset[-3:-4]
library(plyr)
library(Matrix)
library(matrixStats)
library(pca3d)

# preprocessing
library(caTools)
set.seed(123)
split = sample.split(dataset$Pos, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
training_set[-1:-2] = scale(training_set[-1:-2])
test_set[-1:-2] = scale(test_set[-1:-2])

#LDA decrease features
library(MASS)
lda = lda(formula = Pos ~ MP+FG+FGA+FG.+X3P+X3PA+X3P.+X2P+X2PA+X2P.+eFG.+FT+FTA+FT.+ORB+DRB+TRB+AST+STL+BLK+TOV+PF+PTS, data = training_set)
training_set = as.data.frame(predict(lda, training_set))
training_set = training_set[c(12:20,1)]
test_set = as.data.frame(predict(lda, test_set))
test_set = test_set[c(12:20,1)]

#used svm classifier to see if the linear discriminant analysis worked for predicting the class of each player
library(e1071)
classifier = svm(formula = class~., data = training_set, type = 'C-classification',
                 kernel = 'linear')
y_pred = predict(classifier, newdata = test_set[,-10])
cm = table(test_set[,10], y_pred)

#applying the training_set to the entire dataset
training_set = dataset
training_set[-1:-2] = scale(training_set[-1:-2])
library(MASS)
lda = lda(formula = Pos ~ FG+FGA+FG.+X3P+X3PA+X3P.+X2P+X2PA+X2P.+eFG.+FT+FTA+FT.+ORB+DRB+TRB+AST+STL+BLK+TOV+PF+PTS, data = training_set)
training_set = as.data.frame(predict(lda, training_set))
training_set = training_set[c(12:20,1)]


#using k-means clustering to create 8 different groups
set.seed(29)
kmeans <- kmeans(training_set[-10],8, iter.max = 500, nstart = 10)
kmeans[1]

library(cluster)
clusplot(training_set[-10], kmeans$cluster, lines = 0, shade = TRUE, color = TRUE, labels = 2, plotchar = FALSE,
         span = TRUE, main = paste('Clusters of clients'), xlab = 'x', ylab = 'y')

new_pos = as.data.frame(kmeans[1])
training_set$new_pos = new_pos


#using principal component analysis to indicate the most important feature that defines each cluster 
# And to reduce the features to 2 so we can see the groups of players on a scatter plot
library(caret)
library(e1071)
training_set = training_set[-10]
test_set = test_set[-10]
training_set$new_pos = as.numeric(unlist(training_set$new_pos))
is.numeric(training_set$new_pos)
pca = preProcess(x = training_set[-10] , method = 'pca', pcaComp = 2)
training_set = predict(pca, training_set)
training_set = training_set[c(2,3,1)]
players = dataset$Player
training_set$Player = players
training_set = training_set[c(4,1,2,3)]

#visualizing the training_set

library(ggplot2)
u <-ggplot(data = training_set, aes(x = PC1, y= PC2, colour = as.factor(new_pos), label = Player)) + geom_point(fill = NA)
u +geom_text(aes(label= Player), hjust = 0.25, vjust = 1.3, size = 4)

cluster1 = training_set[training_set$new_pos == 1,]
cluster2 = training_set[training_set$new_pos == 2,]
cluster3 = training_set[training_set$new_pos == 3,]
cluster4 = training_set[training_set$new_pos == 4,]
cluster5 = training_set[training_set$new_pos == 5,]
cluster6 = training_set[training_set$new_pos == 6,]
cluster7 = training_set[training_set$new_pos == 7,]
cluster8 = training_set[training_set$new_pos == 8,]

#install.packages('ggrepel')
library(ggrepel)
set.seed(42)

#cluster1
data1 = 0

for (i in cluster1$Player){
  data1 = rbind(data1,dataset[dataset$Player == i,])
  data1 = na.omit(data1)
}
data1$group = 'Stationary Bigs'
summary(data1)  

u <-ggplot(data = cluster1, aes(x = PC1, y= PC2, label = Player)) + geom_point(colour = 'red', fill = NA , size = 3)

w <- u + geom_text_repel(size = 4) + theme_minimal()

w + xlab('Dimension 1') + ylab('Dimension 2') + ggtitle('Stationary Bigs') +
  theme(plot.title = element_text(colour = 'red',face = 'bold', size =30, family ="Courier"))

#cluster2
data2 = 0

for (i in cluster2$Player){
  data2 = rbind(data2,dataset[dataset$Player == i,])
  data2 = na.omit(data2)
}
data2$group = 'Floor Generals'
summary(data2)  

u <-ggplot(data = cluster2, aes(x = PC1, y= PC2, label = Player)) + geom_point(colour = 'red', fill = NA , size = 3)

w <- u + geom_text_repel(size = 4) + theme_minimal()

w + xlab('Dimension 1') + ylab('Dimension 2') + ggtitle('Floor Generals') +
  theme(plot.title = element_text(colour = 'red',face = 'bold', size =30, family ="Courier"))

#cluster3
data3 = 0

for (i in cluster3$Player){
  data3 = rbind(data3,dataset[dataset$Player == i,])
  data3 = na.omit(data3)
}
data3$group = 'Role Players'
summary(data3)  

u <-ggplot(data = cluster3, aes(x = PC1, y= PC2, label = Player)) + geom_point(colour = 'red', fill = NA , size = 3)

w <- u + geom_text_repel(size = 4) + theme_minimal()

w + xlab('Dimension 1') + ylab('Dimension 2') + ggtitle('Role Players') +
  theme(plot.title = element_text(colour = 'red',face = 'bold', size =30, family ="Courier"))

#cluster4
data4 = 0

for (i in cluster4$Player){
  data4 = rbind(data4,dataset[dataset$Player == i,])
  data4 = na.omit(data4)
}
data4$group = 'Mobile Bigs'
summary(data4)  

u <-ggplot(data = cluster4, aes(x = PC1, y= PC2, label = Player)) + geom_point(colour = 'red', fill = NA , size = 3)

w <- u + geom_text_repel(size = 4) + theme_minimal()

w + xlab('Dimension 1') + ylab('Dimension 2') + ggtitle('Mobile Bigs') +
  theme(plot.title = element_text(colour = 'red',face = 'bold', size =30, family ="Courier"))

#cluster5
data5 = 0

for (i in cluster5$Player){
  data5 = rbind(data5,dataset[dataset$Player == i,])
  data5 = na.omit(data5)
}
data5$group = 'Designated Scorers'
summary(data5)  

u <-ggplot(data = cluster5, aes(x = PC1, y= PC2, label = Player)) + geom_point(colour = 'red', fill = NA , size = 3)

w <- u + geom_text_repel(size = 4) + theme_minimal()

w + xlab('Dimension 1') + ylab('Dimension 2') + ggtitle('Designated Scorers') +
  theme(plot.title = element_text(colour = 'red',face = 'bold', size =30, family ="Courier"))

#cluster6
data6 = 0

for (i in cluster6$Player){
  data6 = rbind(data6,dataset[dataset$Player == i,])
  data6 = na.omit(data6)
}
data6$group = 'Combo Players'
summary(data6)  

u <-ggplot(data = cluster6, aes(x = PC1, y= PC2, label = Player)) + geom_point(colour = 'red', fill = NA , size = 3)

w <- u + geom_text_repel(size = 4) + theme_minimal()

w + xlab('Dimension 1') + ylab('Dimension 2') + ggtitle('Combo Players') +
  theme(plot.title = element_text(colour = 'red',face = 'bold', size =30, family ="Courier"))

#cluster7
data7 = 0

for (i in cluster7$Player){
  data7 = rbind(data7,dataset[dataset$Player == i,])
  data7 = na.omit(data7)
}
data7$group = 'Versatile Forwards'
summary(data7)  

u <-ggplot(data = cluster7, aes(x = PC1, y= PC2, label = Player)) + geom_point(colour = 'red', fill = NA , size = 3)

w <- u + geom_text_repel(size = 4) + theme_minimal()

w + xlab('Dimension 1') + ylab('Dimension 2') + ggtitle('Versatile Forwards') +
  theme(plot.title = element_text(colour = 'red',face = 'bold', size =30, family ="Courier"))

#cluster8
data8 = 0

for (i in cluster8$Player){
  data8 = rbind(data8,dataset[dataset$Player == i,])
  data8 = na.omit(data8)
}
data8$group = '3 Point Specialists'
summary(data8)  

u <-ggplot(data = cluster8, aes(x = PC1, y= PC2, label = Player)) + geom_point(colour = 'red', fill = NA , size = 3)

w <- u + geom_text_repel(size = 4) + theme_minimal()

w + xlab('Dimension 1') + ylab('Dimension 2') + ggtitle('3 Point Specialist') +
  theme(plot.title = element_text(colour = 'red',face = 'bold', size =30, family ="Courier"))


#we bind our eight loops in one dataset
a=read.csv('desktop/official_total_data.csv')
b=read.csv('desktop/adv_data.csv')
#we bind advanced data along with it and form win shares, PER, and usage rate
adv_data=cbind(b,a)
library(ggplot2)
u <-ggplot(data = adv_data, aes(x = group, y=WS, fill = group)) + geom_bar(stat = 'identity', width = 0.7) +theme_minimal() + xlab('')
v <-u + geom_text(aes(label=WS), vjust=1.6, color="white", size=4.5) + scale_fill_manual(values = c('cyan3','blue','deeppink2', 'darkorange', 'deepskyblue2','darkorchid','red','darkolivegreen3'))
v + ggtitle('Win Shares')+ theme(axis.title.x = element_text(size = 15),
                                 axis.title.y = element_text( size = 15),
                                 legend.title = element_text(size =12),
                                 legend.text = element_text(size = 12),
                                 
                                 legend.justification = c(1,1),
                                 plot.title = element_text(size =35, family ="Times New Roman"))

u <-ggplot(data = adv_data, aes(x = group, y=PER, fill = group)) + geom_bar(stat = 'identity', width = 0.7) +theme_minimal() + xlab('')
v <-u + geom_text(aes(label=PER), vjust=1.6, color="white", size=4.5) + scale_fill_manual(values = c('cyan3','blue','deeppink2', 'darkorange', 'deepskyblue2','darkorchid','red','darkolivegreen3'))
v + ggtitle('Player Efficiency Rating')+ theme(axis.title.x = element_text(size = 15),
                                 axis.title.y = element_text( size = 15),
                                 legend.title = element_text(size =12),
                                 legend.text = element_text(size = 12),
                                 
                                 legend.justification = c(1,1),
                                 plot.title = element_text(size =35, family ="Times New Roman"))

u <-ggplot(data = adv_data, aes(x = group, y=USG., fill = group)) + geom_bar(stat = 'identity', width = 0.7) +theme_minimal() + xlab('')
v <-u + geom_text(aes(label=USG.), vjust=1.6, color="white", size=4.5) + scale_fill_manual(values = c('cyan3','blue','deeppink2', 'darkorange', 'deepskyblue2','darkorchid','red','darkolivegreen3'))
v + ggtitle('Usage Rate')+ theme(axis.title.x = element_text(size = 15),
                                 axis.title.y = element_text( size = 15),
                                 legend.title = element_text(size =12),
                                 legend.text = element_text(size = 12),
                                 
                                 legend.justification = c(1,1),
                                 plot.title = element_text(size =35, family ="Times New Roman"))

#pca biplots
c=rbind(data1,data4)
c$Player<-NULL
c$Pos<-NULL
c$group[c$group == "Stationary Bigs"] <- "1"
c$group[c$group == "Mobile Bigs"] <- "2"
c$group<-as.numeric(c$group)
par(mar=c(1,1,1,1))
library(pca3d)
Mpca<-prcomp(c,center = TRUE,scale. = TRUE)
pca3d(Mpca, group = c$group, legend = "topright", biplot = TRUE, biplot.vars = 2)


d=rbind(data2,data7)
d$Player<-NULL
d$Pos<-NULL
d$group[d$group == "Floor Generals"] <- "1"
d$group[d$group == "Versatile Forwards"] <- "2"
d$group<-as.numeric(d$group)
par(mar=c(1,1,1,1))
library(pca3d)
Mpca<-prcomp(d,center = TRUE,scale. = TRUE)
pca3d(Mpca, group = d$group, legend = "topright", biplot = TRUE, biplot.vars = 2)

e=rbind(data3,data6)
e$Player<-NULL
e$Pos<-NULL
e$group[e$group == "Role Players"] <- "1"
e$group[e$group == "Combo Players"] <- "2"
e$group<-as.numeric(e$group)
par(mar=c(1,1,1,1))
library(pca3d)
Mpca<-prcomp(e,center = TRUE,scale. = TRUE)
pca3d(Mpca, group = e$group, legend = "topright", biplot = TRUE, biplot.vars = 2)

f=rbind(data5,data8)
f$Player<-NULL
f$Pos<-NULL
f$group[f$group == "Designated Scorers"] <- "1"
f$group[f$group == "3 Point Specialists"] <- "2"
f$group<-as.numeric(f$group)
par(mar=c(1,1,1,1))
library(pca3d)
Mpca<-prcomp(f,center = TRUE,scale. = TRUE)
pca3d(Mpca, group = f$group, legend = "topright", biplot = TRUE, biplot.vars = 2)

#diffusion maps
library(diffusionMap)
library(rdist)
library(readr)
library(rgl)
dis <- rdist(f, metric = "hamming")
dif <- diffuse(dis, neigen = 35)
plot3d(dif$X[,1:23],xlab = "Classifications",ylab="Clusters",zlab = "NBA")