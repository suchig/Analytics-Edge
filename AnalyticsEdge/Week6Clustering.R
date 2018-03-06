#################
#About how many years did it take for a team to submit a 10% improvement over Cinematch?
#2.5 correct
#If Amazon.com constructs a recommendation system for books, and would like to use the same exact algorithm for shoes, what type would it have to be?
#Collaborative Filtering
#If Amazon.com would like to suggest books to users based on the previous books they have purchased, what type of recommendation system would it be?
#Content Filtering correct
#In the previous video, we discussed how clustering is used to split the data into similar groups. Which of the following tasks do you think are appropriate for clustering? Select all that apply.
#Dividing search results on Google into categories based on the topic
#Grouping players into different "types" of basketball players that make it to the NBA
#What is the distance between "The Godfather" and "Titanic", using euclidean distance?
#sqrt(2)
#How many clusters will there be at the start of the algorithm?
#212
#How many clusters will there be at the end of the algorithm?
#1
########################################
movies=read.table("movieLens.txt",header=FALSE,sep="|",quote="\"")
str(movies)
colnames(movies)=c("ID","Title","ReleaseDate","VideoReleaseDate","IMDB","Unknown", "Action","Adventure","Animation","Childrens","Comedy","Crime","Documentary","Drama","Fantasy","FilmNoir","Horro","Musical","Mystery","Romance", "SciFi","Thriller","War","Western")
movies$ID=NULL
movies$ReleaseDate=NULL
movies$VideoReleaseDate=NULL
movies$IMDB=NULL
movies=unique(movies)
###########################
#How many movies are classified as comedies? - 502
table(movies$Comedy)
#How many movies are classified as westerns? - 27
table(movies$Western)
#How many movies are classified as romance AND drama? - 97
table(movies$Romance&movies$Drama)
########################
distances = dist(movies[2:20],method="euclidean")
clusterMovies=hclust(distances,method="ward.D")
plot(clusterMovies)
clusterGroups=cutree(clusterMovies,k=10)
tapply(movies$Action,clusterGroups,mean)
tapply(movies$Romance,clusterGroups,mean)
subset(movies,Title=="Men in Black (1997)")
clusterGroups[257]
cluster2=subset(movies,clusterGroups==2)
cluster2$Title[1:10]
#####################
#What is the genre that all of the movies in cluster 2 belong to? - Drama
clusterGroups1=cutree(clusterMovies,k=2)
clustertwo=subset(movies,clusterGroups1==2)
clustertwo[1,]
################
#In this class, we've learned many different methods for predicting outcomes. Which of the following methods is designed to be used to predict an outcome like whether or not someone will experience a heart attack? Select all that apply.
#Logistic Regression
#CART
#Random Forest
#################
#Which bucket has the most data, in terms of number of patients?
#Cost Bucket 1 correct
#Which bucket probably has the densest data, in terms of number of claims per person?
#Cost Bucket 3 correct
###############
#K-means clustering differs from Hierarchical clustering in a couple important ways. Which of the following statements is true?
#In k-means clustering, you have to pick the number of clusters you want before you run the algorithm. correct
############
#If you wanted to find more unusual patterns shared by a small number of people, would you increase or decrease the number of clusters?
#Increase correct
##############
flower=read.csv("flower.csv",header=FALSE)
str(flower)
flowerMatrix=as.matrix(flower)
str(flowerMatrix)
flowerVector = as.vector(flowerMatrix)
str(flowerVector)
distance=dist(flowerVector,method="euclidean")
clusterIntensity=hclust(distance,method="ward.D")
plot(clusterIntensity)
rect.hclust(clusterIntensity,k=3, border="red")
flowerClusters=cutree(clusterIntensity,k=3)
flowerClusters
tapply(flowerVector,flowerClusters, mean)
dim(flowerClusters) = c(50,50)
image(flowerClusters,axes=FALSE)
image(flower,axes=FALSE)
image(flowerMatrix,axes=FALSE)
image(flowerMatrix,axes=FALSE,col=grey(seq(0,1,length=256)))
###################
healthy=read.csv("healthy.csv",header=FALSE)
healthyMatrix=as.matrix(healthy)
image(healthyMatrix,axes=FALSE, col=grey(seq(0,1,length=256)))
healthyVector=as.vector(healthyMatrix)
str(healthyVector)
n=365636
n*(n-1)/2
#66844659430 - So hierarchical clustering wont work
k=5
set.seed(1)
KMC=kmeans(healthyVector,centers=k,iter.max=1000)
str(KMC)
healthyClusters=KMC$cluster
dim(healthyClusters)=c(nrow(healthyMatrix),ncol(healthyMatrix))
image(healthyClusters,axes=FALSE,col=rainbow(k))
tumor = read.csv("tumor.csv",header=FALSE)
tumorMatrix=as.matrix(tumor)
tumorVector=as.vector(tumorMatrix)
install.packages("flexclust")
library(flexclust)
KMC.kcca=as.kcca(KMC,healthyVector)
tumorClusters=predict(KMC.kcca,newdata=tumorVector)
dim(tumorClusters)=c(nrow(tumorMatrix),ncol(tumorMatrix))
image(tumorClusters,axes=FALSE,col=rainbow(k))
######################################
#Running the dist function will probably take you a while. Why? Select all that apply.
#We have a lot of observations, so it takes a long time to compute the distance between each pair of observations. correct
#We have a lot of variables, so the distance computation is long. correct
election=read.csv("dailykos.csv")
distElection = dist(election,method="euclidean")
clusterElection=hclust(distElection,method="ward.D")
###
#Plot the dendrogram of your hierarchical clustering model. Just looking at the dendrogram, which of the following seem like good choices for the number of clusters? Select all that apply.
#2 (lot of space)
#3 (lot of space)
plot(clusterElection)
###
#In this problem, we are trying to cluster news articles or blog posts into groups. This can be used to show readers categories to choose from when trying to decide what to read. Just thinking about this application, what are good choices for the number of clusters? Select all that apply.
#
#7 correct
#8 correct
#it is probably better to show the reader more categories than 2 or 3. Seven or eight categories seems more reasonable.
####
clusterGroups=cutree(clusterElection,k=7)
cluster1=subset(election,clusterGroups==1)
cluster2=subset(election,clusterGroups==2)
cluster3=subset(election,clusterGroups==3)
cluster4=subset(election,clusterGroups==4)
cluster5=subset(election,clusterGroups==5)
cluster6=subset(election,clusterGroups==6)
cluster7=subset(election,clusterGroups==7)
str(cluster3)
#How many observations are in cluster 3?
#374
#Which cluster has the most observations?
nrow(cluster1)
nrow(cluster2)
nrow(cluster3)
nrow(cluster4)
nrow(cluster5)
nrow(cluster6)
nrow(cluster7)
#Ans - Cluster 1
#Which cluster has the fewest observations?
#Ans - Cluster 4
########
tail(sort(colMeans(cluster1)))
#What is the most frequent word in this cluster, in terms of average value? Enter the word exactly how you see it in the output:
#Ans - bush
###########
#Which words best describe cluster 2?
tail(sort(colMeans(cluster2)))
# Ans - november, poll, vote, challenge
#Which cluster could best be described as the cluster related to the Iraq war?
tail(sort(colMeans(cluster3)))
tail(sort(colMeans(cluster4)))
tail(sort(colMeans(cluster5)))
tail(sort(colMeans(cluster6)))
tail(sort(colMeans(cluster7)))
#Ans - Cluster 5
#Given this information, which cluster best corresponds to the democratic party?
#Answer - Cluster 7
##########
set.seed(1000)
clusterNewGroups=kmeans(election,centers=7)$cluster
clusterAfterSplit=split(election,clusterNewGroups)
#How many observations are in Cluster 3?
nrow(clusterAfterSplit[[3]])
#Ans - 277
nrow(clusterAfterSplit[[1]])
nrow(clusterAfterSplit[[2]])
nrow(clusterAfterSplit[[4]])
nrow(clusterAfterSplit[[5]])
nrow(clusterAfterSplit[[6]])
nrow(clusterAfterSplit[[7]])
#Which cluster has the most number of observations?
#Ans - Cluster 4
#Which cluster has the fewest number of observations?
#Ans - Cluster 2
####################
tail(sort(colMeans(clusterAfterSplit[[1]])))
tail(sort(colMeans(clusterAfterSplit[[2]])))
tail(sort(colMeans(clusterAfterSplit[[3]])))
tail(sort(colMeans(clusterAfterSplit[[4]])))
tail(sort(colMeans(clusterAfterSplit[[5]])))
tail(sort(colMeans(clusterAfterSplit[[6]])))
tail(sort(colMeans(clusterAfterSplit[[7]])))
#Which k-means cluster best corresponds to the Iraq War?
#Ans - Cluster 3
#Which k-means cluster best corresponds to the democratic party? (Remember that we are looking for the names of the key democratic party leaders.)
#Ans - Cluster 2
#################
#2.3
table(clusterGroups,clusterNewGroups)
#Which Hierarchical Cluster best corresponds to K-Means Cluster 2?
#Ans - 7
#2.4 Which Hierarchical Cluster best corresponds to K-Means Cluster 3?
#Ans - 5
#2.5 Which Hierarchical Cluster best corresponds to K-Means Cluster 7?
#Ans - No Hierarchical Cluster contains at least half of the points in K-Means Cluster 7.
#2.6 Which Hierarchical Cluster best corresponds to K-Means Cluster 6?
#Ans - 2
###################
airlines=read.csv("AirlinesCluster.csv")
#1.1
#Looking at the summary of airlines, which TWO variables have (on average) the smallest values?
summary(airlines)
#Ans - BonusTrans, FlightTrans
#Which TWO variables have (on average) the largest values?
#Ans - Balance, BonusMiles
####
#1.2
#In this problem, we will normalize our data before we run the clustering algorithms. Why is it important to normalize the data before clustering?
#Ans - If we don't normalize the data, the clustering will be dominated by the variables that are on a larger scale.
###
library("caret")
preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
#1.3 - In the normalized data, which variable has the largest maximum value?
summary(airlinesNorm)
#Ans - FlightMiles
#n the normalized data, which variable has the smallest minimum value?
#Ans - DaysSinceEnrol
#########
#2.1
distanceAirlines = dist(airlinesNorm,method="euclidean")
hclusterAirlines = hclust(distanceAirlines,method="ward.D")
plot(hclusterAirlines)
#According to the dendrogram, which of the following is NOT a good choice for the number of clusters?
#Ans - 6 (not possible)
#2.2
hclusterGroupsAirlines = cutree(hclusterAirlines,k=5)
#How many data points are in Cluster 1?
hclusterGroupsAirlinesSplit=split(airlinesNorm,hclusterGroupsAirlines)
nrow(hclusterGroupsAirlinesSplit[[1]])
#Ans - 776
#2.3
tapply(airlines$Balance, hclusterGroupsAirlines , mean)
tapply(airlines$QualMiles, hclusterGroupsAirlines , mean)
tapply(airlines$BonusMiles, hclusterGroupsAirlines , mean)
tapply(airlines$BonusTrans, hclusterGroupsAirlines , mean)
tapply(airlines$FlightMiles, hclusterGroupsAirlines , mean)
tapply(airlines$FlightTrans, hclusterGroupsAirlines , mean)
tapply(airlines$DaysSinceEnroll, hclusterGroupsAirlines , mean)
#Compared to the other clusters, Cluster 1 has the largest average values in which variables (if any)?
#Ans - DaysSinceEnroll
#How would you describe the customers in Cluster 1?
#Ans - Infrequent but loyal customers. correct
#2.4 - Compared to the other clusters, Cluster 2 has the largest average values in which variables (if any)? 
#Ans - QualMiles, FlightMiles,FlightTrans
# How would you describe the customers in Cluster 2?
#Ans - Customers who have accumulated a large amount of miles, and the ones with the largest number of flight transactions
#2.5 - Compared to the other clusters, Cluster 3 has the largest average values in which variables (if any)? 
#Ans - Balance, BonusMiles, BonusTrans
#How would you describe the customers in Cluster 3?
#Ans - Customers who have accumulated a large amount of miles, mostly through non-flight transactions.
#2.6 - Compared to the other clusters, Cluster 4 has the largest average values in which variables (if any)?
#Ans - None
#How would you describe the customers in Cluster 4?
#Ans - Relatively new customers who seem to be accumulating miles, mostly through non-flight transactions
#2.7 - Compared to the other clusters, Cluster 5 has the largest average values in which variables (if any)?
#Ans - None
#How would you describe the customers in Cluster 5?
#Ans - Relatively new customers who don't use the airline very often.
########
#3.1
set.seed(88)
kCluster = kmeans(airlinesNorm,centers=5,iter.max=1000)$cluster
#How many clusters have more than 1,000 observations?
table(kCluster)
#Ans - 2
#3.2
tapply(airlines$Balance, kCluster, mean)
tapply(airlines$QualMiles, kCluster, mean)
tapply(airlines$BonusMiles, kCluster, mean)
tapply(airlines$BonusTrans, kCluster, mean)
tapply(airlines$FlightMiles, kCluster, mean)
tapply(airlines$FlightTrans, kCluster, mean)
tapply(airlines$DaysSinceEnroll, kCluster, mean)
#Ans - No, because cluster ordering is not meaningful in either k-means clustering or hierarchical clustering.
###############################
##Stock Returns
stocks=read.csv("StocksCluster.csv")
#1.1 - How many observations are in the dataset?
str(stocks)
#Ans - 11580
#1.2 What proportion of the observations have positive returns in December?
table(stocks$PositiveDec)
6324/11580
#Ans -  0.546114
#1.3 What is the maximum correlation between any two return variables in the dataset?
cor(stocks)
#Ans - 0.19167279
#1.4 - Which month (from January through November) has the largest mean return across all observations in the dataset?
summary(stocks)
#Ans - Apr
#Which month (from January through November) has the smallest mean return across all observations in the dataset?
#Ans - Sep
#2.1
library(caTools)
set.seed(144)
splitStocks = sample.split(stocks$PositiveDec,SplitRatio=0.7)
stocksTrain = subset(stocks,splitStocks==TRUE)
stocksTest = subset(stocks,splitStocks==FALSE)
stocksModel = glm(PositiveDec ~ ., data=stocksTrain, family=binomial)
predictStocksTrain = predict(stocksModel,type="response")
#What is the overall accuracy on the training set, using a threshold of 0.5?
table(stocksTrain$PositiveDec, predictStocksTrain > 0.5)
(990+3640)/length(predictStocksTrain)
#Ans -  0.5711818
#2.2
predictStocksTest = predict(stocksModel, type="response", newdata=stocksTest)
#What is the overall accuracy of the model on the test, again using a threshold of 0.5?
table(stocksTest$PositiveDec, predictStocksTest > 0.5)
(417+1553)/length(predictStocksTest)
#Ans - 0.5670697
#2.3 - What is the accuracy on the test set of a baseline model that always predicts the most common outcome (PositiveDec = 1)?
#Ans - table(stocksTest$PositiveDec)
#######
limitedTrain = stocksTrain
limitedTrain$PositiveDec = NULL
limitedTest = stocksTest
limitedTest$PositiveDec = NULL
#3.1 - Why do we need to remove the dependent variable in the clustering phase of the cluster-then-predict methodology?
# Ans - Needing to know the dependent variable value to assign an observation to a cluster defeats the purpose of the methodology
#3.2
library(caret)
preproc = preProcess(limitedTrain)
normTrain = predict(preproc, limitedTrain)
normTest = predict(preproc, limitedTest)
# What is the mean of the ReturnJan variable in normTrain?
mean(normTrain$ReturnJan)
#Ans - 0
# What is the mean of the ReturnJan variable in normTest?
mean(normTest$ReturnJan)
#Ans - 0.0004185886
#3.3 - Why is the mean ReturnJan variable much closer to 0 in normTrain than in normTest?
#Ans - The distribution of the ReturnJan variable is different in the training and testing set
#3.4
set.seed(144)
km=kmeans(normTrain,centers=3)
#Which cluster has the largest number of observations?
table(km$cluster)
#Ans - 2
#3.5
library(flexclust)
km.kcca = as.kcca(km, normTrain)
clusterTrain = predict(km.kcca)
clusterTest = predict(km.kcca, newdata=normTest)
#How many test-set observations were assigned to Cluster 2?
table(clusterTest)
#Ans - 2080
###################
#4.1
stocksTrain1 = subset(stocksTrain, clusterTrain==1)
stocksTrain2 = subset(stocksTrain, clusterTrain==2)
stocksTrain3 = subset(stocksTrain, clusterTrain==3)
stocksTest1 = subset(stocksTest, clusterTest==1)
stocksTest2 = subset(stocksTest, clusterTest==2)
stocksTest3 = subset(stocksTest, clusterTest==3)
#Which training set data frame has the highest average value of the dependent variable?
mean(stocksTrain1$PositiveDec)
mean(stocksTrain2$PositiveDec)
mean(stocksTrain3$PositiveDec)
#Ans - Train 1
#4.2
StocksModel1=glm(PositiveDec ~ ., data=stocksTrain1, family=binomial)
StocksModel2=glm(PositiveDec ~ ., data=stocksTrain2, family=binomial)
StocksModel3=glm(PositiveDec ~ ., data=stocksTrain3, family=binomial)
#Which variables have a positive sign for the coefficient in at least one of StocksModel1, StocksModel2, and StocksModel3 and a negative sign for the coefficient in at least one of StocksModel1, StocksModel2, and StocksModel3?
summary(StocksModel1)
summary(StocksModel2)
summary(StocksModel3)
#Ans - Jan, Feb, Mar, June, Aug, Oct
#4.3
PredictTest1=predict(StocksModel1,type="response", newdata=stocksTest1)
PredictTest2=predict(StocksModel2,type="response", newdata=stocksTest2)
PredictTest3=predict(StocksModel3,type="response", newdata=stocksTest3)
#What is the overall accuracy of StocksModel1 on the test set stocksTest1, using a threshold of 0.5?
table(stocksTest1$PositiveDec, PredictTest1> 0.5)
(30+774)/nrow(stocksTest1)
#Ans -  0.6194145
table(stocksTest2$PositiveDec, PredictTest2 > 0.5)
(388+757)/nrow(stocksTest2)
#Ans - 0.5504808
table(stocksTest3$PositiveDec, PredictTest3 > 0.5)
(49+13)/nrow(stocksTest3)
#Ans - 0.6458333
#############
#4.4
AllPredictions = c(PredictTest1, PredictTest2, PredictTest3)
AllOutcomes = c(stocksTest1$PositiveDec, stocksTest2$PositiveDec, stocksTest3$PositiveDec)
#What is the overall test-set accuracy of the cluster-then-predict approach, again using a threshold of 0.5?
table(AllOutcomes, AllPredictions  > 0.5)
 (467+1544)/length(AllOutcomes)
#Ans - 0.5788716
