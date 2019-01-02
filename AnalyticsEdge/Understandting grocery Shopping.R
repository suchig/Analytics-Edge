# 1 What time of day are most orders placed?
orders=read.csv("orders.csv")
table(orders$order_hour_of_day)
#Ans - Mid day (Since when we divide hours as 8 buckets, we see that mid 8 has more orders.)
# What is the average days since prior order?
mean(orders$days_since_prior_order)
# Ans - 17.093

#2 What's the correlation between the orders of "fresh.fruits" and "fresh.vegetables"?
cor(orders$fresh.fruits, orders$fresh.vegetables)
# Ans 0.3955114
#In the dataset, what proportion of orders have at least one item from the frozen.pizza aisle?
# Ans - 0.0522


#3 What is the maximum value of frozen.dessert after normalization?
orders.aisle = orders[, 5:ncol(orders)]
library(caret)
preproc = preProcess(orders.aisle)
ordersNorm = predict(preproc, orders.aisle)
summary(orderNorm)
#Ans - 11.74144
# What is the minimum value of soft.drinks in the normalized dataset?
# Ans - -0.2873

#4 Based on the dendrogram, how many clusters do you think would NOT be appropriate for this problem?
distances <- dist(ordersNorm, method = "euclidean")
ClusterProducts <- hclust(distances, method = "ward.D")
plot(ClusterProducts, labels = FALSE)
#Ans -5 (There is not much width to make anything more than 4 clusters)

#5 How many observations are in the smallest cluster?
set.seed(200)
kCluster = kmeans(ordersNorm,centers=4,iter.max=1000)$cluster
table(kCluster)
#Ans
#   1	    2	    3 	   	4 
# 403 1152 3409   36 
# 36
# How many observations are in the largest cluster?
#Ans 3409

#6 Which cluster best fits the description "orders mostly consistents of cleaning supplies, beauty, and some pantry foods"?
Cluster4 = subset(ordersNorm,kCluster==4)
Cluster1 = subset(ordersNorm,kCluster==1)
Cluster2 = subset(ordersNorm,kCluster==2)
Cluster3 = subset(ordersNorm,kCluster==3)
tail(sort(colMeans(Cluster4)))
#       hot.dogs.bacon.sausage                 cookies.cakes                chips.pretzels                 ice.cream.ice refrigerated.pudding.desserts                frozen.dessert 
 #                   0.6303632                     0.6337265                     0.7812282                     0.9941437                     1.0725432                    11.7414356 
tail(sort(colMeans(Cluster3)))
#          red.wines baby.bath.body.care           skin.care         white.wines             spirits    packaged.produce 
#        0.006488456         0.006753500         0.007872597         0.007917013         0.011110078         0.033738924 
tail(sort(colMeans(Cluster2)))
#               pasta.sauce                fresh.herbs            packaged.cheese               fresh.fruits packaged.vegetables.fruits           fresh.vegetables 
#                 0.5147132                  0.5208811                  0.6405145                  0.7699809                  0.8092633                  0.8942010 
tail(sort(colMeans(Cluster1)))
# laundry    chips.pretzels       facial.care     cookies.cakes       paper.goods body.lotions.soap 
#        0.7611845         0.7693375         0.8060653         0.8575045         1.1017668         1.1180811 
#Ans: Cluster 1 as the same has categories asked in question

#7 Which cluster best fits the description "frozen desserts"?
#Ans Cluster 4

#8 Which cluster on average has the smallest amount of items ordered?
kMeans = kmeans(ordersNorm,centers=4,iter.max=1000)
rowSums(kMeans$centers)
#        1           2                3              4 
# 26.399041 25.571015 -9.564745 23.273325 
# Ans - 3

#9 If we ran hierarchical clustering a second time without making any additional calls to set.seed, we would expect:
#Ans Identical results to the first hierarchical clustering 
# If we ran k-means clustering a second time without making any additional calls to set.seed, we would expect:
# Ans - Different results
#If we ran k-means clustering a second time, again running the command set.seed(200) right before doing the clustering, we would expect:
# Ans - Identical results to the first k-means clustering 
# If we ran k-means clustering a second time, running the command set.seed(100) right before doing the clustering, we would expect:
# Ans Different results from the first k-means clustering 

#10 Would they want to increase or decrease the number of clusters?
# Ans - Increase

#11 Which has latest average hour of the day
tapply(orders$order_hour_of_day,kCluster,mean)
#       1        2        3        4 
# 13.50868 13.34722 13.68642 14.50000 
#Ans 4

#12 Why do we typically use cluster centroids to describe the clusters?
#Ans The cluster centroid captures the average behavior in the cluster, and can be used to summarize the general pattern in the cluster.

#13 Which of the following visualizations could be used to observe the distribution of days_since_prior_order, broken down by cluster? Select all that apply.
boxplot(kCluster~orders$days_since_prior_order)
library(ggplot2)
ggplot(data=orders,aes(x=kCluster,y=days_since_prior_order,group=kCluster))+geom_boxplot()
#Ans - Box Plot with cluster in x axis and ggplot with boxplot since data is discrete and not continous
#Which cluster has the longest average days since prior order?
tapply(orders$days_since_prior_order,kCluster,mean)
#Ans 1 - You can see this from the plots or use the above command