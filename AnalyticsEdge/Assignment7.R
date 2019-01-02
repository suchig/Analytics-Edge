#######Election Forecastin############
statesMap = map_data("state")
str(statesMap)

###1.1
#How many different groups are there?
length(unique(statesMap$group))
#Ans - 63

###1.2
ggplot(statesMap,aes(x=long, y=lat, group=group))+geom_polygon(fill="white", color="black")
#Which one defined the color of the outline of the states?
#color

####2.1
polling=read.csv("PollingImputed.csv")
str(polling)
Train=subset(polling, Year>=2004&Year<=2008)
Test=subset(polling,Year>=2012)
pollingModel = glm(Republican~SurveyUSA+DiffCount,data=Train,family="binomial")
pollingPrediction = predict(pollingModel,newdata=Test,type="response")
TestPredictionBinary=as.numeric(pollingPrediction>0.5)
predictionDataFrame = data.frame(pollingPrediction,TestPredictionBinary,Test$State)
#For how many states is our binary prediction 1 (for 2012), corresponding to Republican
table(TestPredictionBinary)
#Ans - 22
#What is the average predicted probability of our model (on the Test set, for 2012)?
 mean(pollingPrediction)
# 0.4852626

###2.2
predictionDataFrame$region = tolower(predictionDataFrame$Test.State)
predictionMap=merge(statesMap,predictionDataFrame,by="region")
predictionMap = predictionMap[order(predictionMap$order),]
str(predictionMap)
#How many observations are there in predictionMap?
#Ans - 15034
#How many observations are there in statesMap?
str(statesMap)
#Ans - 15537

###2.3
# When we merged the data in the previous problem, it caused the number of observations to change. Why?
# Ans - Because we only make predictions for 45 states, we no longer have observations for some of the states. These observations were removed in the merging process

###2.4
ggplot(predictionMap,aes(x=long,y=lat,group=group))+geom_polygon(aes(fill=TestPredictionBinary),color="black")
#Which color represents a Republican prediction?
#Ans - LightBlue

###2.5
ggplot(predictionMap,aes(x=long,y=lat,group=group,fill=TestPredictionBinary))+geom_polygon(color="black")+scale_fill_gradient(low="blue",high="red",guide="legend",breaks=c(0,1),labels=c("Democrat","Republican"),name="Prediction 2012")
#Do the colors of the states in the map for TestPrediction look different from the colors of the states in the map with TestPredictionBinary? Why or why not?
ggplot(predictionMap,aes(x=long,y=lat,group=group,fill=pollingPrediction))+geom_polygon(color="black")+scale_fill_gradient(low="blue",high="red",guide="legend",breaks=c(0,1),labels=c("Democrat","Republican"),name="Prediction 2012")
#Ans - The two maps look very similar. This is because most of our predicted probabilities are close to 0 or close to 1

###3.1
# Did we predict this state correctly or incorrectly?
#Ans - We incorrectly predicted this state by predicting that it would be won by the Republican party

###3.2
#What was our predicted probability for the state of Florida?
predictionMap$pollingPrediction[predictionMap$region=="florida"]
#Ans - 0.9640395
#What does this imply?
# Ans -  Our prediction model did not do a very good job of correctly predicting the state of Florida, and we were very confident in our incorrect prediction

###Problem 4.1
# What is the name of the parameter we changed to create plot (1)? 
ggplot(predictionMap,aes(x=long,y=lat,group=group,fill=pollingPrediction))+geom_polygon(color="black",linetype=2)+scale_fill_gradient(low="blue",high="red",guide="legend",breaks=c(0,1),labels=c("Democrat","Republican"),name="Prediction 2012")
# Ans - linetype
#What is the name of the parameter we changed to create plot (2)?
ggplot(predictionMap,aes(x=long,y=lat,group=group,fill=pollingPrediction))+geom_polygon(color="black",size=2)+scale_fill_gradient(low="blue",high="red",guide="legend",breaks=c(0,1),labels=c("Democrat","Republican"),name="Prediction 2012")
#Ans - size

###Problem 4.2
ggplot(predictionMap,aes(x=long,y=lat,group=group,fill=pollingPrediction))+geom_polygon(color="black",alpha=0.3)+scale_fill_gradient(low="blue",high="red",guide="legend",breaks=c(0,1),labels=c("Democrat","Republican"),name="Prediction 2012")
# Ans - alpha

###########Visualizing Network Data#############
###1.1
edges = read.csv("edges.csv")
users=read.csv("users.csv")
#How many Facebook users are there in our dataset?
str(users)
#Ans - 59
#In our dataset, what is the average number of friends per user
#I tried 146/59 (#of edges/#of users). But it is 292/59 since we have to double count edges for friends
#Ans - Answer: 4.949153

##1.2
#Out of all the students who listed a school, what was the most common locale?
table(users$locale, users$school)
#Answer B

##1.3
#Is it possible that either school A or B is an all-girls or all-boys school?
table(users$gender, users$school)
#NO. Both genders have attnded both schools

###2.1
install.packages("igraph")
library(igraph)
?graph.data.frame
#which of the following commands will create a graph g describing our social network, with the attributes of each user correctly loaded?
# g = graph.data.frame(edges, FALSE, users)

##2.2
g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)
# How many connected components with at least 2 nodes are there in the graph?
# Answer 4
# How many users are there with no friends in the network?
# Answer 7 

##2.3
degree(g)
# How many users are friends with 10 or more other Facebook users in this network?
 table(degree(g) >= 10)
# 9

##2.4
#V(g) gives vertices of graph
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)
# What is the largest size we assigned to any node in our graph?
max(V(g)$size)
#Ans - 11
# What is the smallest size we assigned to any node in our graph?
min(V(g)$size)
#Ans - 2

############
##3.1
V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)
# What is the gender of the users with the highest degree in the graph?
# Ans - B

##3.2
V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"
plot(g, vertex.label=NA)
# Are the two users who attended both schools A and B Facebook friends with each other?
# Ans - Yes
# What best describes the users with highest degree?
# Ans - Some, but not all, of the high-degree users attended school A

#3.3
V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"
plot(g, vertex.label=NA)
# The large connected component is most associated with which locale?
# Ans - B
#The 4-user connected component is most associated with which locale?
# Ans - A
###############

#4
?igraph.plotting 
#Which igraph plotting function would enable us to plot our graph in 3-D?
rglplot(g)
# Ans - rglplot
#What parameter to the plot() function would we use to change the edge width when plotting g?
plot(g, edge.width = 4,vertex.label=NA)
#Ans - edge.width

#######################Visualizing Text Data Using Word Clouds
###1.1
tweets=read.csv("tweets.csv",stringsAsFactors=FALSE)
library(tm)
library(SnowballC)
corpus=Corpus(VectorSource(tweets$Tweet))
corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeWords,stopwords("english"))
frequencies=DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))
#How many unique words are there across all the documents?
findFreqTerms(frequencies)
str(allTweets)
#Ans - 3780

###1.2
#  What is the most compelling rationale for skipping this step when visualizing text data?
# Ans - It will be easier to read and understand the word cloud if it includes full words instead of just the word stems

###2.1
install.packages("wordcloud")
library(wordcloud)
#Which function can we apply to allTweets to get a vector of the words in our dataset, which we'll pass as the first argument to wordcloud()?
#Ans colnames(allTweets)

###2.2
#Which function should we apply to allTweets to obtain the frequency of each word across all tweets?
# Ans - colSums(allTweets)

###2.3
#What is the most common word across all the tweets (it will be the largest in the outputted word cloud)?
wordcloud(colnames(allTweets), colSums(allTweets))
# Ans - apple

###2.4
# What is the most common word in this new corpus
corpus=Corpus(VectorSource(tweets$Tweet))
corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeWords,c("apple",stopwords("english")))
frequencies=DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))
wordcloud(colnames(allTweets), colSums(allTweets))
#Ans - iphone

###3.1
# Which word cloud is based only on the negative tweets (tweets with Avg value -1 or less)?
corpus=Corpus(VectorSource(tweets$Tweet[tweets$Avg <= -1]))
corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeWords,c("apple",stopwords("english")))
frequencies=DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(frequencies))
wordcloud(colnames(allTweets), colSums(allTweets),colors="purple")
#Ans - C

###3.2
# Only one word cloud was created without modifying parameters min.freq or max.words. Which word cloud is this?
# Ans A

###3.3
# Which word clouds were created with parameter random.order set to FALSE?
# Ans B and D

###3.4
# Which word cloud was built with a non-default value for parameter rot.per?
# Ans A (50% horizontal and 50% vertical)

###3.5
# For which word cloud was the parameter random.color set to TRUE?
# Ans D

###4.1
install.packages("RColorBrewer")
library(RColorBrewer)
display.brewer.all() 
#Which color palette would be most appropriate for use in a word cloud for which we want to use color to indicate word frequency?
# Ans YlOrRd

###4.2
#Which RColorBrewer palette name would be most appropriate to use when preparing an image for a document that must be in grayscale?
Greys

###4.3
wordcloud(colnames(allTweets), colSums(allTweets),colors=brewer.pal(9,"Blues"))
# Which of the following commands addresses this issue by removing the first 4 elements of the 9-color palette of blue colors? 
wordcloud(colnames(allTweets), colSums(allTweets),colors=brewer.pal(9, "Blues")[c(5, 6, 7, 8, 9)])
wordcloud(colnames(allTweets), colSums(allTweets),colors=brewer.pal(9, "Blues")[c(-1, -2, -3, -4)])
