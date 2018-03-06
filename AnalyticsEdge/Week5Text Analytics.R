#################
#Which of these problems is the LEAST likely to be a good application of natural language processing?
#Judging the winner of a poetry contest
##################
#Which of the three alternative metrics below would best capture the typical opinion of the five Amazon Mechanical Turk workers, would be less affected by mistakes, and is well-defined regardless of the five labels?
#An overall score equal to the median (middle) score
#####################
#For each of the following questions, pick the preprocessing task that we discussed in the previous video that would change the sentence "Data is useful AND powerful!" to the new sentence listed in the question.
#New sentence: Data useful powerful!
#Ans - Removing stop words
####################
tweets=read.csv("tweets.csv",stringsAsFactors=FALSE)
tweets$Negative=as.factor(tweets$Avg<=-1)
table(tweets$Negative)
install.packages("tm")
library(tm)
install.packages("SnowballC")
library(SnowballC)
corpus=Corpus(VectorSource(tweets$Tweet))
corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus,removePunctuation)
stopwords("english")[1:10]
corpus=tm_map(corpus,removeWords,c("apple", stopwords("english")))
corpus=tm_map(corpus,stemDocument)
###################
#Given a corpus in R, how many commands do you need to run in R to clean up the irregularities (removing capital letters and punctuation)? - 2
#How many commands do you need to run to stem the document? - 1
##################
frequencies=DocumentTermMatrix(corpus)
inspect(frequencies[1000:1005,505:515])
findFreqTerms(frequencies, lowfreq=20)
sparse=removeSparseTerms(frequencies,0.995)
tweetSparse=as.data.frame(as.matrix(sparse))
colnames(tweetSparse)=make.names(colnames(tweetSparse))
tweetSparse$negative=tweets$Negative
library(caTools)
set.seed(123)
split=sample.split(tweetSparse$negative,SplitRatio=0.7)
trainSparse=subset(tweetSparse,split==TRUE)
testSparse=subset(tweetSparse,split==FALSE)
##################
#n the previous video, we showed a list of all words that appear at least 20 times in our tweets. Which of the following words appear at least 100 times? Select all that apply. (HINT: use the findFreqTerms function)
#iphon, itun, new
#################
library(rpart)
library(rpart.plot)
tweetCart=rpart(negative~.,data=trainSparse,method="class")
predictCart=predict(tweetCart,newdata=testSparse,type="class")
table(testSparse$negative,predictCart)
table(testSparse$negative)
library(randomForest)
tweetRF=randomForest(negative~.,data=trainSparse,method="class")
predictRF=predict(tweetRF,newdata=testSparse,type="class")
table(testSparse$negative,predictRF)
######################
#Build a confusion matrix (with a threshold of 0.5) and compute the accuracy of the model. What is the accuracy?
#Ans -  0.8084507
####################
#What were the goals of IBM when they set out to build Watson? Select all that apply.
#To build a computer that could compete with the best human players at Jeopardy!.
#To build a computer that could answer questions that are commonly believed to require human intelligence.
###################
#For which of the following reasons is Jeopardy! challenging? Select all that apply.
#A wide variety of categories.
#Speed is required - you have to buzz in faster than your competitors.
#The categories and clues are often cryptic
################
#Which of the following two questions do you think would be EASIEST for a computer to answer?
#What year was Abraham Lincoln born? correct
###########
#Select the LAT of the following Jeopardy question: NICHOLAS II WAS THE LAST RULING CZAR OF THIS ROYAL FAMILY (Hint: The answer is "The Romanovs")
#THIS ROYAL FAMILY correct
#Select the LAT of the following Jeopardy question: REGARDING THIS DEVICE, ARCHIMEDES SAID, "GIVE ME A PLACE TO STAND ON, AND I WILL MOVE THE EARTH" (Hint: The answer is "A lever")
#THIS DEVICE correct
###############
#To predict which candidate answer is correct, we said that Watson uses logistic regression. Which of the other techniques that we have learned could be used instead? Select all that apply.
#CART correct
#Random Forests correct
###############
#Recitation
emails=read.csv("energy_bids.csv",stringsAsFactors=FALSE)
strwrap(emails$email[1])
table(emails$responsive) 
corpus=Corpus(VectorSource(emails$email))
strwrap(corpus[[1]])
corpus=tm_map(corpus,tolower)
corpus=tm_map(corpus,removePunctuation)
corpus=tm_map(corpus,removeWords,stopwords("english"))
corpus=tm_map(corpus,stemDocument)
dtm=DocumentTermMatrix(corpus)
dtm=removeSparseTerms(dtm,0.97)
labeledTerms=as.data.frame(as.matrix(dtm))
labeledTerms$responsive = emails$responsive
set.seed(144)
split=sample.split(labeledTerms$responsive, SplitRatio=0.7)
train=subset(labeledTerms,split==TRUE)
test=subset(labeledTerms,split==FALSE)
emailCART=rpart(responsive~.,data=train,method="class")
prp(emailCART)
predictCART=predict(emailCART,newdata=test,type="response")
table(test$responsive,predictCART)
(195+25)/nrow(test)
table(test$responsive)
215/(215+42)
library(ROCR)
predROCR=prediction(as.numeric(predictCART),test$responsive)
perfROCR=performance(predROCR,"tpr","fpr")
plot(perfROCR,colorize=TRUE)
perfAUCROCR=performance(predROCR,"auc")@y.values
