##########
#1.1 - 1815
wiki=read.csv("wiki.csv",stringsAsFactors=FALSE)
str(wiki)
wiki$Vandal = as.factor(wiki$Vandal)
table(wiki$Vandal)
#1.2- 6676 (6675)
corpusAdded=Corpus(VectorSource(wiki$Added))
corpusAdded=tm_map(corpusAdded,removeWords,stopwords("english"))
corpusAdded=tm_map(corpusAdded,stemDocument)
dtmAdded=DocumentTermMatrix(corpusAdded)
dtmAdded
#1.3 - 166
sparseAdded=removeSparseTerms(dtmAdded,0.997)
#1.4 - 162
wordsAdded=as.data.frame(as.matrix(sparseAdded))
colnames(wordsAdded) = paste("A", colnames(wordsAdded))
corpusremoved=Corpus(VectorSource(wiki$Removed))
corpusremoved=tm_map(corpusremoved,removeWords,stopwords("english"))
corpusremoved=tm_map(corpusremoved,stemDocument)
dtmRemoved=DocumentTermMatrix(corpusremoved)
sparseRemoved=removeSparseTerms(dtmRemoved,0.997)
wordsRemoved=as.data.frame(as.matrix(sparseRemoved))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))
str(wordsRemoved)
#1.5 - 0.5313844
wikiWords = cbind(wordsAdded, wordsRemoved)
wikiWords$Vandal=wiki$Vandal
set.seed(123)
split=sample.split(wikiWords$Vandal,SplitRatio=0.7)
train=subset(wikiWords,split==TRUE)
test=subset(wikiWords,split==FALSE)
table(test$Vandal)
618/nrow(test)
#1.6 - 0.544282 (0.5417)
wikiCART=rpart(Vandal~.,data=train,method="class")
predictCART=predict(wikiCART,newdata=test,type="class")
table(test$Vandal,predictCART)
(614+19)/nrow(test)
#1.7 - 3 (2) 
prp(wikiCART)
#1.8 - Although it beats the baseline, bag of words is not very predictive for this problem
###############################
#2.1 - 217
wikiWords2=wikiWords
wikiWords2$Http=grepl("http",wiki$Added,fixed=TRUE)
table(wikiWords2$Http)
217/nrow(wikiWords2)
#2.2 - 0.5735168 (.5726569)
wikiTrain2 = subset(wikiWords2,split==TRUE)
wikiTest2 = subset(wikiWords2,split==FALSE)
wikiCART2=rpart(Vandal~.,data=wikiTrain2 ,method="class")
predictCART2=predict(wikiCART2,newdata=wikiTest2 ,type="class")
table(wikiTest2$Vandal,predictCART2)
(603+64)/nrow(wikiTest2)
#2.3 - 4.050568
wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))
mean(wikiWords2$NumWordsAdded)
#2.4 - 0.6552021
wikiTrain2 = subset(wikiWords2,split==TRUE)
wikiTest2 = subset(wikiWords2,split==FALSE)
wikiCART2=rpart(Vandal~.,data=wikiTrain2 ,method="class")
predictCART2=predict(wikiCART2,newdata=wikiTest2 ,type="class")
table(wikiTest2$Vandal,predictCART2)
(514+248)/nrow(wikiTest2)
#2.5 -  0.7188306
wikiWords3=wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin
wikiTrain3 = subset(wikiWords3,split==TRUE)
wikiTest3 = subset(wikiWords3,split==FALSE)
wikiCART3=rpart(Vandal~.,data=wikiTrain3 ,method="class")
predictCART3=predict(wikiCART3,newdata=wikiTest3 ,type="class")
table(wikiTest3$Vandal,predictCART3)
(595+241)/nrow(wikiTest3)
#2.6 - 3
prp(wikiCART3)
####################################################################
#Automating Reviews in Medicine
#1.1 - 3708
trials=read.csv("clinical_trial.csv",stringsAsFactors=FALSE)
summary(trials)
max(nchar(trials$abstract))
#1.2 - 112
nrow(trials[nchar(trials$abstract)==0,])
#1.3 - A decade of letrozole: FACE.
trials$title[nchar(trials$title)==min(nchar(trials$title))]
###############################
#2.1 - 31,335
corpusTitle=Corpus(VectorSource(trials$title))
corpusAbstract=Corpus(VectorSource(trials$abstract))
corpusTitle=tm_map(corpusTitle,tolower)
corpusAbstract=tm_map(corpusAbstract,tolower)
corpusTitle=tm_map(corpusTitle,removePunctuation)
corpusAbstract=tm_map(corpusAbstract,removePunctuation)
corpusTitle=tm_map(corpusTitle,removeWords,stopwords("english"))
corpusAbstract=tm_map(corpusAbstract,removeWords,stopwords("english"))
corpusTitle=tm_map(corpusTitle,stemDocument)
corpusAbstract=tm_map(corpusAbstract,stemDocument)
dtmTitle=DocumentTermMatrix(corpusTitle)
dtmAbstract=DocumentTermMatrix(corpusAbstract)
sparseTitle=removeSparseTerms(dtmTitle,0.95)
sparseAbstract=removeSparseTerms(dtmAbstract,0.95)
titleDF=as.data.frame(as.matrix(sparseTitle))
abstractDF=as.data.frame(as.matrix(sparseAbstract))
sparseTitle
sparseAbstract
#2.2 - Abstracts tend to have many more words than titles
#2.3 - patient
sort(colSums(abstractDF))
####################################
#3.1 - Adding the letter T in front of all the title variable names and adding the letter A in front of all the abstract variable names
colnames(titleDF) = paste0("T", colnames(titleDF))
colnames(abstractDF) = paste0("A", colnames(abstractDF))
#3.2 - 367
dtm=cbind(titleDF, abstractDF)
dtm$trial=trials$trial
str(dtm)
#3.3 - 0.5606759
set.seed(144)
split=sample.split(dtm$trial,0.7)
train = subset(dtm,split==TRUE)
test = subset(dtm,split==FALSE)
table(train$trial)
730/nrow(train)
#3.4 - Tphase
trialCART=rpart(trial~.,data=train,method="class")
prp(trialCART)
#3.5 - 0.8718861
trainPredict=predict(trialCART)
max(trainPredict[,2])
#3.6 - The maximum predicted probability will likely be exactly the same in the testing set.
#3.7 - 0.8233487,0.770979,0.8643836
table(train$trial,trainPredict[,2]>=0.5)
(631+441)/nrow(train)
441/(131+441)
631/(631+99)
#################################
#4.1 - 0.7580645
predTest=predict(trialCART,newdata=test)
table(test$trial,predTest[,2]>=0.5)
(261+162)/nrow(test)
#4.2 - 0.8371063
predictROCR=prediction(predTest[,2],test$trial)
AUC=performance(predictROCR,"auc")@y.values
AUC
#####################
#5.1 - A paper that should have been included in Set A will be missed, affecting the quality of the results of Step 3.
#5.2 - A paper will be mistakenly added to Set A, yielding additional work in Step 2 of the process but not affecting the quality of the results of Step 3.
#5.3 - A false negative is more costly than a false positive; the decision maker should use a probability threshold less than 0.5 for the machine learning model
######################
#Separating Spam from Ham (Part 1)
#1.1 - 5728
emails=read.csv("emails.csv",stringsAsFactors=FALSE)
str(emails)
#1.2 - 1368
table(emails$spam)
#1.3 - subject
corpusText=Corpus(VectorSource(emails$text))
corpusText=tm_map(corpusText,tolower)
corpusText=tm_map(corpusText,removePunctuation)
toString(corpusText[1])
#1.4 - Yes -- the number of times the word appears might help us differentiate spam from ham.
#1.5 - 43952
max(nchar(emails$text))
#1.6 - 1992
which.min(nchar(emails$text))
############################3
#2.1 - 28687
corpusText=tm_map(corpusText,removeWords,stopwords("english"))
corpusText=tm_map(corpusText,stemDocument)
dtm=DocumentTermMatrix(corpusText)
dtm
#2.2 - 330
spdtm=removeSparseTerms(dtm,0.95)
spdtm
#2.3 - enron
emailsSparse=as.data.frame(as.matrix(spdtm))
colnames(emailsSparse)=make.names(colnames(emailsSparse))
which.max(colSums(emailsSparse))
#2.4 -6
emailsSparse$spam=emails$spam
which(colSums(emailsSparse[emailsSparse$spam == 0,])>=5000)
#2.5 - 3
which(colSums(emailsSparse[emailsSparse$spam == 1,])>=1000)
#2.6 - he frequencies of these most common words are likely to help differentiate between spam and ham.
#2.7 - The models we build are personalized, and would need to be further tested before being used as a spam filter for another person
######################
#3.1 - 3046, 954,10
emailsSparse$spam=as.factor(emailsSparse$spam)
set.seed(123)
split=sample.split(emailsSparse$spam,0.7)
train=subset(emailsSparse,split==TRUE)
test=subset(emailsSparse,split==FALSE)
spamLog=glm(spam~.,data=train,family=binomial)
spamCART=rpart(spam~.,data=train,method="class")
set.seed(123)
spamRF=randomForest(spam~.,data=train,method="class")
predictLog=predict(spamLog,type="response")
table(predictLog<0.00001)
table(predictLog>0.99999)
nrow(train)-3059-953
table(predictLog>= 0.00001 & predictLog<= 0.99999)
#3.2 - 0
summary(spamLog)
#3.3 - 2
prp(spamCART)
#3.4 - [1] 0.9990025
table(train$spam,predictLog>0.5)
(3052+954)/nrow(train)
#3.5 - 0.9999959
performance(prediction(predictLog,train$spam),"auc")@y.values
#3.6 - [1] 0.942394
predictCART=predict(spamCART,type="class")
table(train$spam,predictCART)
(2885+894)/nrow(train)
#3.7 - 0.9696044
predictCART=predict(spamCART)
performance(prediction(predictCART[,2],train$spam),"auc")@y.values
#3.8-[1] 0.978803
predictRF=predict(spamRF,type="prob")
table(train$spam,predictRF[,2]>0.5)
(3013+912)/nrow(train)
#3.9 - 0.9978502
performance(prediction(predictRF[,2],train$spam),"auc")@y.values
#3.10 - Logistic
###############################
#4.1-0.9505239
predictTestLog=predict(spamLog,newdata=test,type="response")
table(test$spam,predictTestLog>0.5)
(1257+376)/nrow(test)
#4.2- 0.9627517
performance(prediction(predictTestLog,test$spam),"auc")@y.values
#4.3 - 0.9394645
predictTestCART=predict(spamCART,newdata=test)
table(test$spam,predictTestCART[,2]>0.5)
(1228+386)/nrow(test)
#4.4 - 0.963176
performance(prediction(predictTestCART[,2],test$spam),"auc")@y.values
#4.5 - 0.976135
predictTestRF=predict(spamRF,newdata=test,type="prob")
table(test$spam,predictTestRF[,2]>0.5)
(1290+387)/nrow(test)
#4.6 - 0.997506
performance(prediction(predictTestRF[,2],test$spam),"auc")@y.values
#4.7 - RandomForest
#4.8 - Logistic

