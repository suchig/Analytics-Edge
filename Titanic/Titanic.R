library("mice")
library("stringr")
library("caTools")
library("rpart")
library("rpart.plot")
library(caret)
library(e1071)
library(randomForest)
library(dplyr)

titanicTrain=read.csv("TitanicTrain.csv")
titanicTest=read.csv("TitanicTest.csv")
titanic = bind_rows(titanicTrain,titanicTest)

titanic$Title = str_extract(titanic$Name,"[A-Z][a-z]+\\.")
titanic$Title[titanic$Title == "Don." |titanic$Title == "Major."|titanic$Title == "Capt."|titanic$Title == "Jonkheer."|titanic$Title == "Rev."|titanic$Title == "Col."|titanic$Title == "Sir."]="Rare."
titanic$Title[titanic$Title == "Mme." |titanic$Title == "Countess."|titanic$Title == "Lady."|titanic$Title == "Dona."]="Mrs."
titanic$Title[titanic$Title == "Mlle." |titanic$Title == "Ms."]="Miss."
titanic$Title[titanic$Title == "Dr." &titanic$Sex == "male"]="Rare."
titanic$Title[titanic$Title == "Dr." &titanic$Sex == "female"]="Mrs."
titanic$Title=factor(titanic$Title)
titanic$Titleint[titanic$Title=="Mr."]=0
titanic$Titleint[titanic$Title=="Mrs."]=1
titanic$Titleint[titanic$Title=="Miss."]=2
titanic$Titleint[titanic$Title=="Master."]=3
titanic$Titleint[titanic$Title=="Rare."]=4
titanic$Pclass=factor(titanic$Pclass)
#table(titanic$Sex,titanic$Title)
#titanic$Surname = str_extract(titanic$Name,"[A-Z][a-z][^,]+")
#library(ggplot2)
#ggplot(titanic[1:891],aes(x=FamilySize))+ geom_bar(aes(fill=factor(Survived)),position="dodge")+scale_x_continuous(breaks=c(1:11))

tempData = as.data.frame(titanic%>%group_by(Ticket) %>% summarise(FamilySize=n()))
titanic = merge(titanic,tempData,by="Ticket",all.x=TRUE)
titanic$FamilySize[titanic$FamilySize==1] = titanic$SibSp[titanic$FamilySize==1]+titanic$Parch[titanic$FamilySize==1]+1

titanic$FamilySizeD[titanic$FamilySize==1] = 0
titanic$FamilySizeD[titanic$FamilySize<5 && titanic$FamilySize>1] = 1
titanic$FamilySizeD[titanic$FamilySize>=5] = 2

#titanic[titanic$Embarked=="",]
#titanic[substr(titanic$Cabin,1,1)=="B",]
#summary(titanic[substr(titanic$Cabin,1,1)=="B"&titanic$Embarked=="C",])
#summary(titanic[substr(titanic$Cabin,1,1)=="B"&titanic$Embarked=="S",])
titanic$Embarked[titanic$Embarked==""] = "S"
titanic$Embarked = as.factor(titanic$Embarked)

titanic$Fare[is.na(titanic$Fare)]=mean(titanic$Fare[titanic$Embarked=="S"&titanic$Pclass==3],na.rm=TRUE)
titanic$FareInd[titanic$Fare<=7.91]=0
titanic$FareInd[titanic$Fare>7.91 & titanic$Fare<=14.454]=1
titanic$FareInd[titanic$Fare>14.454 & titanic$Fare<=31]=2
titanic$FareInd[titanic$Fare>31]=3

#titanic[is.na(titanic$Fare),]
#titanic[is.na(titanic$Age),]

titanic$CabinInt[substr(titanic$Cabin,1,1)=="A"]=1
titanic$CabinInt[substr(titanic$Cabin,1,1)=="B"]=2
titanic$CabinInt[substr(titanic$Cabin,1,1)=="C"]=3
titanic$CabinInt[substr(titanic$Cabin,1,1)=="D"]=4
titanic$CabinInt[substr(titanic$Cabin,1,1)=="E"]=5
titanic$CabinInt[substr(titanic$Cabin,1,1)=="F"]=6
titanic$CabinInt[substr(titanic$Cabin,1,1)=="G"]=7
titanic$CabinInt[substr(titanic$Cabin,1,1)=="T"]=8

#titanic$Age[is.na(titanic$Age) & titanic$Title=="Mr."]=mean(titanic$Age[titanic$Title=="Mr."],na.rm=TRUE)
#titanic$Age[is.na(titanic$Age) & titanic$Title=="Mrs."]=mean(titanic$Age[titanic$Title=="Mrs."],na.rm=TRUE)
#titanic$Age[is.na(titanic$Age) & titanic$Title=="Miss."]=mean(titanic$Age[titanic$Title=="Miss."],na.rm=TRUE)
#titanic$Age[is.na(titanic$Age) & titanic$Title=="Master."]=mean(titanic$Age[titanic$Title=="Master."],na.rm=TRUE)
#titanic$Age[is.na(titanic$Age) & titanic$Title=="Rare."]=mean(titanic$Age[titanic$Title=="Rare."],na.rm=TRUE)
#Age and Deck manipulation
imputed=mice(titanic[c("Sex","Age","Titleint","Pclass","FamilySize","Embarked","CabinInt")])
completed=complete(imputed)
titanic$Age=completed$Age
titanic$CabinInt=completed$CabinInt

titanic$AgeRange[titanic$Age<=16]=0
titanic$AgeRange[titanic$Age>16.1 & titanic$Age<=32.1]=1
titanic$AgeRange[titanic$Age>32.1 & titanic$Age<=48.1]=2
titanic$AgeRange[titanic$Age>48.1 & titanic$Age<=64]=3
titanic$AgeRange[titanic$Age>64]=4

titanicTrain = titanic[is.na(titanic$Survived)==FALSE,]
titanicTest = titanic[is.na(titanic$Survived)==TRUE,]

set.seed(123)
split=sample.split(titanicTrain,SplitRatio=0.7)
titanicTrainSplit1 = subset(titanicTrain,split==TRUE)
titanicTrainSplit2 = subset(titanicTrain,split==FALSE)

set.seed(123)
titanicSplitModel = randomForest(factor(Survived) ~ Pclass+Sex+AgeRange+Embarked+Titleint+FamilySizeD+FareInd,data=titanicTrainSplit1)
predictTitanicSplit = predict(titanicSplitModel,newdata=titanicTrainSplit2)

table(titanicTrainSplit2$Survived,predictTitanicSplit)

set.seed(123)
titanicModel = randomForest(factor(Survived) ~ Pclass+Sex+AgeRange+Embarked+Titleint+FamilySizeD+FareInd,data=titanicTrain)
predictTitanic = predict(titanicModel,newdata=titanicTest)

table(predictTitanic)

PassengerId=titanicTest$PassengerId
Survived=predictTitanic
writeDF=data.frame(PassengerId,Survived)
write.csv(writeDF,"Survived.csv",row.names=FALSE)