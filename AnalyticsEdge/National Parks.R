# 1. Which park type has the most number of parks?
visits = read.csv("park_visits.csv")
str(visits)
visits2016jul = subset(visits,Year==2016 & Month==7)
table(visits2016jul$ParkType)
# Ans - National Historic Site
# Which specific park has the most number of visitors?
visits2016jul$ParkName[visits2016jul$logVisits == max (visits2016jul$logVisits)]
# Ans - Great Smoky Mountains NP

# 2 Which region has the highest average log visits in July 2016?
tapply(visits2016jul$logVisits,visits2016jul$Region,mean)
# Ans Pacific West
#the highest average log visits?
# Ans - 10.767849
# the lowest average log visits?
# Ans - 9.374157

#3 What is the correlation between entrance fee (the variable cost) and the log visits in July 2016?
cor(visits2016jul$cost,visits2016jul$logVisits)
# Ans 0.4010611
# Choose the most reasonable possible answer from the following statements:
##Higher entrance fees are associated with higher log visits, likely because more expensive parks are often more popular due to other features of the parks

#4 What observations do you make?
ys_ts=ts(ys$logVisits,start=c(2010,1),freq=12)
plot(ys_ts)
# Ans - Shapes are similar, cyclical with peaks in summer

# 5Why do we have NA's in the laglogVisits and laglogVisitsYear?
head(visits[is.na(visits$laglogVisits),])
head(visits[is.na(visits$laglogVisits) & visits$Month != 1,])
visits[ visits$ParkName == "Martin Luther King, Jr. Memorial" & visits$Year==2011,]
# Ans hese are lagged variables and the earlier data is not available for the first months.
# How many observations are there in visits now?
visits = visits[rowSums(is.na(visits)) == 0, ]
str(visits)
# Ans 21855

# 6 What's the coefficient of the laglogVisits variable?
visits$Month = as.factor(visits$Month)
Train = subset(visits,Year>=2010 & Year<=2014)
Test = subset(visits,Year>=2015 & Year<=2016)
mod = lm(logVisits ~ laglogVisits, data = Train)
summary(mod)
# Ans - 0.927945
# What's the out-of-sample R2 in the testing set for this simple model?
predictTest = predict(mod, newdata=Test)
# (For some reason SSETest = sum((predictTest-Test$logVists)^2) is returning 0)
SSETest = (predictTest - Test$logVisits)^2
SSTTest = (Test$logVisits-mean(Train$logVisits))^2
1-sum(SSETest)/sum(SSTTest)
# Ans - 0.8975923

# 7 which of the following statements are correct
newmod = lm(logVisits ~ laglogVisits+ laglogVisitsYear+ Year + Month + Region + ParkType + cost, data=Train)
summary(mod)
# The more stars the mre relevant. So laglogVisits and logLastYear are significant and ParkType does not make a difference

# 8 In the new model, what's the out-of-sample R2 in the testing set?
predictNewTest = predict(newmod, newdata=Test)
SSENewTest = (predictNewTest - Test$logVisits)^2
SSTNewTest = (Test$logVisits-mean(Train$logVisits))^2
1-sum(SSENewTest)/sum(SSTNewTest)

#9 Looking at the plot of the tree, how many different predicted values are there?
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
treeMod = rpart(logVisits ~ laglogVisits+ laglogVisitsYear+ Year + Month + Region + ParkType + cost, data=Train,method="anova",cp=0.05)
prp(treeMod)
# Ans 4
# What is the out-of-sample R2 on the testing set?
predictTreeTest = predict(treeMod,newdata=Test)
SSETreeTest = (predictTreeTest - Test$logVisits)^2
SSTTest = SSTNewTest
1-sum(SSETreeTest)/sum(SSTTest)
# Ans 0.7864307

#10 What is optimal cp value on this grid?
library(caret)
library(e1071)
numFolds=trainControl(method="cv", number=10)
cpGrid=expand.grid(.cp=seq(0.0001,0.005,0.0001))
tr = train(logVisits ~ laglogVisits+ laglogVisitsYear+ Year + Month + Region + ParkType + cost, data=Train,method="rpart",trControl=numFolds,tuneGrid=cpGrid)
tr
#Final cp value is the Answer = 0.0001
#In what direction should it change?
# Since R2 is 0.91 for cp value of 0.0001 and it decreases progressively as cp value increases and our goal is to bring it closer to 100, cp value has to be decreasing

#11 What is the out-of-sample R2 in the testing set?
treeCPMod=rpart(logVisits ~ laglogVisits+ laglogVisitsYear+ Year + Month + Region + ParkType + cost, data=Train,method="anova",cp=0.0001)
predictTreeCP=predict(treeCPMod,newdata=Test)
SSETreeCP = (predictTreeCP-Test$logVisits)^2
1-sum(SSETreeCP)/sum(SSTTest)
# Ans - 0.937113

#12 What is the R2 on the testing set for the random forest model?
library(randomForest)
set.seed(201)
modForest=randomForest(logVisits ~ laglogVisits+ laglogVisitsYear+ Year + Month + Region + ParkType + cost, data=Train)
predictForest=predict(modForest,newdata=Test)
SSERandom=(predictForest-Test$logVisits)^2
1-sum(SSERandom)/sum(SSTTest)
#Ans - 0.9461221 (not the same as actual answer)

