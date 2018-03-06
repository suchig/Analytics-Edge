
#################################Assignment############################
#1.1 - 0.3158996
gerber=read.csv("gerber.csv")
table(gerber$voting)
108696/344084

#1.2 - neighbors
table(gerber$voting,gerber$hawthorne)
table(gerber$voting,gerber$civicduty)
table(gerber$voting,gerber$neighbors)
table(gerber$voting,gerber$self)

#1.3 - All of them
logmod=glm(voting~hawthorne+civicduty+neighbors+self,data=gerber,family="binomial")
summary(logmod)


#1.4 - 0.5419578
pred=predict(logmod,type="response")
table(gerber$voting,pred>=0.3)
(134513+51966)/nrow(gerber)

#1.5 - 0.6841004
table(gerber$voting,pred>=0.5)
235388/nrow(gerber)

#1.6 - ven though all of the variables are significant, this is a weak predictive model.
table(gerber$voting)
235388/nrow(gerber)
library(ROCR)
ROCRPredict = prediction(pred,gerber$voting) 
auc = as.numeric(performance(ROCRPredict, "auc")@y.values)

#2.1 - No variables are used (the tree is only a root node) - none of the variables make a big enough effect to be split on
cartModel=rpart(voting~hawthorne+civicduty+neighbors+self,data=gerber)
prp(cartModel)

#2.2 - Neighbor is the first split, civic duty is the last.
cartModel2=rpart(voting~hawthorne+civicduty+neighbors+self,data=gerber,cp=0.0)
prp(cartModel2)

#2.3 - 0.31

#2.4 - Men, Men
cartModel3=rpart(voting~sex+hawthorne+civicduty+neighbors+self,data=gerber,cp=0.0)
prp(cartModel3)

#3.1 - 0.043362
cartControlReg=rpart(voting~control,data=gerber,cp=0.0)
cartControlSexReg=rpart(voting~sex+control,data=gerber,cp=0.0)
ControlPred=predict(cartControlReg)
ControlSexPred=predict(cartControlSexReg)
prp(cartControlReg,digits=6)
abs(0.34-0.296638)

#3.2 - They are affected about the same (change in probability within 0.001 of each other). 
prp(cartControlSexReg,digits=6)

#3.3 - Coefficient is negative, reflecting that women are less likely to vote
LogReg=glm(voting~sex+control,data=gerber,family="binomial")
summary(LogReg)

#3.4 - 0.0003505
Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(LogReg, newdata=Possibilities, type="response")
abs(0.2908065-0.290456)

#3.5 - If a person is a woman and in the control group, the chance that she voted goes down.
LogModel2 = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")
summary(LogModel2)

#3.6 - 0
predict(LogModel2, newdata=Possibilities, type="response")
abs(0.2904558-0.290456)

#3.7 - No

###################
#1.1 - 0.754172
letters=read.csv("letters_ABPR.csv")
letters$isB=as.factor(letters$letter=="B")
set.seed(1000)
split=sample.split(letters$isB,SplitRatio=0.5)
LettersTrain=subset(letters,split==TRUE)
LettersTest=subset(letters,split==FALSE)
table(LettersTrain$isB)
table(LettersTest$isB)
1175/1558

#1.2 - 0.9358151
lettersTree = rpart(isB~.-letter,data=LettersTrain, method="class")
lettersPredict = predict(lettersTree,newdata=LettersTest,type="class")
table(LettersTest$isB,lettersPredict)
(1118+340)/nrow(LettersTest)

#1.3 -  0.9878049
set.seed(1000)
lettersForest=randomForest(isB~.-letter,data=LettersTrain)
lettersPredictForest = predict(lettersForest,newdata=LettersTest)
table(LettersTest$isB,lettersPredictForest )
(1165+374)/nrow(LettersTest)

#2.1 - 0.2573813
letters$letter = as.factor( letters$letter )
set.seed(2000)
split=sample.split(letters$letter,SplitRatio=0.5)
LettersTrain=subset(letters,split==TRUE)
LettersTest=subset(letters,split==FALSE)
table(LettersTrain$letter)
table(LettersTest$letter)
401/nrow(LettersTest)

#2.2 - 0.8786906
lettersTree = rpart(letter~.-isB,data=LettersTrain,method="class")
lettersPredict=predict(lettersTree,newdata=LettersTest,type="class")
table(LettersTest$letter,lettersPredict)
(348+318+363+340)/nrow(LettersTest)

#2.3 -  0.9801027
set.seed(1000)
lettersForest=randomForest(letter~.-isB,data=LettersTrain)
lettersPredictForest=predict(lettersForest,newdata=LettersTest)
table(LettersTest$letter,lettersPredictForest)
(390+380+393+364)/nrow(LettersTest)

###########################
#1.1 - Everything except race and native
census=read.csv("census.csv")
set.seed(2000)
split=sample.split(census$over50k,SplitRatio=0.6)
censusTrain=subset(census,split==TRUE)
censusTest=subset(census,split==FALSE)
censusReg=glm(over50k~.,data=censusTrain,family="binomial")
summary(censusReg)

#1.2 - 0.8552107
predictLogit = predict(censusReg,newdata=censusTest, type="response")
table(censusTest$over50k,predictLogit>0.5)
(9051+1888)/nrow(censusTest)

#1.3 - 0.7593621
table(censusTest$over50k)
9713/nrow(censusTest)

#1.4 - 0.9061598
predictAUC = prediction(predictLogit,censusTest$over50k)
auc=performance(predictAUC,"auc")
auc

#2.1 - 4
censusTree=rpart(over50k~.,data=censusTrain,method="class")
prp(censusTree)

#2.2 - relation

#2.3 - capitalgain, education

#2.4 - 0.8473927
censusTreePredict=predict(censusTree,newdata=censusTest,type="class")
table(censusTest$over50k,censusTreePredict)
(9243+1596)/nrow(censusTest)

#2.5 - The probabilities from the CART model take only a handful of values (five, one for each end bucket/leaf of the tree); the changes in the ROC curve correspond to setting the threshold to one of those values
censusTree=rpart(over50k~.,data=censusTrain)
censusTreePredict=predict(censusTree,newdata=censusTest)
predictPlot=prediction(censusTreePredict[,2],censusTest$over50k)
performanceAUC = performance(predictPlot,"tpr","fpr")
plot(performanceAUC)

#2.6 - 0.8470256
auc = performance(predictPlot,"auc")
auc

#3.1 - 0.8183097
set.seed(1)
censusTrainSmall=census[sample(nrow(censusTrain),2000),]
set.seed(1)
censusForest=randomForest(over50k~.,data=censusTrainSmall)
predictCensusForest=predict(censusForest,newdata=censusTest)
table(censusTest$over50k,predictCensusForest)
(9654+813)/nrow(censusTest)

#3.2 - age
vu = varUsed(censusForest, count=TRUE)
vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)
dotchart(vusorted$x, names(censusForest$forest$xlevels[vusorted$ix]))

#3.3 - occupation
varImpPlot(censusForest)

#4.1 - 0.002
set.seed(2)
numFolds=trainControl(method="cv", number=10)
cpGrid=expand.grid(.cp=seq(0.002,0.1,0.002))
train(over50k~.,data=censusTrain, method="rpart", trControl=numFolds, tuneGrid=cpGrid)

#4.2 - 0.8612306
censusModelCP=rpart(over50k~.,data=censusTrain,method="class",cp=0.002)
predictCensusCP=predict(censusModelCP,newdata=censusTest,type="class")
table(censusTest$over50k,predictCensusCP)
(9178+1838)/nrow(censusTest)

#4.3 - 18
prp(censusModelCP)