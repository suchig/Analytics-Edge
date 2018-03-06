framingham= read.csv("framingham.csv")
str(framingham)
set.seed(1000)
splitFramingham = sample.split(framingham$TenYearCHD,SplitRatio=0.65)
framinghamTrain = subset(framingham,splitFramingham==TRUE)
framinghamTest = subset(framingham,splitFramingham==FALSE)
framinghamLog = glm(TenYearCHD~.,data=framinghamTrain, family=binomial)
summary(framinghamLog)
framinghamTestPred=predict(framinghamLog, type="response",newdata=framinghamTest)
table(framinghamTest$TenYearCHD,framinghamTestPred>0.5)
ROCRpred = prediction(framinghamTestPred,framinghamTest$TenYearCHD)
as.numeric(performance(ROCRpred,measure="auc")
auc(CR(prediction())

#QQ 1 - 0.05555556,0.9944186
11/198
1069/1075

#QQ2 - California, Boston

#QQ3 - Cigarrettes