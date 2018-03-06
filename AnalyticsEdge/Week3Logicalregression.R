#Quick Question 1.1 - Deciding whether to buy, sell, or hold a stock,The winner of an election with two candidates,Whether or not revenue will exceed $50,000,The day of the week with the highest revenue
#Quick Question 1.2 - The winner of an election with two candidates, Whether or not revenue will exceed $50,000

#Quick Question 2.1 -> -1, 0.3678794,0.2689414

-1.5+1*3+5*(-0.5)
exp(-1)
1/(1+(exp(1)))

quality=read.csv("quality.csv")
str(quality)
table(quality$PoorCare)
#Baseline accuracy
98/131
set.seed(88)
split = sample.split(quality$PoorCare,SplitRatio = 0.75)
split
qualityTrain = subset(quality, split==TRUE)
qualityTest= subset(quality, split==FALSE)
nrow(qualityTrain)
nrow(qualityTest)
QualityLog = glm(PoorCare ~ OfficeVisits + Narcotics, data=qualityTrain, family=binomial)
summary(QualityLog)
predictTrain = predict(QualityLog, type="response")
summary(predictTrain)
tapply(predictTrain, qualityTrain$PoorCare, mean)

#Quick Question 3.1 - 1.95230, PoorCare
QualityLogSCPC = glm(PoorCare ~ StartedOnCombination + ProviderCount, data=qualityTrain, family=binomial)
summary(QualityLogSCPC)

table(qualityTrain$PoorCare, predictTrain > 0.5)
10/25
70/74
table(qualityTrain$PoorCare, predictTrain > 0.7)

table(qualityTrain$PoorCare, predictTrain > 0.3)
library(ROCR)
ROCRpred = prediction(predictTrain, qualityTrain$PoorCare)
ROCRperf = performance(ROCRpred, "tpr","fpr")
plot(ROCRperf)
plot(ROCRperf,colorize=TRUE)
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.1),text.adj=c(-0.2,1.7))

#Quick Question 1 - t=0.2

predictTest = predict(QualityLog, type="response", newdata=qualityTest)
ROCRpredtest = prediction(predictTest,qualityTest$PoorCare)
auc = as.numeric(performance(ROCRpredtest, "auc")@y.values)