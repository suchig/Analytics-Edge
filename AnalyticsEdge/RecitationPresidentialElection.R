polling=read.csv("PollingData.csv")
str(polling)
library(mice)
multiImpute=polling[c("Rasmussen","SurveyUSA","DiffCount","PropR")]
set.seed(144)
imputed=complete(mice(multiImpute))
summary(imputed)
polling$Rasmussen=imputed$Rasmussen
polling$SurveyUSA=imputed$SurveyUSA
pollingTrain = subset(polling, Year ==2004 | Year ==2008 )
pollingTest = subset(polling, Year == 2012)
table(pollingTrain$Republican)
table(sign(pollingTrain$Rasmussen))
table(pollingTrain$Republican,sign(pollingTrain$Rasmussen))

cor(pollingTrain[c("Rasmussen","SurveyUSA","DiffCount","PropR","Republican")])

pollingTrainLog = glm(Republican ~ PropR,data=pollingTrain,family="binomial")
summary(pollingTrainLog)
predictionTrainLog = predict(pollingTrainLog,type="response")
table(pollingTrain$Republican,predictionTrainLog>=0.5)
pollingTrainLogNew = glm(Republican ~ SurveyUSA+DiffCount,data=pollingTrain,family="binomial")
summary(pollingTrainLogNew)
predictionTrainLogNew = predict(pollingTrainLogNew,type="response")
table(pollingTrain$Republican,predictionTrainLogNew>=0.5)

table(pollingTest$Republican,sign(pollingTest$Rasmussen))

pollingTestPredict = predict(pollingTrainLogNew, newdata=pollingTest, type="response")
table(pollingTest$Republican,pollingTestPredict>=0.5)
subset(pollingTest, pollingTestPredict>=0.5 & Republican == 0)


