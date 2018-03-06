wine=read.csv("wine.csv")
str(wine)
summary(wine)
modelAGST = lm(Price ~ AGST, data=wine)
summary(modelAGST)
modelAGST$residuals
SSEAGST = sum(modelAGST$residuals^2)
SSEAGST
modelAGSTHarvestRain = lm(Price ~ AGST + HarvestRain, data=wine)
summary(modelAGSTHarvestRain)
SSEAGSTHarvestRain = sum(modelAGSTHarvestRain$residuals^2)
SSEAGSTHarvestRain
modelAll = lm(Price ~ AGST + HarvestRain + WinterRain + Age + FrancePop, data=wine)
summary(modelAll)
SSEAll = sum(modelAll$residuals^2)
SSEAll
modelHarvestWinter = lm(Price ~ HarvestRain + WinterRain, data=wine)
summary(modelHarvestWinter)
SSEHarvestWinter = sum(modelHarvestWinter$residuals^2)
SSEHarvestWinter
modelNoPop = lm(Price ~ AGST + HarvestRain + WinterRain + Age, data=wine)
summary(modelNoPop)
SSENoPop = sum(modelNoPop$residuals^2)
SSENoPop
cor(wine$WinterRain, wine$Price)
cor(wine$Age, wine$FrancePop)
cor(wine)
modelNoPopAge = lm(Price ~ AGST + HarvestRain + WinterRain , data=wine)
summary(modelNoPopAge)
wineTest = read.csv("wine_test.csv")
str(wineTest)
predictTest = predict(modelNoPop, newdata=wineTest)
predictTest
SSETest = sum((wineTest$Price-predictTest)^2)
SSETest
SSTTest = sum((wineTest$Price-mean(wine$Price))^2)
SSTTest
1-SSETest/SSTTest

baseball=read.csv("baseball.csv")
str(baseball)
moneyball = subset(baseball, Year < 2002)
str(moneyball)
moneyball$RD=moneyball$RS - Moneyball$RA
plot(moneyball$RD, moneyball$W)
WinsReg = lm(W ~ RD,data=moneyball)
summary(WinsReg)
RunsReg = lm(RS ~ OBP + SLG + BA, data=moneyball)
summary(RunsReg)
RunsRegRunsRegNoBA = lm(RS ~ OBP + SLG, data=moneyball)
summary(RunsRegNoBA)
RunsAll = lm(RA ~ OOBP + OSLG, data=moneyball)
summary(RunsAll)
baseball2002 = subset(baseball, Year==2002)
teamRank = c(1,2,3,3,4,4,4,4,5,5)
wins2012 = c(94,88,95,88,93,94,98,97,93,94)
cor(teamRank,wins2012)
wins2013 = c(97,97,92,93,92,96,94,96,92,90)
cor(teamRank,wins2013)

#Recitation
NBA=read.csv("NBA_train.csv")
str(NBA)
table(NBA$W, NBA$Playoffs)
NBA$PtsDiff=NBA$PTS-NBA$oppPTS
plot(NBA$PtsDiff,NBA$W)
WinsReg = lm(W ~ PtsDiff, data=NBA)
summary(WinsReg)
#W = 41+0.0326*PtsDiff
#So If Wins >= 42, PtsDiff >= 42-41/0.0326 = 30.67 or 31 more points
PointsReg = lm (PTS ~ X2PA + X3PA + FTA+ AST+ ORB+ DRB + STL + BLK +TOV, data=NBA)
summary(PointsReg)
SSE = sum(PointsReg$residuals^2)
mean(NBA$PTS)
PointsRegNoTOV = lm (PTS ~ X2PA + X3PA + FTA+ AST+ ORB+ DRB + STL + BLK, data=NBA)
summary(PointsRegNoTOV)
PointsRegNoTOVDRB = lm (PTS ~ X2PA + X3PA + FTA+ AST+ ORB+ STL + BLK, data=NBA)
summary(PointsRegNoTOVDRB)

PointsRegNoTOVDRBBLK = lm (PTS ~ X2PA + X3PA + FTA+ AST+ ORB+ STL , data=NBA)
summary(PointsRegNoTOVDRBBLK)
SSENew = sum(PointsRegNoTOVDRBBLK$residuals^2)
RMSENew = sqrt(SSENew/nrow(NBA))

NBA_test=read.csv("NBA_test.csv")
summary(NBA_test)
PointsPredictions = predict(PointsRegNoTOVDRBBLK, newdata=NBA_test)
SSETest = sum((PointsPrediction - NBA_test$PTS)^2)
SSETest
SSTTest = sum((mean(NBA$PTS)-NBA_test$PTS)^2)
SSTTest
R2 = 1-SSETest/SSTTest
RMSE = sqrt(SSETest/nrow(NBA_test))
RMSE