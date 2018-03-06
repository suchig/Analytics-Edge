#Assignment 1 - Climate Change
#1.1 - 0.7509
ClimateChangeMaster = read.csv("climate_change.csv")
CCTraining = subset(ClimateChangeMaster, Year <= 2006)
CCTest = subset(ClimateChangeMaster, Year > 2006)
CCReg = lm(Temp ~ MEI+CO2+CH4+N2O+CFC.11 + CFC.12 + TSI + Aerosols, data = CCTraining)
summary(CCReg)

#1.2 MEI, CO2, CFC.11, CFC.12, TSI, Aerosols

#2.1 - All of the gas concentration variables reflect human development - N2O and CFC.11 are correlated with other variables in the data set.

#2.2 - CO2, CH4, CFC.12
cor(CCTraining)

#2.3 - CH4, CFC.12

#3 - 2.532e-02,  0.7261
CCRegN2OCor = lm(Temp ~ MEI+N2O+ TSI + Aerosols, data = CCTraining)
summary(CCRegN2OCor)

#4 -  0.7508, CH4
CCRegStep = step(CCReg)
summary(CCRegStep)

#5 - 0.6286051
PredictReg = predict(CCRegStep, newdata=CCTest)
summary(PredictReg)
SSECCTest = sum((PredictReg - CCTest$Temp)^2)
SSTCCTest = sum((mean(CCTraining$Temp)-CCTest$Temp)^2)
R2CC= 1 - SSECCTest/SSTCCTest

#Assignment 2 - PISA
#1.1 - 3663
pisaTrain = read.csv("pisa2009train.csv")
pisaTest = read.csv("pisa2009test.csv")
str(pisaTrain)

#1.2 - 483.5325, 512.9406
tapply(pisaTrain$readingScore,pisaTrain$male,mean)

#1.3 all except grade, male, publicSchool, urban, readingScore
summary(pisaTrain)

#1.4 - 2414,990
pisaTrain = na.omit(pisaTrain)
pisaTest = na.omit(pisaTest)
str(pisaTrain)
str(pisaTest)

#2.1 - raceeth, none

#2.2 - Everything other than more than one race and white

#2.3 - raceethAsian, None

#3.1 - 0.3251
pisaTrain$raceeth = relevel(pisaTrain$raceeth, "White")
pisaTest$raceeth = relevel(pisaTest$raceeth, "White")
lmScore = lm(readingScore ~.,data=pisaTrain)
summary(lmScore)

#3.2 - 73.36555
SSE = sum(lmScore$residuals^2)
RMSE = sqrt(SSE/nrow(pisaTrain))
RMSE

#3.3 - 29.54

#3.4 - Predicted average reading score of an Asian student

#3.5 - grade, male,all race, expectBachelors,motherBachelors,fatherBachelors,computerForSchoolWork,read30MinsDay,publicSchool,schoolSize

#4.1 - 284.5
predTest=predict(lmScore, newdata=pisaTest)
summary(predTest) 

#4.2 - 5762082, 76.29079
SSE = sum((predTest-pisaTest$readingScore)^2)
SSE
RMSE=sqrt(SSE/nrow(pisaTest))
RMSE

#4.3 - 517.9629,7802354
mean(pisaTrain$readingScore)
SST = sum((pisaTest$readingScore-mean(pisaTrain$readingScore))^2)
SST

#4.4 - 0.2614944
R2TestScore = 1-SSE/SST

#Assignment3
#1.1 2009-10-18 - 2009-10-24
FluTrain = read.csv("FluTrain.csv")
str(FluTrain)
FluTrain$Week[FluTrain$ILI==max(FluTrain$ILI)]
FluTrain$Week[FluTrain$Queries==max(FluTrain$Queries)]

#1.2 - Most of the ILI values are small, with a relatively small number of much larger values (in statistics, this sort of data is called "skew right").
hist(FluTrain$ILI)

#1.3 - There is a positive, linear relationship between log(ILI) and Queries.
plot(log(FluTrain$ILI),FluTrain$Queries)

#2.1 log(ILI) = intercept + coefficient x Queries, where the coefficient is positive

#2.2 - 0.709
FluTrend = lm(log(ILI) ~ Queries, data=FluTrain)
summary(FluTrend)

#2.3 - R-squared = Correlation^2
correlation = cor(log(FluTrain$ILI),FluTrain$Queries)
correlation^2
log(1/correlation)
exp(-0.5*correlation)

#3.1 - 2.187378
FluTest = read.csv("FluTest.csv")
PredTest = exp(predict(FluTrend, newdata=FluTest))
PredTest[grep("2012-03-11",FluTest$Week) ]

#3.2 - 0.04623827
(FluTest$ILI[11] - PredTest[11])/FluTest$ILI[11]

#3.3 -  0.7490645
SSE2 = sum((FluTest$ILI-PredTest)^2)
RMSE2 = sqrt(SSE2/nrow(FluTest))

#4.1 - 2
install.packages("zoo")
library(zoo)
ILILag2 = lag(zoo(FluTrain$ILI), -2, na.pad=TRUE)
FluTrain$ILILag2 = coredata(ILILag2)
summary(FluTrain)

#4.1 - There is a strong positive relationship between log(ILILag2) and log(ILI). 
plot(log(FluTrain$ILILag2),log(FluTrain$ILI))

#4.3 - All but Intercept,  0.9063
FluTrendLag = lm(log(ILI) ~ Queries + log(ILILag2), data=FluTrain)
summary(FluTrendLag)

#5.1 - 2
FluTest$ILILag2 = coredata(lag(zoo(FluTest$ILI), -2, na.pad=TRUE))
summary(FluTest)

#5.2 - The ILI value of the second-to-last observation in the FluTrain data frame, The ILI value of the last observation in the FluTrain data frame.

#5.3 - 1.852736,2.124130
FluTest$ILILag2[1]=FluTrain$ILI[416]
FluTest$ILILag2[2]=FluTrain$ILI[417]

#5.4 - 0.2942029
PredTestLag = exp(predict(FluTrendLag, newdata=FluTest))
summary(PredTestLag )
SSELag = sum((FluTest$ILI-PredTestLag)^2)
RMSELag = sqrt(SSELag/nrow(FluTest))
