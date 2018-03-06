#Music
#1.1 - 7574
music=read.csv("songs.csv")
str(music)

#1.2 - 18
MJMusic = subset(music, artistname=="Michael Jackson")
str(MJMusic)

#1.3 - You Rock My World, You Are Not Alone
MJMusic$songtitle[MJMusic$Top10 == 1]

#1.4 - 3 4 5 7 1 0, 4
unique(music$timesignature)
table(music$timesignature)

#1.5 -  Wanna Be Startin' Somethin'
music$songtitle[music$tempo == max(music$tempo)]

#2.1 - 7201
musicTrain = subset(music, year <= 2009)
musicTest = subset(music, year >2009)
str(musicTrain)
str(musicTest)

#2.2 - 4827.2
filteredMusicTrain = musicTrain[,!(names(musicTrain) %in% c("year", "songtitle", "artistname", "songID", "artistID"))]
filteredMusicTest = musicTest[,!(names(musicTest) %in% c("year", "songtitle", "artistname", "songID", "artistID"))]
modelTrainAll = glm(Top10~.,data=filteredMusicTrain,family=binomial)
summary(modelTrainAll)

#2.3 - The higher our confidence about time signature, key and tempo, the more likely the song is to be in the Top 10

#2.4 - Mainstream listeners tend to prefer less complex songs

#2.5 - Mainstream listeners prefer songs with heavy instrumentation, No

#3.1 - 0.7399067
cor(filteredMusicTrain$loudness,filteredMusicTrain$energy)

#3.2 - Model 2 suggests that songs with high energy levels tend to be more popular. This contradicts our observation in Model 1.
modelTrainNoLoudness = glm(Top10~.-loudness,data=filteredMusicTrain,family=binomial)
summary(modelTrainNoLoudness)

#3.3 - Yes
modelTrainNoEnergy = glm(Top10~.-energy,data=filteredMusicTrain,family=binomial)
summary(modelTrainNoEnergy)

#4.1 -  0.8793566
predictionTestNoEnergy = predict(modelTrainNoEnergy, newdata=filteredMusicTest, type="response")
table(filteredMusicTest$Top10,predictionTestNoEnergy >=0.45)
(309+19)/nrow(filteredMusicTest)

#4.2 - 0.8418231
table(filteredMusicTest$Top10)
314/(314+59)

#4.3 - 24,349

#4.4 -  0.3220339,  0.9840764
19/59
309/314

#4.5 - Model 3 provides conservative predictions, and predicts that a song will make it to the Top 10 very rarely. So while it detects less than half of the Top 10 songs, we can be very confident in the songs that it does predict to be Top 10 hits.

#Predicting Parole
#1.1 - 675
parole=read.csv("parole.csv")
str(parole)

#1.2 - 78
nrow(parole[parole$violator==1,])

#2.1 - state,crime

#2.2
 parole$state = as.factor(parole$state)
 parole$crime = as.factor(parole$crime)
summary(parole)

#3.1 70, 30
set.seed(144)
split = sample.split(parole$violator, SplitRatio = 0.7)
paroleTrain = subset(parole, split == TRUE)
paroleTest = subset(parole, split == FALSE)

#3.2 - The exact same training/testing set split as the first execution of [1]-[5], A different training/testing set split from the first execution of [1]-[5], A different training/testing set split from the first execution of [1]-[5]

#4.1 - race, state4, multipleoffenses
paroleTrainModel = glm(violator ~.,data=paroleTrain,family=binomial)
summary(paroleTrainModel)

#4.2 - Our model predicts that a parolee who committed multiple offenses has 5.01 times higher odds of being a violator than a parolee who did not commit multiple offenses but is otherwise identical.
 exp(1.6119919)

#4.3 - 0.1825687
Odds = exp(-4.2411574 + (0.3869904*1) + (0.8867192*1) + (-0.0001756*50) + (-0.1238867*3) + (12*0.0802954) + (0.6837143*1))
prob = log(-4.2411574 + (0.3869904*1) + (0.8867192*1) + (-0.0001756*50) + (-0.1238867*3) + (12*0.0802954) + (0.6837143*1))

#5.1 - 0.907279
paroleTestPredict = predict(paroleTrainModel,newdata=paroleTest,type="response")
summary(paroleTestPredict)

#5.2 - 0.5217391,0.9329609, 0.8861386
table(paroleTest$violator,paroleTestPredict >=0.5)
12/23
167/(167+12)
(167+12)/(167+12+11+12)

#5.3 0.8861386
table(paroleTest$violator)
179/(179+23)

#5.4 - The board assigns more cost to a false negative than a false positive, and should therefore use a logistic regression cutoff less than 0.5.

#5.5 - The model is likely of value to the board, and using a different logistic regression cutoff is likely to improve the model's value

#5.6 - 0.8945834
ROCRPredict = prediction(paroleTestPredict,paroleTest$violator) 
auc = as.numeric(performance(ROCRPredict, "auc")@y.values)

#5,7 - The probability the model can correctly differentiate between a randomly selected parole violator and a randomly selected parole non-violator.

#6.1 - We should use a dataset tracking a group of parolees from the start of their parole until either they violated parole or they completed their term. correct

#LendingClub
#1.1 - 0.1600543
loans=read.csv("loans.csv")
str(loans)
table(loans$not.fully.paid)

#1.2 - log.annual.inc, days.with.cr.line, revol.util,inq.last.6mths,delinq.2yrs,pub.rec
summary(loans)

#1.3 - e want to be able to predict risk for all borrowers, instead of just the ones with all data reported
missing=subset(loans,is.na(log.annual.inc)==TRUE | is.na(days.with.cr.line)==TRUE |is.na(revol.util)==TRUE |is.na(inq.last.6mths)==TRUE |is.na(delinq.2yrs)==TRUE |is.na(pub.rec)==TRUE )
table(missing$not.fully.paid)
12/62

#1.4 - We predicted missing variable values using the available independent variables for each observation.
set.seed(144)
lendingImputed=complete(mice(loans[c("log.annual.inc", "days.with.cr.line", "revol.util","inq.last.6mths","delinq.2yrs","pub.rec")]))
loans[c("log.annual.inc", "days.with.cr.line", "revol.util","inq.last.6mths","delinq.2yrs","pub.rec")]=lendingImputed

#2.1 - credit.policy,purposecredit_card, purposedebt_consolidation,purposesmall_business,installment,log.annual.inc,fico,inq.last.6mths,pub.rec
set.seed(144)
split=sample.split(loans$not.fully.paid, SplitRatio = 0.7)
loanTrain = subset(loans,split==TRUE)
loanTest = subset(loans,split==FALSE)
loanTrainModel = glm(not.fully.paid~.,data=loanTrain,family="binomial")
summary(loanTrainModel)

#2.2 - 0.09317
-.009317*-10

#2.3 - 1.097648
exp(700*-.009317)/exp(710*-.009317)

#2.4 - 0.8364079,0.8398886
predicted.risk=predict(loanTrainModel,newdata=loanTest, type="response")
loanTest$predicted.risk=predicted.risk
table(loanTest$not.fully.paid,predicted.risk>=0.5)
2403/(2403+13+457)
table(loanTest$not.fully.paid)
2413/(2413+460)

#2.5 - 0.6720995
ROCRpred = prediction(predicted.risk,loanTest$not.fully.paid)
as.numeric(performance(ROCRpred,"auc")@y.values)

#3.1 - int.rate is correlated with other risk-related variables, and therefore does not incrementally improve the model when those other variables are included.
loanSmartModel = glm(not.fully.paid~int.rate,data=loanTrain,family=binomial)
summary(loanSmartModel)

#3.2 - 0.42662,0
predicted.smartrisk = predict(loanSmartModel,newdata=loanTest,type="response")
summary(predicted.smartrisk)
table(loanTest$not.fully.paid,predicted.smartrisk==0.5)

#3.3 - 0.6239081
as.numeric(performance(prediction(predicted.smartrisk,loanTest$not.fully.paid),"auc")@y.values)

#4.1 - 11.97217
10*exp(0.06*3)

#4.2 - c * exp(rt) - c

#4.3 -> -c

#5.1  8.894769
loanTest$profit = exp(loanTest$int.rate*3)-1
loanTest$profit[loanTest$not.fully.paid==1]=-1
max(loanTest$profit)
max(loanTest$profit)*10

#6.1 - 0.2251, 0.2517162
loanHighInterest=subset(loanTest,int.rate>=0.15)
summary(loanHighInterest)
110/(110+327)

#6.2 - 31.27825
cutoff = sort(loanHighInterest$predicted.risk, decreasing=FALSE)[100]
selectedLoans = subset(loanHighInterest,predicted.risk<=cutoff)
sum(selectedLoans$profit)
table(selectedLoans$not.fully.paid)