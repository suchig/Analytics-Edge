#1 What is the average age in the data set?bank = read.csv("bank.csv")
mean(bank$age)
#Ans - 39.5814

#2 Which three jobs have the longest average call durations?
tapply(bank$duration,bank$job,mean)
# Ans: housemaid, self-employed, retired

# 3 Which of the following statements are correct (limited to just these selected variables)?
cor(bank[,c("emp.var.rate", "cons.price.idx","cons.conf.idx","euribor3m","nr.employed")])
#Ans cons.conf.idx has no string colinearity with other variables (smallest of all). cons. conf.idx and cons.price.idx have the lowest of all.

#4 Why do we use the sample.split() function to split into a training and testing set?
# Ans t balances the dependent variable between the training and testing sets

#5 Which of the following characteristics are statistically significantly POSITIVELY (at 0.05 level) associated with an increased chance of subscribing to the product?
set.seed(201)
library(caTools)
spl = sample.split(bank$y, 0.7)
training = subset(bank, spl==TRUE)
testing = subset(bank,spl==FALSE)
bankMod = glm(y ~ age+job+marital+education+default+housing+loan+contact+month+day_of_week+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx, data=training, family=binomial)
summary(bankMod)
#Ans - Look for variables with *, ** and *** and positive estimate. Age, August, March, poutcomenonexistent, cons.price.idx

# 6 What is the meaning of the coefficient labeled "monthmar" in the logistic regression summary output?
# Ans: Co-efficient of monthmar is 1.286. This is log(p/1-p). We have to get p to get odds. So exp(1.286)=3.618. This means given other coefficients are standard, the odds increase by 3.618-1 = 2.618 or 261.8% than an identical contact. 

#7 What is the number of test set observations where the prediction from the logistic regression model is different than the prediction from the baseline model?
predictLogTest = predict(bankMod, type="response", newdata=testing)
table(testing$y) #gives baseline model which says that 0 is success
table (predictLogTest>0.5) # shows test result

# Ans - 94. Number of cases that were noit 0.

# 8 What is the test-set AUC of the logistic regression model?
library(ROCR)
ROCRpredtest = prediction(predictLogTest,testing$y)
auc = as.numeric(performance(ROCRpredtest, "auc")@y.values)
auc
#Ans -  0.7507334

#9 What is the meaning of the AUC?
#Ans - The proportion of the time the model can differentiate between a randomly selected client who subscribed to a term deposit and a randomly selected client who did not subscribe

#10 Which logistic regression threshold is associated with the upper-right corner of the ROC plot (true positive rate 1 and false positive rate 1)?
# Ans 0, When threshold is 0 all values turn 100% True Positive and 100% False Positive

#11 At roughly which logistic regression cutoff does the model achieve a true positive rate of 60% and a false positive rate of 25%?
ROCRperf = performance(ROCRpredtest, "tpr","fpr")
plot(ROCRperf,colorize=TRUE,print.cutoffs.at=seq(0,1,0.05),text.adj=c(-0.2,1.7))
#Ans - The points are between 0.1 and 0.15, hence answer should be 0.11. The other way to see this isusing color.

#12 Which of the following best describes how 10-fold cross-validation works when selecting between 4 different parameter values?
# For each parameter, 10 sets of data is used to train and a small subset is used to test. So totally 40

#13 What cp value maximizes the cross-validation accuracy?
set.seed(201)
numFolds=trainControl(method="cv", number=10)
cpGrid=expand.grid(.cp=seq(0.001,0.05,0.001))
tr = train(y ~ age+job+marital+education+default+housing+loan+contact+month+day_of_week+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx, data=training,method="rpart",trControl=numFolds,tuneGrid=cpGrid)
tr
#Ans - I got 0.012 based on suggestion from tr output. But answer was 0.016

#14 What variable is used as the first (upper-most) split in the tree?
treeCPMod=rpart(y ~ age+job+marital+education+default+housing+loan+contact+month+day_of_week+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx, data=training,method="class",cp=0.012)
prp(treeCPMod)
#Ans - emp.rate

#15 What is the accuracy of your CART model?
treeCPMod=rpart(y ~ age+job+marital+education+default+housing+loan+contact+month+day_of_week+campaign+pdays+previous+poutcome+emp.var.rate+cons.price.idx+cons.conf.idx, data=training,cp=0.012)
prp(treeCPMod)
predictCPTest = predict(treeCPMod, newdata=testing)
table(testing$y, predictCPTest>= 0.5)
(1293+37)/(1293+30+140+37)
#Ans 0.8866667
