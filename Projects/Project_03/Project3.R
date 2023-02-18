#Project 3: 
#Predict whether baseball player made an above average salary using variables 
#associated with performance during their previous season and over their career.

#get data set ready
library(regclass)
library(ISLR)
head(Hitters)

Hitters2<-Hitters[!is.na(Hitters$Salary),]

hist(Hitters2$Salary)
summary(Hitters2$Salary)

Hitters2$AboveAvgSalary="Yes"
for(i in 1:nrow(Hitters2)){
  if(Hitters2$Salary[i]<=536){ 
    Hitters2$AboveAvgSalary[i]="No"
  }
}
Hitters2$AboveAvgSalary<-as.factor(Hitters2$AboveAvgSalary)
head(Hitters2)

#############################
# Part A: Descriptive Model #
#############################
#1.
#view the data
view(Hitters2)

#initialize naive, preliminary, and complex models
m.naive<-glm(AboveAvgSalary~1,data=Hitters2,family='binomial')
m.prelim<-glm(AboveAvgSalary~Hits,data=Hitters2,family='binomial')
m.complex<-glm(AboveAvgSalary~(AtBat+Hits+HmRun+Runs+RBI+Walks+Assists+Errors)^2,data=Hitters2,family='binomial')

#check to see what model is predicting (It predicts probability of 'Yes')
visualize_model(m.prelim)

#forward selection
result<-step(m.prelim,scope=list(lower=m.naive,upper=m.complex),direction='forward')
summary(result)


#2.
#initialize naive, preliminary, and complex models
m.naive<-glm(AboveAvgSalary~1,data=Hitters2,family='binomial')
m.prelim<-glm(AboveAvgSalary~Runs,data=Hitters2,family='binomial')
m.complex<-glm(AboveAvgSalary~(AtBat+Hits+HmRun+Runs+RBI+Walks+Assists+Errors)^2,data=Hitters2,family='binomial')

#forward selection
result<-step(m.prelim,scope=list(lower=m.naive,upper=m.complex),direction='forward')
summary(result)


#3.
M<-glm(AboveAvgSalary~Runs+Hits+Walks+Assists+Hits*Walks+NewLeague,data=Hitters2,family='binomial')
summary(M)

exp(0.5912650) #e^(0.5912650 * 1)


#4.
exp(0.0295681*50) #e^(0.0295681 * 50)

exp(-0.0266012*50) #e^(-0.0266012*50)
1/0.2644614

#5.
#confusion matrix
confusion_matrix(M)

#            Predicted No Predicted Yes Total
# Actual No           134            24   158
# Actual Yes           40            65   105
# Total               174            89   263

#misclassification rate
(40+24)/263

#naive model
m.naive<-glm(AboveAvgSalary~1,data=Hitters2,family='binomial')
confusion_matrix(m.naive)
#            Predicted No
# Actual No           158
# Actual Yes          105

#misclassification rate
105/263

#############################
# Part B: Predictive Model  #
#############################

set.seed(12345)
ii<-sample(1:263, 50)
TRAIN<-Hitters2[-ii,]
HOLDOUT<-Hitters2[ii,]

#7.
#default all possible models search procedure 
result<-build_model(AboveAvgSalary~AtBat+Hits+HmRun+Runs+RBI+Walks+Assists+Errors+NewLeague+CAtBat+
                      CHits+CHmRun+CRuns+CRBI+CWalks,data=TRAIN,type='predictive')
result$CVtable

M<-glm(AboveAvgSalary~Hits+CHits, data=TRAIN, family="binomial")
summary(M)

generalization_error(M, HOLDOUT)

(0.1502927)*1.2

.18/0.1502927
