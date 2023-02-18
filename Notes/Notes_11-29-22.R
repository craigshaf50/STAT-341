library(regclass)
data("BODYFAT")

#nbest=2^k=2^13=8192
#if more than 50 models really.big=true
result<-regsubsets(BodyFat~.,data=BODYFAT,method='exhaustive',nbest=8192,really.big = TRUE)
see_models(result,report=20)

#might prefer the model with 5 var because it is close to best and much simpler





m.naive<-lm(BodyFat~1,data=BODYFAT)
m.prelim<-lm(BodyFat~Weight,data=BODYFAT)
m.complex<-lm(BodyFat~.,data=BODYFAT)

#forward
result<-step(m.prelim,scope=list(lower=m.naive,upper=m.complex),direction='forward')
summary(result)

#backward
result<-step(m.complex,scope=list(lower=m.naive,upper=m.complex),direction='backward')
summary(result)

#stepwise
result<-step(m.prelim,scope=list(lower=m.naive,upper=m.complex),direction='both')
summary(result)





#12-1
#salary -build a predictive model to estimate salary
data("SALARY")
#ALL POSSIBLE MODELS
result<-regsubsets(Salary~.,data=SALARY,method='exhaustive',nbest = 16, really.big = T)
see_models(result,report=15)
#pick the second best model, simpler and only 2 more AIC
#Has education, months, gender

m.naive<-lm(Salary~1,data=SALARY)
m.prelim<-lm(Salary~Experience,data=SALARY)
m.complex<-lm(Salary~.^2,data=SALARY) #include all 2 way interactions

#forward
result<-step(m.prelim,scope=list(lower=m.naive,upper=m.complex),direction='forward')
summary(result)
#choose the model with 1088.1 instead of best one
#Salary ~ Experience + Gender + Months + Education

#backward
result<-step(m.complex,scope=list(lower=m.naive,upper=m.complex),direction='backward')
summary(result)


#stepwise
result<-step(m.prelim,scope=list(lower=m.naive,upper=m.complex),direction='both')
summary(result)





#TIPS
data("TIPS")

m.naive<-lm(TipPercentage~1,data=TIPS)
m.prelim<-lm(TipPercentage~Bill,data=TIPS)
m.complex<-lm(TipPercentage~(Bill+PartySize+Weekday+Smoker)^2,data=TIPS) 

#forward
result<-step(m.prelim,scope=list(lower=m.naive,upper=m.complex),direction='forward')
summary(result)

#backward
result<-step(m.complex,scope=list(lower=m.naive,upper=m.complex),direction='backward')
summary(result)

#stepwise
result<-step(m.prelim,scope=list(lower=m.naive,upper=m.complex),direction='both')
summary(result)
