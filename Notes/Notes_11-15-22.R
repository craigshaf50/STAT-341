#11-15
library(regclass)

data("POISON")
head(POISON)

M<-glm(Outcome~Dose,data=POISON,family='binomial')

visualize_model(M)

summary(M)




data("ACCOUNT")
head(ACCOUNT)

M<-glm(Purchase~Tenure+Age+SavingBalance,data=ACCOUNT,family='binomial')

summary(M)
#age not significant -can recmove

M<-glm(Purchase~Tenure+SavingBalance,data=ACCOUNT,family='binomial')

summary(M)


simple.M<-glm(Purchase~1,data=ACCOUNT,family='binomial')
complex.M<-glm(Purchase~Tenure+SavingBalance,data=ACCOUNT,family='binomial')
anova(simple.M,complex.M,test='Chisq')
#p-val ~ 0, model is significant

#add interaction, it is significant
M<-glm(Purchase~Tenure*SavingBalance,data=ACCOUNT,family='binomial')
summary(M)
visualize_model(M,'bottomleft')



#11-17-22
M<-glm(Purchase~Tenure*SavingBalance+Area.Classification,data=ACCOUNT,family='binomial')
summary(M)
drop1(M,test='Chisq')
#area.classification is significant, interaction is significant

confusion_matrix(M)

simple.M<-glm(Purchase~1,data=ACCOUNT,family='binomial')

confusion_matrix(simple.M)
#________________________________________________________#
install.packages('ISLR')
library(ISLR)

head(Default)

#predict whether person will default on credit card payment based on annual income,
#monthly balance, and student

#logistic regression will model: p=probability person will default (y=1=Yes)
M<-glm(default~balance,data=Default,family='binomial')
summary(M)

confusion_matrix(M)
#           Predicted No Predicted Yes Total
#Actual No          9625            42  9667
#Actual Yes          233           100   333
#Total              9858           142 10000

#misclassification rate = 233+42/10000 = .0275

#more false negatives

#naive
naive.M<-glm(default~1,data=Default,family='binomial')
confusion_matrix(naive.M)
#Predicted levels same as naive model (majority level)
#            Predicted No
#Actual No          9667
#Actual Yes          333


#add student
M<-glm(default~balance+student,data=Default,family='binomial')
summary(M)
visualize_model(M) #students are less likely to default

confusion_matrix(M)
#           Predicted No Predicted Yes Total
#Actual No          9628            39  9667
#Actual Yes          228           105   333
#Total              9856           144 10000

#misclassification rate = 228+39/10000 = .0267