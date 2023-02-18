library(regclass)
data("SALARY")

M<-lm(Salary~Education+Experience,data=SALARY)
visualize_model(M)

#with interaction term
M<-lm(Salary~Education+Experience+Education*Experience,data=SALARY)
M<-lm(Salary~Education*Experience,data=SALARY) #shortcut notation
summary(M)
#Education*Experience is significant, p-val=.01341



data("TIPS")
head(TIPS)

#difference between I() and without
M<-lm(TipPercentage~Bill*PartySize,data=TIPS)
M<-lm(TipPercentage~I(Bill*PartySize),data=TIPS) #just interaction variable, dont use

M<-lm(TipPercentage~Bill*PartySize,data=TIPS)
summary(M)

cor_matrix(TIPS)



data("BODYFAT")
cor_matrix(BODYFAT)

M<-lm(BodyFat~Abdomen+Chest+Hip+Weight+Thigh+Knee,data=BODYFAT)
summary(M)
#Chest, Hip, Knee not significant

complex.M<-lm(BodyFat~Abdomen+Chest+Hip+Weight+Thigh+Knee,data=BODYFAT)
simple.M<-lm(BodyFat~Abdomen+Weight+Thigh,data=BODYFAT)
anova(simple.M,complex.M)
#p-value is .66, we can remove all 3

#three way interaction
M<-lm(BodyFat~Abdomen*Weight*Thigh,data=BODYFAT)
summary(M)

#can we remove interaction terms?
simple.M<-lm(BodyFat~Abdomen+Weight+Thigh,data=BODYFAT)
complex.M<-lm(BodyFat~Abdomen*Weight*Thigh,data=BODYFAT)
anova(simple.M,complex.M)
#p-val= 0.005, No, at least one interaction term should stay in the model

#remove 3 way interaction? yes
M<-lm(BodyFat~Abdomen+Weight+Thigh+Abdomen*Weight+Abdomen*Thigh+Weight*Thigh,data=BODYFAT)
summary(M)

#see if we can remove other interaction terms
simple.m<-lm(BodyFat~Abdomen+Weight+Thigh+Weight*Thigh,data=BODYFAT)
anova(simple.m,M)
#p val is .61 can remove Abdomen*Weight and Abdomen*Thigh

#remove Abdomen*Weight and Abdomen*Thigh
M<-lm(BodyFat~Abdomen+Weight+Thigh+Weight*Thigh,data=BODYFAT)
summary(M)
#Weight*Thigh is significant, so even though thigh is not significant it must stay
#R^2 is .7344 and RMSE is 3.995
#estimates off by 3.995% on average

#drop abdomen to look at interaction plots
M<-lm(BodyFat~Weight+Thigh+Weight*Thigh,data=BODYFAT)
visualize_model(M)
#interaction term p val is .0007304
see_interactions(M)
#if slopes are different then interaction terms should be in the model
#y=-60.53 + 0.39x1 + 0.88x2 -0.003x1x2
#interpret b3: amount of change in slope (describes relationship between weight and bodyfat%).
#for each unit increase in thigh measurement 

#leave out interaction
M<-lm(BodyFat~Abdomen+Weight+Thigh,data=BODYFAT)
summary(M)
#even though interaction was significant, it doesn't really improve the model by much
#better off excluding it because a simple model is better


