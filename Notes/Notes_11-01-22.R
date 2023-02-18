library(regclass)
data(SALARY)

plot(Salary~Gender, data=SALARY)

str(SALARY)

M<-lm(Salary~Education+Gender, data=SALARY)
summary(M)
#fitted model: y = 3212 + 54 Education + 461 GenderMale
#interpret gender: 461.21 <- difference in intercepts for separate regression lines for male and females.
#                                                               Implicit regression equations
#Female (x2=0) = y=3212 + 54 education
#Male (x2=1) = y=(3212+461) + 54 education = 3673 + 54 education
#For two people with the same level of education, the male would earn 461.21 more on average, than  the female.
#model is significant, so are predictors

visualize_model(M)

#try adding interaction term
M<-lm(Salary~Education*Gender, data=SALARY)
summary(M)
visualize_model(M)
#interaction term is not significant and did not really improve the model. 
#fitted model y = 3261.58 + 41.28 edu + 209.22 Gender + 49.06 Edu:Gender
#Implicit regression equations
#female:  y = 3261.58 + 41.28 edu
#  two women who differ in 1 year of education are suspected to differ in salary by 41.28 on average
#male: y = (3261.58 + 209.22) + (41.28 + 49.06)Edu = 3470.8 + 90.48 Edu
#  two men who differ by 1 year of education are suspespected to differ in salary by 90.48 on average

#try adding experience and the interacrion between education and experience
M<-lm(Salary~Experience*Education, data=SALARY)
summary(M)
#interaction is significant on its own
#with gender back in
M<-lm(Salary~Experience*Education+Gender, data=SALARY)
summary(M)
#now the interaction term is not significant, can remove
M<-lm(Salary~Experience+Education+Gender, data=SALARY)
summary(M)
#try interaction with experience and gender
M<-lm(Salary~Experience*Gender, data=SALARY)
summary(M)
visualize_model(M)
#interaction not significant




#TIPS
data("TIPS")
M<-lm(TipPercentage~Bill+Weekday, data=TIPS)
summary(M)
visualize_model(M)
#dummy variables do not appear significant
#fitted model: y    =     21.11      -0.24 Bill    -0.90 WeekdaySat         +      0.71 WeekdaySun    -0.75 WeekdayThurs
#                  Friday Intercept    Slope       diff in avg tip% between      Diff in avg tip%       diff between fri and thurs
#                                                 Friday and saturday          between fri and sun
#interpretations all go back to the reference level (Friday)
#difference between saturday and sunday: (WeekdaySat-WeekdaySun) = -1.61


#significance of group of dummy variables? (visualize model or drop1)
#  Effect test = partial F-test
#  drop1 function -> drop1(M, test = F)
drop1(M,test='F')
# pval for weekday (three dummy variables not significant) = .3003, not significant and can be removed
# day of week does not significantly impact tip%
#keep all or remove all <- only options for dummy variables 