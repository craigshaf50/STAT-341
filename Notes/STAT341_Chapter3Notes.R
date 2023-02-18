####### CHAPTER 3######
## 9/8/22 ##
#come up with equation that relates both variables and/or makes predictions
## 3.1 philosophy of modeling
# models are an approximation, depend on accuracy of assumptions
# descriptive, goal-describe reality, estimate b0 and b1 accurately, choose set of x
# predictive, make predictions
# 3.2 regression model and interpretation
# y = b0 + b1x
# b0 = avg y at x=0, y intercept
# b1 = slope of line, how much y changes for one unit difference of x
# do not over extend interpretation of b1
# example: monthly salary to education (x=education,y=monthly salary)
library(regclass)
data("SALARY")
associate(Salary~Education,data=SALARY)
# yes, there is a moderate associacion with a statistically significant correlation
# R^2 = .168 education only explains 16.8% of monthly salary
#making linear model
M<-lm(Salary~Education,data=SALARY)
summary(M)
#estimate of b0 = 3228.83, intercept implies the avg montly salary among emps with 
# no ed is 3229
# b1 = 85.39, difference of salary between 1yr ed and 0yr is ~$85
# y = 3228.83 + 85.39x
# unless we have a census we estimate b0 and b1
plot(Salary~Education,data=SALARY,col='red',pch=19)
abline(M,lwd=2)
# example: salary to experience
head(SALARY)
plot(Salary~Experience,data=SALARY,col='red',pch=19)
associate(Salary~Experience,data=SALARY)
#spearmans corr = .292, p val = .000
M<-lm(Salary~Experience,data=SALARY)
summary(M)
# b0 = 3525.79, b1 = 10.42
# y = 3525.79 + 10.42x
# b0 someone w/ no prior experience avg monthly salary of $3525.79
# b1 for two individuals that differ by 1 yr of prior exp, we'd expect the dif in salary
# to be $10.42
# multiple R^2 = 0.02785
plot(Salary~Experience,data=SALARY,col='red',pch=19)
abline(M,lwd=2)
# use the months stat instead of exp
associate(Salary~Months,data=SALARY)
# pearsons r=.2858 p val = .002, spearmans = .2862 p val = .004
M<-lm(Salary~Months,data=SALARY)
summary(M)
# b0 =3393.07, b1 = 13.19, y = 3393.07 + 13.19x
# b0 someone w/ no months working at company avg monthly salary of 3393.07
# b1 for two individuals that differ by 1 month at comp, we'd expect the dif in salary
# to be $13.19
plot(Salary~Months,data=SALARY,col='red',pch=19)
abline(M,lwd=2)
# residual = actual y - predicted y
# residual = y - (b0 + b1x)
## SSE - refer to slides

##9/13-22##
#salary~experience
library(regclass)
data("SALARY")
M<-lm(Salary~Experience,data=SALARY)
summary(M)
# Predictions for monthly salary from experience will be off by 469 on average
# Experience accounts for 2.784% of variation in the monthly salary 
# #SEb1=6.458
# b1=10.423
# 95% CI = b1 +- 2(SEb1)
# 95% CI = (-2.493,23.339)
# two employees who differ by 1yr prior exp are expected to differ in mothly salaries by
# (-2.493,23.229) on avg
# the -2.493 tells us the person with 1 yr more exp could make 2.493 less than the person