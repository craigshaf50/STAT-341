library(regclass)
#9-22-22
data(MOVIE)
head(MOVIE)

#check equal spread
plot(Total~Weekend,data=MOVIE)
m<-lm(Total~Weekend,data=MOVIE)

#check normality
check_regression(m)
#unequal spread not normal


data(AUTO)
plot(FuelEfficiency~Horsepower,data=AUTO)

M<-lm(FuelEfficiency~Horsepower,data=AUTO)
check_regression(M)
#non linearity, not normal, 

#example
data(SALARY)
head(SALARY)
plot(Salary~Education,data=SALARY)
#linearity - fine, equal spread - fine,
M<-lm(Salary~Education,data=SALARY)
check_regression(M)
#no apparent patterns, normality - yes


plot(Salary~Experience,data=SALARY)
#linearity - no, equal spread - no,larger for lower predicted
M<-lm(Salary~Experience,data=SALARY)
check_regression(M)
#normality - yes


#best transofrmations have largest r^2
data(AUTO)
plot(FuelEfficiency~Horsepower,data=AUTO)

M<-lm(FuelEfficiency~Horsepower,data=AUTO)
check_regression(M)

M<-lm(FuelEfficiency ~ I(1/Horsepower),data=AUTO)
summary(M)
check_regression(M)
#transformation meets assumptions

#3.7 predictions
data(TIPS)
head(TIPS)

M<-lm(TipPercentage ~ Bill, data = TIPS)
summary(M)
#y^=20.68-0.23x
#rmse = 5.758
mean(TIPS$Bill)
#mean of x = 19.79
#244 observations
sd(TIPS$Bill)
#SD of x = 8.9
#SDx = 
visualize_model(M)
#predict for bill of 30$
# y^ = 20.68-.23(30) =13.78
# prediction interval = 13.78 +- 2 * 5.758((1/244)+((30-19.79)/((244-1)*(8.9^2)))
# preditction interval = 13.78 +- 2 * 

#interpretation
#I am confident that 95% of tips for a 30$ bill will be between __ and ___