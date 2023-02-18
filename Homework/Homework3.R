library(regclass)
#Q1
data("EX4.BIKE")
head(EX4.BIKE)

pairs(Demand~AvgTemp+EffectiveAvgTemp+AvgHumidity+AvgWindspeed,data=EX4.BIKE)

AvgTemp.M<-lm(Demand~AvgTemp, data=EX4.BIKE)
summary(AvgTemp.M)

EffectiveAvgTemp.M<-lm(Demand~EffectiveAvgTemp, data=EX4.BIKE)
summary(EffectiveAvgTemp.M)

AvgHumidity.M<-lm(Demand~AvgHumidity, data=EX4.BIKE)
summary(AvgHumidity.M)

AvgWindspeed.M<-lm(Demand~AvgWindspeed, data=EX4.BIKE)
summary(AvgWindspeed.M)

M<-lm(Demand~AvgTemp+EffectiveAvgTemp+AvgHumidity+AvgWindspeed,data=EX4.BIKE)
summary(M)

RemBoth.M<-lm(Demand~AvgHumidity+AvgWindspeed,data=EX4.BIKE)
anova(RemBoth.M,M)

VIF(M)
check_regression(M)

M1<-lm(Demand~EffectiveAvgTemp+AvgHumidity+AvgWindspeed,data=EX4.BIKE)
summary(M1)

M2<-lm(Demand~AvgTemp+AvgHumidity+AvgWindspeed,data=EX4.BIKE)
summary(M2)

check_regression(M,extra=TRUE)
#Q2
data("EX4.STOCKS")
head(EX4.STOCKS)

M<-lm(AA~., data =  EX4.STOCKS)
check_regression(M)
summary(M)

VIF(M)

data("EX4.STOCKPREDICT")
M<-lm(AA~., data =  EX4.STOCKPREDICT)
PI<-predict(M,interval="prediction",level=.95)
PI<-as.data.frame(PI)
PI[,"Difference"]= PI$upr-PI$lwr

mean(PI$Difference)




