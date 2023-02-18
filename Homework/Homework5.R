library(regclass)
data('EX5.BIKE')

#Q1
associate(Demand~Weather,data=EX5.BIKE)

M<-lm(Demand~EffectiveAvgTemp + Weather, data = EX5.BIKE)
visualize_model(M)
summary(M)

M<-lm(Demand~EffectiveAvgTemp + Day, data = EX5.BIKE)
visualize_model(M)
summary(M)

M<-lm(Demand~EffectiveAvgTemp + Day, data = EX5.BIKE)
summary(M)

M<-lm(Demand~., data = EX5.BIKE)
summary(M)
drop1(M,test='F')
#Q3
data('EX5.DONOR')
M<-lm(LastAmount~RecentAvgAmount*Homeowner, data = EX5.DONOR)
visualize_model(M)
summary(M)

M<-lm(LastAmount~RecentAvgAmount*Setting+AccountAge, data = EX5.DONOR)
summary(M)
drop1(M,test='F')
