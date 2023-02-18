library(regclass)
data("BULLDOZER2")

M<-lm(Price~Usage*Decade, data=BULLDOZER2)
summary(M)
visualize_model(M)
drop1(M,test='F')

#HW5 
data('EX5.BIKE')
#1c
M<-lm(Demand~EffectiveAvgTemp + Weather, data = EX5.BIKE)
visualize_model(M)
summary(M)
#statistically significant
#demand is higher by 656 on days with no rain
#1a
associate(Demand~Weather,data=EX5.BIKE)
#5048 - 4141 = 907 part a
#different by 907 on average

#c is noticeably different from a 