library(regclass)
data("AUTO")

#Question 3
pairs(FuelEfficiency~., data=AUTO)

M<-lm(FuelEfficiency~., data=AUTO)
summary(M)
check_regression(M)


M.p1<-lm(FuelEfficiency~Weight+CabVolume+TopSpeed+Horsepower+I(CabVolume^2)+I(CabVolume^3)
         +I(TopSpeed^2)+I(TopSpeed^3)+I(Horsepower^2)+I(Horsepower^3) ,data=AUTO)
summary(M.p1)

M.p2<-lm(FuelEfficiency~Weight+CabVolume+TopSpeed+Horsepower+I(CabVolume^2) ,data=AUTO)
summary(M.p2)

M.poly<-lm(FuelEfficiency~Weight+CabVolume+TopSpeed+Horsepower+I(CabVolume^2) ,data=AUTO)
summary(M.poly)

anova(M,M.poly)

check_regression(M.poly,extra=T)

#Question 4
data("EX4.BIKE")

M<-lm(Demand~.,data=EX4.BIKE)
summary(M)
influence_plot(M)

M<-lm(Demand~ ., data=EX4.BIKE[-337,])
summary(M)

M<-lm(Demand~ .^2, data=EX4.BIKE[-337,])
see_interactions(M)

M<-lm(Demand~AvgTemp*AvgWindspeed, data=EX4.BIKE[-337,])
summary(M)
visualize_model(M)

#Question A and B
data("STUDENT")
M<-lm(CollegeGPA~HSGPA*JobHours,data=STUDENT)

see_interactions(M)
summary(M)

M<-lm(ACT~HSGPA*Honors*HSClubs,data=STUDENT)
summary(M)
see_interactions(M)

M<-lm(ACT~HSGPA*Honors+HSClubs*HSGPA+Honors*HSClubs,data=STUDENT)
summary(M)

M<-lm(ACT~HSGPA+Honors,data=STUDENT)
summary(M)

M1<-lm(ACT~HSGPA+Honors,data=STUDENT)
#summary(M1)

M2<-lm(ACT~HSGPA*Honors,data=STUDENT)
summary(M2)
anova(M1,M2)
