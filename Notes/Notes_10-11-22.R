library(regclass)

data(AUTO)
head(AUTO)

#EX1
M<-lm(TopSpeed~Weight+Horsepower,data=AUTO)
VIF(M)

M<-lm(TopSpeed~Weight+Horsepower+FuelEfficiency,data=AUTO)
summary(M)
VIF(M)

data("BULLDOZER")

plot(SalePrice~YearMade, data=BULLDOZER)
plot(SalePrice~Tire, data=BULLDOZER)
plot(YearMade~Tire, data=BULLDOZER)

M<-lm(SalePrice~YearMade+Tire, data=BULLDOZER)
check_regression(M,extra=TRUE)

M<-lm(SalePrice~YearMade+Tire+I(YearMade^2), data=BULLDOZER)
check_regression(M,extra=TRUE)
summary(M)
