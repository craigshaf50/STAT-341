library(regclass)
data(BODYFAT)
## 9/20/22
head(BODYFAT)

plot(BodyFat~Age, data=BODYFAT)

M<-lm(BodyFat~Age, data=BODYFAT)
summary(M)

plot(BodyFat~Age, data=BODYFAT)
abline(M,col='red')
#y=10.955+.178x, y=bodyfat, x=age

#extrapolating because everyone is over 20
#B0 = 10.955, Someone zero years old will have an average bodyfat% of 10.955%
#B1 = b1 = 0.178, For two people that differ by one year of age, we expect the older
# person to have a higher bodyfat % by .178%

#R^2 = .08, 8% of the variation in bodyfat% is explained by age
#RMSE = 7.435, predictions will be off by 7.4% on average **NOT A GOOD FIT**
#outliers? not really

#95% CI for B1: b1 +- 2SEb1
#0.178-+2(0.0372)=(.1408, .2524) ##percentages

possible_regressions(M)
#regression is statistically significant pval=0, not practically significant
#age significant, t val =4.776, pval = 0, YES significant


## predict from abdomen
M<-lm(BodyFat~Abdomen, data=BODYFAT)
summary(M)

plot(BodyFat~Abdomen, data=BODYFAT)
abline(M,col='red')
#linear relationship? YES

#reg equation -> y= -35.20 + .585x
#y= bodyfat, x= abdomen (cm)
#b0 = -35.2, b1 = .585
#Someone with 0cm abdomen to have -35% bodyfat
#Two people who differ by 1cm abdomen will differ by .585% bodyfat on avg

#R^2: 66% of variation in bodyfat% can be explained by abdomen
#RMSE: 4.514, predictions will be off by 4.5% on avg
#outliers? 1 yes
#Good fit? **GOOD FIT**

#95% CI: .585+-2(0.02643) = (0.532,0.638)
# since both positive, higher abdomen higher bodyfat %

possible_regressions(M)
#regression statistically significant? pval=0, YES SIGNIFICANT
#abdomen significant? t =22.13, pval = 0, YES SIGNIFICANT

##3.5 checking assumptions

#linearity
data(AUTO)
plot(FuelEfficiency~Horsepower,data=AUTO)

M<-lm(FuelEfficiency~Horsepower,data=AUTO)
check_regression(M)
#curve pattern in residual plot. linearity-NONLINEAR

#equal spread









