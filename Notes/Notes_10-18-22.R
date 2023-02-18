library(regclass)
data("SOLD26")
head(SOLD26)

#predict soldweek26

M<-lm(SoldWeek26~.,data=SOLD26)
summary(M)

check_regression(M,extra=T)
#eunequal spread, not normal, curvatue - Violation for all 3

#natural log transform
M<-lm(I(log(SoldWeek26))~.,data=SOLD26)
check_regression(M,extra=T)
#everything got worse

#book model with thier own variable
M<-lm(SoldWeek26~I((Sold13/StoresSelling13)*StoresSelling26),data=SOLD26)
check_regression(M,extra=T)

#now transofrm y
M<-lm(I(log(SoldWeek26))~I((Sold13/StoresSelling13)*StoresSelling26),data=SOLD26)
check_regression(M,extra=T)
#transform x too
M<-lm(I(log(SoldWeek26))~I(log((Sold13/StoresSelling13)*StoresSelling26)),data=SOLD26)
check_regression(M,extra=T)
#fixed first plot, random scatter. just need to fix normality

#is the model significant? yes
summary(M)
# ln(y)= -.119 +1.016(ln((x1/x2)*x3))

# simple regression with log-log transformation
# e^ln(y) = e^-.119 +1.016(ln((x1/x2)*x3))
# y = e^(-.119 +1.016(ln((x1/x2)*x3)))
# y = e^(-.119) * ((x1/x2)*x3)^(1.016) #power model