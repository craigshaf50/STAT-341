library(regclass)
data(BULLDOZER)

M<-lm(SalePrice ~ ., data=BULLDOZER)
summary(M)
check_regression(M,extra=T)
VIF(M)

#partial F test
simple.M<-lm(SalePrice ~ YearsAgo + YearMade + Tire, data=BULLDOZER)
complex.M<-lm(SalePrice ~ ., data=BULLDOZER)
anova(simple.M,complex.M)

M<-lm(SalePrice ~ YearsAgo + YearMade + Tire, data=BULLDOZER)
summary(M)
check_regression(M,extra=T)
#RMSE 22100 R2 .5806

M<-lm(SalePrice ~ YearsAgo + YearMade + Tire + I(YearMade^2), data=BULLDOZER)
summary(M)
check_regression(M,extra=T)
#Still not fixed assumptions
#RMSE 21270 R2 .6121

M<-lm(SalePrice ~ YearsAgo + YearMade + Tire + I(YearMade^2), data=BULLDOZER)
summary(M)
check_regression(M,extra=T)
