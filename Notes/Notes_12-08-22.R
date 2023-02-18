install.packages("ISLR")
install.packages("NHANES")

library(ISLR)
library(NHANES)

View(Hitters)
View(NHANES)


library(regclass)
data(WINE)
head(WINE)

2700 observations
Choose about 20% for the hold out sample (540 observations)

set.seed(1234)
ii<-sample(1:2700, 540)
HOLDOUT<-WINE[ii,]
TRAIN<-WINE[-ii,]

result<-build_model(Quality~., data=TRAIN, type="predictive", 
                    method="backward", seed=123)
help(build_model)

summary(result$bestmodel)
result$bestformula

M<-glm(Quality ~ fixed.acidity + volatile.acidity + residual.sugar + 
         density + pH + sulphates, data=TRAIN, family="binomial")
generalization_error(M, HOLDOUT)
help(WINE)

confusion_matrix(M)

missclassification rate = (211+179)/2160 = 0.18
False negative - 211
False positive - 179

M.naive<-glm(Quality ~ 1, data=TRAIN, family="binomial")
confusion_matrix(M.naive)
missclassification rate = (841)/2160 = 0.39

data("EX5.DONOR")
M<-glm(Donate~ TotalDonations, data=EX5.DONOR, family="binomial")
summary(M)

-(-1.355629/0.024351)

data("EX6.CLICK")
head(EX6.CLICK)

M<-glm(Click ~ BannerPosition+ SiteID+ SiteCategory+ AppDomain+
         AppCategory+ DeviceModel, data=EX6.CLICK, family = "binomial")
summary(M)
drop1(M, test="Chisq")

data("EX6.WINE")
M<-glm(Quality ~ volatile.acidity * alcohol, 
       data=EX6.WINE, family="binomial")
visualize_model(M)
