#12-1
#CH8 - predictive modeling
library(regclass)
insurance <-read.csv(file="https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv")
nrow(insurance)
#1338 rows    265~20%
set.seed(123)
ii<-sample(1:1338,265)

holdout<-insurance[ii,]
train<-insurance[-ii,]

result<-build_model(charges~.,data=train,type='predictive')
result$CVtable

summary(result$bestmodel)

m<-lm(charges~age+bmi+smoker,data=insurance)
generalization_error(m,holdout)
#holdout RMSE less than 20% higher than validation RMSE