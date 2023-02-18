library('boot')
library(regclass)
head(melanoma)

melanoma.uncensored <- melanoma[melanoma$status==1,c(1,3:7)]
head(melanoma.uncensored)

class(melanoma.uncensored$sex)<-"character"
class(melanoma.uncensored$ulcer)<-"character"
str(melanoma.uncensored)


#question 1:
pairs(time~age+year+thickness, data = melanoma.uncensored)
all_correlations(melanoma.uncensored)
associate(time~thickness,data=melanoma.uncensored)

associate(time~year, data = melanoma.uncensored)
associate(time~age, data = melanoma.uncensored)

associate(time~sex,data=melanoma.uncensored)
associate(time~ulcer,data=melanoma.uncensored)

#question 2:
M<-lm(time~thickness,data=melanoma.uncensored)
summary(M)

check_regression(M)

find_transformations(M)

M.1<-lm(I(time^-0.25)~I(thickness^0.25),data=melanoma.uncensored)
check_regression(M.1)
M.2<-lm(log10(time)~I(thickness^0.25),data=melanoma.uncensored)
check_regression(M.2)
summary(M.2)

M<-lm(time~thickness,data=melanoma.uncensored)
summary(M)

mean(melanoma.uncensored$thickness)

avg.tumor<-data.frame(thickness=(4.311053))
predict(M,newdata=avg.tumor,interval='prediction',level=.95)
predict(M,newdata=avg.tumor,interval='confidence',level=.95)
