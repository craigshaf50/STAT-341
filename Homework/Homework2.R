library(regclass)
library(tidyverse)

data("EX3.NFL")
correlations <-all_correlations(EX3.NFL)
cor<-correlations[264:393, ]
cor %>% arrange(desc(correlation))

M<-lm(Wins~X1.Off.Tot.Yds,data=EX3.NFL)
plot(Wins~X1.Off.Tot.Yds,data=EX3.NFL)
abline(M,col='red')

summary(M)

M<-lm(Wins~X82.Def.Fumbles.Recovered,data=EX3.NFL)
summary(M)

data("EX3.ABALONE")
plot(Meat.Weight~Diameter, data=EX3.ABALONE, pch=20,cex=.3)

M<-lm(Meat.Weight~Diameter, data=EX3.ABALONE)
visualize_model(M)

find_transformations(M)

M.trans<-lm(I(1/((Meat.Weight)^.75))~I(1/(Diameter^2.25)), data=EX3.ABALONE)
summary(M.trans)

check_regression(M.trans)
plot(I(1/((Meat.Weight)^.75))~I(1/(Diameter^2.25)), data=EX3.ABALONE)

plot(fitted(M.trans),residuals(M.trans),xlim=c(1.5,6),ylim=c(-4,4),pch=20,cex=0.3)

data("EX3.BODYFAT")
M<-lm(Fat~Triceps, data = EX3.BODYFAT)
summary(M)

visualize_model(M)
to.pred<-data.frame(x=2)
to.pred<-data.frame(Triceps=c(18,31))
predict(M,newdata=to.pred,interval='confidence',level=.95)

predict(M,newdata=to.pred,interval='predict',level=.95)

mean(EX3.BODYFAT$Triceps)
sd(EX3.BODYFAT$Triceps)
