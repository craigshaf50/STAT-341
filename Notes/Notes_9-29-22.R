data("NFL")

#predict y=x4.wins
#X13.OffPassYds,X14.OffPassTds,X28.OffRushYds,X67.DefTotYdsAlwd,X73.DefFumblesRecovered

#scatterplots
pairs(NFL[,c(1,10,12,25,64,70)])
all_correlations(NFL[,c(1,10,12,25,64,70)])

M<-lm(X4.Wins~X13.OffPassYds+X14.OffPassTds+X28.OffRushYds+X67.DefTotYdsAlwd+X73.DefFumblesRecovered, data=NFL)
summary(M)


complex.M<-lm(X4.Wins~X13.OffPassYds+X14.OffPassTds+X28.OffRushYds+X67.DefTotYdsAlwd+X73.DefFumblesRecovered, data=NFL)
simple.M<-lm(X4.Wins~X13.OffPassYds+X73.DefFumblesRecovered, data=NFL)
anova(simple.M,complex.M)
