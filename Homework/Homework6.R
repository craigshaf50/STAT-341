library(regclass)


#Homework 6

#Q1
data("EX6.DONOR")
head(EX6.DONOR)
M<-glm(Donate~TotalDonations,data=EX6.DONOR,family='binomial')

visualize_model(M)

summary(M)

#Q2
data("EX6.CLICK")
M<-glm(Click~BannerPosition+SiteID+SiteCategory+AppDomain+AppCategory+DeviceModel,data=EX6.CLICK,family='binomial')
summary(M)

drop1(M,test="Chisq")

M.naive<-glm(Click~1,data=EX6.CLICK,family='binomial')
confusion_matrix(M.naive)

confusion_matrix(M)

#Q3
data("EX6.WINE")
head(EX6.WINE)
M<-glm(Quality~volatile.acidity+alcohol,data=EX6.WINE,family='binomial')
visualize_model(M,loc='left')

M<-glm(Quality~volatile.acidity*alcohol,data=EX6.WINE,family='binomial')
visualize_model(M,loc='left')
summary(M)

M.naive<-glm(Quality~1,data=EX6.WINE,family='binomial')
confusion_matrix(M.naive)
confusion_matrix(M)

M<-glm(Quality~alcohol+chlorides,data=EX6.WINE,family='binomial')
visualize_model(M)

M<-glm(Quality~alcohol*chlorides,data=EX6.WINE,family='binomial')
visualize_model(M)





