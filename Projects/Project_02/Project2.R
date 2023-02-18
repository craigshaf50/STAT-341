#project 2 code & commentary

#load data
library(regclass)
insurance <-read.csv(file="https://raw.githubusercontent.com/stedy/Machine-Learning-with-R-datasets/master/insurance.csv")


###############################################
# Table of contents:                  (line)  #
# ~~~~~~~~~~~~~~~~~~                          #
# Section 1: variable exploration       20    #
# Section 2: initial model building    135    #
# Section 3: best three models         422    #
# Section 4: best model decision       462    #
###############################################



##----------------------------------##
## Section 1: variable exploration: ##
##----------------------------------##
head(insurance)
#data has the variables age, sex, bmi, children, smoker, region, charges
#charges is the variable we are trying to predict (it is numeric)

str(insurance)
#age, bmi, children are numeric
#sex, smoker, region are categorical

summary(insurance$charges)
#min 1122, max 63770, mean 13270
#we should restrict predictions and conclusions to this range so
#we don't risk extrapolating
hist(insurance$charges)
#very right skewed. Most values fall between 0 and 15000.
#should consider limiting our predictions and conclusions.
#might want to consider transforming charges for building our model.
hist(log(insurance$charges))
#a log transformation makes the distribution mostly normal
#we should conisder this for building the model

summary(insurance$age)
#lowest value is 18, highest value is 64, mean is 39.21
#we should restrict predictions and conclusions to this range so
#we don't risk extrapolating
hist(insurance$age)
#distribution relatively even (uniform) across buckets ~140 each
#except for under 20 (18-20) has the most ~165
#and over 60 (61-64) has the least ~100

summary(insurance$bmi)
#min 15.96, max 53.13, mean is 30.66
#we should restrict predictions and conclusions to this range so
#we don't risk extrapolating
hist(insurance$bmi)
#distribution is very normal. There is a slight tail beyond 50,
#but I don't think it is enough to effect the data.

summary(insurance$children)
#min 0, max 5, mean is 1.095
#we should restrict predictions and conclusions to this range so
#we don't risk extrapolating
hist(insurance$children)
#distribution very right skewed. The peak is at 0 with over ~600 observations,
#1 has over ~310 observations, 2 has ~280 observations, 3 has ~190 observations,
#4 has ~30 observations, and 5 has ~20 observations.
#it may be helpful to transform this variable to pull in our values.

#exploring relationship between categorical variables and charges
associate(charges~sex,data=insurance)
#Anova believes the relationship is significant (p-value < 0.05)
#however there appears to be several outliers so we should consider other tests
#Kruskal and Median test does not (p-value > 0.05)
#distribution of charges appears similar for both male and female
boxplot(charges~sex,data=insurance)
#male appears to have a slightly wider range of charges. 
#the average values are very similar.
#this variable does not appear too helpful based on the box plot,
#but I will still consider it for building the model

associate(charges~smoker,data=insurance)
#all tests describe smoker as significant
#The yes sample size is a 274 vs no sample size of 1064
#distributions are very different. No is right skewed and yes is somewhat bimodal
#yes has a much higher average charge (32050 vs 8434). 
#the difference leads me to believe that this variable will be useful for building
#a model to predict charges.

associate(charges~region,data=insurance)
#average charges appear to be similar for northwest and southwest (12418 and 12347)
#charges differ for northeast and southeast (13406 and 14735) 
#northeast and southeast have wider ranges than northwest and southwest
#Anova believes the relationship is significant (p-val=0.026) but the other tests
#do not (p-val>0.05). There are several outliers so the other tests are likely more accurate than anova
#I will consider this variable but it does not appear to be significant on its own.


#checking correlation for numeric variables to charges
pairs(charges~age+bmi+children,data=insurance)
#charges and age appear linearly correlated. However, there
#appears to be 3 distinct lines in the data. This leads me to
#believe there is another underlying relationship in the data
#charges and bmi has one big blob of points at the lower values with
#a smaller second blob of points in the upper middle of the plot.
#this tells me that we will likely have to transform this variable or
#charges.
#the charges and children plot doesn't tell us too much. It appears
#that the people with 5 children have a much smaller amount to pay in most
#observations. While people with 0-4 have a wide range of charges.

#correlation values for numeric variables
cor<-all_correlations(insurance)
cor
#age and charges are the most heavily correlated (.29).
#bmi and charges is second most correlated (.20)
#age and bmi is third most correlated (.11)
#children and charges is third least correlated (.07) (out of 6 correlations)
#this information tells me that age and bmi will be helpful when predicting 
#charges. However, we need to remember to consider problems with multicollinearity 
#if age and bmi provide similar information. Especially if we transform bmi and 
#it gains a more linear relationship with charges. I should also consider interactions
#between the variables.


#Overall, it appears that the variables smoker and age will be most useful for
#predicting charges. This makes sense, because older people who smoke are at more risk
#to health complications than someone young who doesn't smoke.
#However, as I move into further testing, other variables might 
#present themselves as useful and provide new insight for predicting charges.

#Additionally, we should consider transforming charges with log() because it appeared to
#normalized the distribution.

##------------------------------------##
## Section 2: initial model building: ##
##------------------------------------##

#~~~~~~~~~~~~~~~~~~#
# Model Attempt 1: #
#~~~~~~~~~~~~~~~~~~#

#first I'm going to make an initial model containing all variables and evaluate their significance
#then I'm going to evaluate model assumptions
M<-lm(charges~age+sex+bmi+children+smoker+region,data=insurance)
summary(M)
#age, children, and smokeryes all have significant p-values (2e-16 = ~0 < 0.05)
#sexmale appears not significant when these variables are included in the model
#to find the significance of region and other categorical variables that were 
#Adjusted R-squared:  0.7494, the model explains 75% of the variance in charges
#Residual standard error: 6062, typical error committed is $6062
#F-statistic: 500.8 on 8 and 1329 DF,  p-value: < 2.2e-16, Model is significant (p-val<0.05)
drop1(M,test='F')
#the variables region (p-val=0.096221) and sex (p-val=0.693348) aren't significant in the model
#with the other variables

#checking model assumptions
check_regression(M,extra=T)
#fails linearity based on residual plot, it looks like there 2 different curves going on between 3 groups 
#fails normality based on qq plot, line of residuals does not stay in bounds
#bmi appears to be the biggest offender causing the plot to have distinct patterns in the residual plot
#should consider adding a polynomial term for bmi.

#Based on the model assumptions I'm going to try transforming charges to log(charges) in attempt to see if 
#we can make assumptions better. It may appear that I'm forgetting to remove insignificant variables from 
#the last model, but I'd like to see if this transformation may alter the significance of variables.
M<-lm(log(charges)~age+sex+bmi+children+smoker+region,data=insurance)
summary(M)
#all variables and dummy variables with the exception of regionnorthwest (p-value = .067860) 
# appears to be significant (p-val < 0.05).
#Residual standard error: 0.4443, the typical error committed by the model is 0.4443
#that number is misleading as we would have to untransform the model to compare it with the other
#value.
#Adjusted R-squared:  0.7666, the model explains 76.7% of the variation in charges
#the R2 does tell us the model improved from the last version.
#549.8 on 8 and 1329 DF,  p-value: < 2.2e-16, Model is significant (p-val<0.05)
#F statistic also got bigger, telling us that it is a little more significant than the previous model
drop1(M,test='F')
#all predictors came back as significant (p-values < 0.05)
#the lowest p-value of the variables was sex with p-value of 0.002038
#drop1 also tells us that the model RSS is 262.
#removing sex only increases that number to 264 -we should consider removing sex (only 2 better RSS)
#removing region increases that number to 267 

#checking model assumptions
check_regression(M)
#fails linearity, there is a defined curve in the residuals plot
#fails normality, based on qq plot, the line does not stay in bounds
#should still consider changing bmi to find best predictor or consider adding polynomial term
#however, there are still things I want to explore before proceeding with this model

#before fixing assumptions and considering a polynomial term, I want to explore interactions
#between the variables
M<-lm(log(charges)~.^2,data=insurance)
summary(M)
drop1(M,test='F')
#The significant interactions and their p-values are:
#children:smoker 1.184e-08
#bmi:region 0.0134113
#bmi:smoker 2.2e-16
#age:region 0.0001059
#age:smoker 2.2e-16
#age:children 3.154e-10
#age:sex 0.0011668
#including all interactions made region insignificant but it has to stay because it is part of
#an interaction.

#model with significant interactions
M<-lm(log(charges)~age+sex+bmi+children+smoker+region+children*smoker+bmi*region+bmi*smoker
      +age*region+age*smoker+age*children+age*sex,data=insurance)
summary(M)
#every variable with the exception of some dummy variables for region and their interactions
#are significant.
#Residual standard error: 0.3728, the typical error committed by the model is 0.3728,
#this is an improvement on the previous model's RMSE of 0.4443
#Adjusted R-squared:  0.8357 , the model explains 83.6% of the variation in charges
#F-statistic: 358.8 on 19 and 1318 DF,  p-value: < 2.2e-16
#the p-value of the model is significant
drop1(M,test='F')
#                 Df Sum of Sq    RSS     AIC  F value    Pr(>F)    
#  <none>                       183.14 -2620.8                       
#  children:smoker  1     4.137 187.28 -2592.9  29.7724 5.797e-08 ***
#  bmi:region       3     1.625 184.77 -2615.0   3.8987 0.0086879 ** 
#  bmi:smoker       1    23.017 206.16 -2464.4 165.6435 < 2.2e-16 ***
#  age:region       3     2.455 185.60 -2609.0   5.8897 0.0005424 ***
#  age:smoker       1    45.649 228.79 -2325.1 328.5110 < 2.2e-16 ***
#  age:children     1     5.548 188.69 -2582.9  39.9246 3.603e-10 ***
#  age:sex          1     1.543 184.69 -2611.6  11.1063 0.0008842 ***

#lets check assumptions
check_regression(M)
#nothing really changed. still failing the normal and linearity assumptions the same way
#I will now try adding a polynomial term for bmi like I considered earlier.

M<-lm(log(charges)~age+sex+bmi+I(bmi^2)+children+smoker+region+children*smoker+bmi*region+bmi*smoker
      +age*region+age*smoker+age*children+age*sex,data=insurance)
summary(M)
#Residual standard error: 0.372 (slightly better than before), the typical error committed by the model is 0.372
#Adjusted R-squared:  0.8363 (slightly better), the model explains 83.6% of variance in charges 
#variables that were significant before stayed significant, while the variables that weren't are still not
#but they cannot be removed due to interactions being significant.
#bmi^2 was significant, p-value 0.001937
drop1(M,test='F')
#the interesting change is that bmi:region is now insignificant. So I will drop it and refit the model.

M<-lm(log(charges)~age+sex+bmi+I(bmi^2)+children+smoker+region+children*smoker+bmi*smoker
      +age*region+age*smoker+age*children+age*sex,data=insurance)
summary(M)
#Residual standard error: 0.3722, the typical error committed by the model is 0.3722
#Adjusted R-squared:  0.8361, the model explains 83.6% of variance in charges
#F-statistic: 402.3 on 17 and 1320 DF,  p-value: < 2.2e-16, the model is significant
drop1(M,test='F')
#all variables are significant to the model (p-val<0.05)

#check assumptions
check_regression(M,extra=T)
#it appears that BMI^2 did not alter the plots as much as I thought it would
#we are still failing linearity and normality by a large amount
#even though all of our variables are significant, it might be time to consider
#taking steps backward until the model can meet model assumptions.

#~~~~~~~~~~~~~~~~~~#
# Model Attempt 2: #
#~~~~~~~~~~~~~~~~~~#

#going back to my initial model in attempt 1:
#running all variables with no interactions in the model told us that sex
#and region were insignificant. So, we will start by testing a model that excludes them.
#the variables region (p-val=0.096221) and sex (p-val=0.693348) aren't significant in the model
M.old<-lm(charges~age+sex+bmi+children+smoker+region,data=insurance)
M.new<-lm(charges~age+bmi+children+smoker, data=insurance)
anova(M.new,M.old)
#p-value of 0.1654, can drop both
summary(M.new)
#Residual standard error: 6068 (slightly worse than 6062 of original model)
#the typical error committed by the model is 6062
#Adjusted R-squared:  0.7489 (slightly worse than 0.7494 of the original model)
#the model explains 74.9% of variance in charges
#F-statistic: 998.1 on 4 and 1333 DF,  p-value: < 2.2e-16, the model is significant
M<-lm(charges~age+bmi+children+smoker, data=insurance)

#check assumptions
check_regression(M,extra=T)
#this is the most normal our qq-plot has looked. Still appears to fail the test but its much better
#normality is by far the worst test for the model (still failing too)
#bmi appears to be the biggest offender
#output tells us:
#Tests of Assumptions: ( sample size n = 1338 ):
#  Linearity
#p-value for age : 0.2608 
#p-value for bmi : 0.4222 
#p-value for children : 0.0369
#p-value for smokeryes : NA (categorical or only 1 or 2 unique values) 
#p-value for overall model : 0.5821
#Equal Spread:  p-value is 0 
#Normality:  p-value is 0 
#you can see that both age and bmi are the reasons for the violation in linearity
#and how the model passes equal spread and normality. 
#however the normality test is very bad but it is still better than the plot from
#earlier of the model with the polynomial bmi and all of the interactions.
#based on the note in the assignment, I'm assuming that the normality test is what you meant
#by not being able to meet all of the regression assumptions.
#Note from word doc:
#[Note: With this data set, it will be difficult to meet all regression assumptions, but you can at least try to minimize violations.]

M<-lm(log(charges)~age+bmi+children+smoker, data=insurance)
check_regression(M,extra=T)
#log transformation of charges does not make the residual plot any better (it might have made it worse)
#because of this I am going to leave the model as it was and proceed to check to see if interactions 
#need to be included

M<-lm(charges~(age+bmi+children+smoker)^2, data=insurance)
summary(M)
#the only significant interaction is bmi:smokeryes  pvalue = 2e-16 < 0.05
#so i'll add that interaction to the model

M<-lm(charges~age+bmi+children+smoker+smoker*bmi, data=insurance)
summary(M)
#Residual standard error: 4871, typical error committed by the model is 4871
#Adjusted R-squared:  0.8382, model explains 83.8% of variance in charges
#this number is better than the highly complex model from earlier (83.6%) but it 
#has half as many variables.
#F-statistic:  1387 on 5 and 1332 DF,  p-value: < 2.2e-16, model is significant
#The variables are all significant (p-value<) except for bmi (p-val=0.82014) but
#it has to stay in the model because of the interaction bmi:smokeryes is significant.

check_regression(M,extra=T)
#this model is a little worse for equal spread, but a little better for linearity (still fails)
#the p-value for normality is 0, but there seems to be an issue there. 
#Ultimately, it is still better at passing these assumptions than the complex model from
#earlier.
#BMI is still a big problem for the residual plots. I will try to add a polynomial term

M<-lm(charges~age+bmi+I(bmi^2)+children+smoker+smoker*bmi, data=insurance)
summary(M)
#Residual standard error: 4853, typical error committed by the model is 4853
#Adjusted R-squared:  0.8394, model explains 83.9% of variance in charges 
#F-statistic:  1166 on 6 and 1331 DF,  p-value: < 2.2e-16, model is significant
#model is slightly better than previous model (lower RMSE and higher R2)
#All variables are significant, even BMI which was not significant before.

check_regression(M,extra=T)
#no real difference in assumption tests. I would say that linearity is improved slightly
#while equal spread remains about the same. Normality's p-value (Normality:  p-value is 0)
#says it is passed, but there appears to be many points outside of the bounds.

#Overall, I'm not sure how to improve this model much past this point without 
#making the model too complex. This model out performs the first attempt because it
#achieves similar results (better R2 than before) without being overly complicated.

#with BMI posing so many issues for the residual plots and regression assumptions,
#I will consider a model without BMI

#~~~~~~~~~~~~~~~~~~#
# Model Attempt 3: #
#~~~~~~~~~~~~~~~~~~#

#considering a model without bmi

M<-lm(charges~age+sex+children+smoker+region,data=insurance)
summary(M)
#Residual standard error: 6372, typical error committed by the model is 6372
#Adjusted R-squared:  0.7231, model explains 72.3% of variance in charges 
#F-statistic: 499.8 on 7 and 1330 DF,  p-value: < 2.2e-16, model is significant
drop1(M,test='F')
#region (0.8776929) and sex (0.2772386) are insignificant to the model (p-values >0.5)

check_regression(M,extra=T)
# Tests of Assumptions: ( sample size n = 1338 ):
#   Linearity
# p-value for age : 0.2608 
# p-value for sexmale : NA (categorical or only 1 or 2 unique values) 
# p-value for children : 0.0369 
# p-value for smokeryes : NA (categorical or only 1 or 2 unique values) 
# p-value for regionnorthwest : NA (categorical or only 1 or 2 unique values) 
# p-value for regionsoutheast : NA (categorical or only 1 or 2 unique values) 
# p-value for regionsouthwest : NA (categorical or only 1 or 2 unique values) 
# p-value for overall model : 0 
# Equal Spread:  p-value is 0 
# Normality:  p-value is 0 
#according to the output, the model passes the tests for regression assumptions
#however, the plots show very different results. It is apparent that a large cluster 
#of data points are very similar and are causing the data to appear this way.
#overall, this is the first time a model has passed all of the tests from the output of
#check_regression(), so I would say we are moving in the right direction

#checking to see if I can drop both sex and region
m.new<-lm(charges~age+children+smoker,data=insurance)
anova(m.new,M)
#p-val (for F=0.9732) is 0.4211, we can drop both because they do not significantly
#reduce RSS.

M<-lm(charges~age+children+smoker,data=insurance)
summary(M)
#Residual standard error: 6372, typical error committed by the model is 6372
#Adjusted R-squared:  0.7231, model explains 72.3% of variance in charges 
#F-statistic:  1165 on 3 and 1334 DF,  p-value: < 2.2e-16, model is significant
#this model has two less variables than the previous model and performs the same'

check_regression(M,extra=T)
#Output still says it passes all assumptions (linearity, equal spread, and normality have 
#p-values = 0) but the plots are kind of all over the place. 

#now, I will consider interactions between the remaining variables.
M<-lm(charges~age*children+age*smoker+children*smoker,data=insurance)
summary(M)
#no interaction was significant 
#Residual standard error: 6371, typical error committed by the model is 6371
#Adjusted R-squared:  0.7232, model explains 72.3% of variance in charges  
#F-statistic: 583.3 on 6 and 1331 DF,  p-value: < 2.2e-16, model is significant

#from here it is best to just keep age, children, and smoker as the predictors for this
#model as the interactions were not significant. I don't think this model can reach the 
#level that model 2 achieved over the model exploration process.



#Moving on, the next section will list the best model from each model building attempt.
#I will then list out the key characteristics of each model before deciding on a single model
#in the section after. This model will be described and interpreted in a separate word file.

##------------------------------------##
## Section 3: best three models:      ##
##------------------------------------##
#~~~~~~~~~~~~~~~~~~#
# Model Attempt 1: #
#~~~~~~~~~~~~~~~~~~#
M<-lm(log(charges)~age+sex+bmi+I(bmi^2)+children+smoker+region+children*smoker+bmi*smoker
      +age*region+age*smoker+age*children+age*sex,data=insurance)
summary(M)
#Residual standard error: 0.3722, the typical error committed by the model is 0.3722
#Adjusted R-squared:  0.8361, the model explains 83.6% of variance in charges
#F-statistic: 402.3 on 17 and 1320 DF,  p-value: < 2.2e-16, the model is significant

#model does not pass test of equal spread or linearity

#~~~~~~~~~~~~~~~~~~#
# Model Attempt 2: #
#~~~~~~~~~~~~~~~~~~#
M<-lm(charges~age+bmi+I(bmi^2)+children+smoker+smoker*bmi, data=insurance)
summary(M)
#Residual standard error: 4853, typical error committed by the model is 4853
#Adjusted R-squared:  0.8394, model explains 83.9% of variance in charges 
#F-statistic:  1166 on 6 and 1331 DF,  p-value: < 2.2e-16, model is significant

#model does not pass test of equal spread or linearity.
#however, it is better than model attempt one in both tests.

#~~~~~~~~~~~~~~~~~~#
# Model Attempt 3: #
#~~~~~~~~~~~~~~~~~~#
M<-lm(charges~age+children+smoker,data=insurance)
summary(M)
#Residual standard error: 6372, typical error committed by the model is 6372
#Adjusted R-squared:  0.7231, model explains 72.3% of variance in charges 
#F-statistic:  1165 on 3 and 1334 DF,  p-value: < 2.2e-16, model is significant

#model passes all tests based on the output, but the plots appear to tell a 
#different story


##------------------------------------##
## Section 4: best model decision:    ##
##------------------------------------##
M<-lm(charges~age+bmi+I(bmi^2)+children+smoker+smoker*bmi, data=insurance)
summary(M)
#for the best model, I will choose model attempt two. It had the highest R2 of any
#model. This means it did the best of the models at explaining the variance in charges.
#Though it did not pass all of the tests. I thought the residual plot had the 
#least egregious violations of the model (even though the statistical output of
#check_regression would disagree.) The model appears to be significant and having a
#typical error of ~4800 is not too bad considering the range of charges goes from
#1122 and 63770. It is also much simpler than the complex model that had a worse R2.