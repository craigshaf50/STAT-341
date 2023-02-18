###### 9/1/22 ######
### STAT 341 Chapter 2 powerpoint
# Association - General term used to describe a relationship between two quantities.
# 1 Visualize the relationship with some combination of mosaic plots, box plots, histograms, and/or scatterplots.
# 2 Numerically measure the strength of the association (computing statistics). 
# 3 Determine whether the association is statistically significant, i.e., unlikely to have arisen by chance (hypothesis testing).
# X = explanatory variable (independent or predictor variable)
# Y = response variable (dependent variable)
# The marginal distribution of Y is the overall distribution of all Y values.
# The conditional distributions of Y (given X) are the distributions of Y values that share a common values of X.
# READ: 2.1, 2.2 for review
## 2.3 Two Categorical variables
# Slideshow Example: Can we predict dropped call frequency based on the provider?
# Mosiac Plot: Conditional, wider bar = more students, marginal stacked bar on side
# Statistically Significant- unlikely to occur by random chance
# 1 Measure the observed difference between the conditional distributions of Y across the levels of X and the overall marginal distribution of Y.
# 2 Determine what size of differences could occur by random chance when X and Y are unrelated. 
# 3 Compare the observed difference to those that could happen through random chance.P value lower than .05
# observed distance = ((O-E)^2)/E. Do for each count and add to find difference/discrepancy for a provider
# Statistic D = total difference between observed and what was expected if there were no association
# If there is no association, what should D equal
# Permutaiotn Procedure - Create artificial data set where x/y are independent, Shuffling observed values for values of x (or Y)
# Sampling Distribution - distribution of possible valeus that the stat can achieve when data is collected in a certain manor
# Our observered D is large enough to be statistically significant - PROVIDER MATTERS!!
# P value is the prob of obtaining by chance alone a discrepency at least as extreme as the one observed
# our p value is 0. There is a 0% probability we'd obserce a discrepency as large as 78.655 if there was
# no association
# Unlikely to occur by chance < 5%
library(regclass)
data(CALLS)
head(CALLS)
associate(DropCallFreq~Provider,data=CALLS)
# 2 Examples - two categorical variables
# 1) Is gender related to with what a person believes is thier main "Weapon" is for
# attracting a potential partner? X=gender y=weapon, No association
data(SURVEY11)
head(SURVEY11)
associate(X36.WeaponAttractMate~X02.Gender,data=SURVEY11)
# 2) Is there an association between relationship status and whether a person smokes?
# x = smoker y = relationship status
associate(X34.RelationshipStatus~X42.Smoker,data=SURVEY11)
# need larger num of permutations
associate(X34.RelationshipStatus~X42.Smoker,data=SURVEY11,permutations=20000)
#.05 is no longer in the p value range (.051 to .057) - not statistically significant
#using Chi Square (X^2)
associate(X34.RelationshipStatus~X42.Smoker,data=SURVEY11,classic=TRUE)
# pval=P(D>=7.68)

###### 9/6/22 ######
### 2.4 Quantitative Y, Categorical X
# EX: Women's attractiveness related to whether she is smiling? not really
# test stat F = measure of variation between group means/measure of variation within each group
# F is a ratio
# The variability between groups is F times larger than we'd expect to see by chance
# F is 1 no variability
# 2 examples
# 1. womans attractiveness related to selfie
library(regclass)
data(ATTRACTF)
associate(Score~Selfie,data=ATTRACTF)
# 2. Mens attractiveness related to Glasses (NO)
data(ATTRACTM)
associate(Score~Glasses,data=ATTRACTM,permutations = 10000)
# add classic = true to get classic p value from f distribution
# ANOVA analysis of variance
# -Compares means
# -USe When:
# *All conditional distributions are well-approximated by a normal distribution
# *Conditional distributions have approximately equal spread.
# Kruskal Wallis
# -ANOVA on ranked data
# -ranks data (smallest value is 1, 2nd smallest is 2, etc)
# Use when:
# *There are extreme outliers that would affect the mean.
# *Conditional distributions are roughly symmetric with about the same spread.
# Median Test 
# -Compares medians
# -Replace values with Smaller (S) or larger (L)
# -use when:
# *conditional distributions are skewed
# 2.5 categorical y, quantitative x
# HW: read section 2.5
# 2.6 two quantitative variables
# 1. visualize(scatterplot)
# 2. Numerically measure the strength of the association (Pearson's correlation coefficient,Spearman's correlation coefficient,Coefficient of determination)
# 3. Determine whether the association is statistically significant.(Test for correlation)
# pearson correlation r - linear relationships
# 0 no correlation, 1(-1) correlation
# want no extreme outliers. 
# otherwise use spearmans ranked correlation
# coefficient of determination = r^2 or R^2
# homework read 2.7-2.9
library(regclass)