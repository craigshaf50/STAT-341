library(regclass)
data("SALARY")

cor_matrix(SALARY)
cor(SALARY[,1:3])
all_correlations(SALARY)
pairs(SALARY)
pairs(SALARY[,1:3])
pairs(SALARY[,1:3],lower.panel = NULL)

M<-lm(Salary ~ Education + Experience, data = SALARY)
summary(M)

check_regression(M, extra = TRUE)
# if residual plot is random, linear