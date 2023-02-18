install.packages('boot')
library('boot')
melanoma
head(melanoma)

melanoma.uncensored <- melanoma[melanoma$status==1,c(1,3:7)]
head(melanoma.uncensored)

str(melanoma.uncensored)
class(melanoma.uncensored$sex)<-"character"
class(melanoma.uncensored$ulcer)<-"character"
str(melanoma.uncensored)