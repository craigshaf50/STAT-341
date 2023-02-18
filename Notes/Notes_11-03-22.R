library(regclass)
data("STUDENT")

M<-lm(CollegeGPA~HSGPA+Pet,data=STUDENT)
visualize_model(M)
summary(M)
drop1(M,test='F')


STUDENT2<-STUDENT
levels(STUDENT2$Pet)[levels(STUDENT2$Pet)=='Both']<-'Mult'
STUDENT2$Pet <- factor(STUDENT2$Pet,levels(STUDENT2$Pet)[c(2,3,1,4)])
levels(STUDENT2$Pet)

M<-lm(CollegeGPA~HSGPA+Pet,data=STUDENT2)
visualize_model(M)
summary(M)
drop1(M,test='F')

#check interaction
M<-lm(CollegeGPA~HSGPA*Pet,data=STUDENT2)
visualize_model(M)
summary(M)
drop1(M,test='F')
#interaction not significant (even though some dummy variables had significant interactions)
#dont need interaction

#add gender
M<-lm(CollegeGPA~HSGPA+Pet+Gender,data=STUDENT2)
visualize_model(M)
summary(M)
drop1(M,test='F')
#pet is not significant when gender is in the model, can remove pet


