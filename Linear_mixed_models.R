# R code
library(lme4)
library(Matrix)
library(lmerTest)
library(MuMIn)
data<-read.csv("Linear_mixed_models.csv", header = T) 
str(data)
summary(data)
fit1<-lmer(growthrate~CECnumber+overlap+(1|regime)+(1|algae)+(1|light)+(1|concentration)+(1|temperature)+(1|replicate), REML=T, data)
#growthrate is the growth rates of two algae
summary(fit1)
anova(fit1)
ranova(fit1)
r.squaredLR(fit1)
# effect size of CEC number
c=data$CECnumber
b =fixef(fit1)[2]  
se=0.0001787
Effect1<-var(c)*(b^2 - se^2)/var(data$growthrate)*100
#effect size of overlap
c=data$overlap
b =fixef(fit1)[3]  
se= 8.646e-05
Effect2<-var(c)*(b^2 - se^2)/var(data$growthrate)*100
