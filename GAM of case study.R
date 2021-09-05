library(mgcv)
data1<-read.csv("GAMcase.csv", header = T)
GAM1 <- gam(log(chlorophyll)~s(number, bs="cr"),data=data1,method="REML",select=TRUE)
GAM2 <- gam(log(chlorophyll)~s(pH, bs="cr"),data=data1,method="REML",select=TRUE)
GAM3 <- gam(log(chlorophyll)~s(SC, bs="cr"),data=data1,method="REML",select=TRUE)
GAM4 <- gam(log(chlorophyll)~s(NOXN, bs="cr"),data=data1,method="REML",select=TRUE)
GAM5 <- gam(log(chlorophyll)~s(NHN, bs="cr"),data=data1,method="REML",select=TRUE)
GAM6 <- gam(log(chlorophyll)~s(TDN, bs="cr"),data=data1,method="REML",select=TRUE)
GAM7 <- gam(log(chlorophyll)~s(TN, bs="cr"),data=data1,method="REML",select=TRUE)
GAM8 <- gam(log(chlorophyll)~s(chloride, bs="cr"),data=data1,method="REML",select=TRUE)
GAM9 <- gam(log(chlorophyll)~s(agricularea, bs="cr"),data=data1,method="REML",select=TRUE)
GAM10 <- gam(log(chlorophyll)~s(urbanarea, bs="cr"),data=data1,method="REML",select=TRUE)
GAM11 <- gam(log(chlorophyll)~s(forestarea, bs="cr"),data=data1,method="REML",select=TRUE)
GAM12 <- gam(Norchlorophyll~s(number, bs="cr"),data=data1,method="REML",select=TRUE)
#GAM12, chlorophyll is normalized by total dissolved nitrogen and total nitrogen contents#
GAM1$converged
GAM2$converged
GAM3$converged
GAM4$converged
GAM5$converged
GAM6$converged
GAM7$converged
GAM8$converged
GAM9$converged
GAM10$converged
GAM11$converged
GAM12$converged
summary(GAM1)
summary(GAM2)
summary(GAM3)
summary(GAM4)
summary(GAM5)
summary(GAM6)
summary(GAM7)
summary(GAM8)
summary(GAM9)
summary(GAM10)
summary(GAM11)
summary(GAM12)
AIC(GAM1,GAM2,GAM3,GAM4,GAM5,GAM6,GAM7,GAM8,GAM9,GAM10,GAM11,GAM12)
par(mfrow = c(3,3))
plot(GAM1, pch = 1, cex=0.7,shade = TRUE, residuals = TRUE)
plot(GAM3, pch = 1,cex=0.7, shade = TRUE, residuals = TRUE)
plot(GAM5, pch = 20, shade = TRUE, residuals = TRUE)
plot(GAM6, pch = 20, shade = TRUE, residuals = TRUE)
plot(GAM7, pch = 20, shade = TRUE, residuals = TRUE)
plot(GAM8, pch = 20, shade = TRUE, residuals = TRUE)
plot(GAM10, pch = 20, shade = TRUE, residuals = TRUE)
plot(GAM11, pch = 20, shade = TRUE, residuals = TRUE)
plot(GAM12, pch = 20, shade = TRUE, residuals = TRUE)

