library(mgcv)
library(ggplot2)
library(eoffice)
data1<-read.csv("GAM5.csv", header = T)
#case1 from Brumovsk et al., Contaminants of emerging concern in the open sea waters of the Western Mediterranean. Environmental Pollution 229 (2017) 976-983
GAM1 <- gam(log(NASA1)~s(number1,k=4),data=data1,method="REML",select=TRUE)
#case2 from Brumovsky et al., Line ferries and cargo ships for the monitoring of marine contaminants of emerging concern: Application along a Europe-Arctic transect. Journal of Hazardous Materials 424 (2022) 127232
GAM2 <- gam(log(OCCCI2)~s(number2,k=4),data=data1,method="REML",select=TRUE)
#case 3 from Brumovsky et al., Exploring the occurrence and distribution of contaminants of emerging concern through unmanned sampling from ships of opportunity in the North Sea. Journal of Marine Systems 162 (2016) 47每56
GAM3 <- gam(NPP3~s(number3,k=7),data=data1,method="REML",select=TRUE)
#case 4 from merga et al., Biological and chemical monitoring of the ecological risks of pesticides in Lake Ziway, Ethiopia. Chemosphere 266 (2021) 129214
GAM4 <- gam(Shannon_W4~s(number4,k=7),data=data1,method="REML",select=TRUE)
GAM4_1 <- gam(Menhinick4~s(number4,k=7),data=data1,method="REML",select=TRUE)
GAM4_2 <- gam(Shannon_W4~s(pH4,k=7),data=data1,method="REML",select=TRUE)
GAM4_3 <- gam(Menhinick4~s(pH4,k=7),data=data1,method="REML",select=TRUE)
GAM4_4 <- gam(Shannon_W4~s(temperature4,k=7),data=data1,method="REML",select=TRUE)
GAM4_5 <- gam(Menhinick4~s(temperature4,k=7),data=data1,method="REML",select=TRUE)
GAM4_6 <- gam(Shannon_W4~s(DO4,k=7),data=data1,method="REML",select=TRUE)
GAM4_7 <- gam(Menhinick4~s(DO4,k=7),data=data1,method="REML",select=TRUE)
#case5 from Bayen et al., Pharmaceutically active compounds and endocrine disrupting chemicals in water, sediments and mollusks in mangrove ecosystemsfrom Singapore. Marine Pollution Bulletin 109 (2016) 716每722
GAM5 <- gam(inverbiomass5~s(number5,k=6),data=data1,method="REML",select=TRUE)
GAM5_1 <- gam(inverbiomass5~s(pH5,k=7),data=data1,method="REML",select=TRUE)
GAM5_2 <- gam(inverbiomass5~s(conductivity5,k=7),data=data1,method="REML",select=TRUE)
GAM5_3 <- gam(inverbiomass5~s(temperature5,k=7),data=data1,method="REML",select=TRUE)
GAM5_4 <- gam(inverbiomass5~s(DO5,k=7),data=data1,method="REML",select=TRUE)
GAM5_5 <- gam(inverbiomass5~s(salinity5,k=7),data=data1,method="REML",select=TRUE)
#case6 from The 2013每2017 U.S. Geological Survey (USGS) Regional Stream Quality Assessments (RSQA); Science of the Total Environment 773 (2021) 145062 and Science of the Total Environment 800 (2021) 149350
GAM6 <- gam(BENTMMI6~s(number6,k=6),data=data1,method="REML",select=TRUE)
GAM6_1 <- gam(FSPTAX6~s(number6,k=7),data=data1,method="REML",select=TRUE)
GAM6_2 <- gam(FMMI6~s(number6,k=7),data=data1,method="REML",select=TRUE)
GAM6_3 <- gam(BC456~s(number6,k=7),data=data1,method="REML",select=TRUE)
GAM6_4 <- gam(BENTMMI6~s(pH6,k=6),data=data1,method="REML",select=TRUE)
GAM6_5 <- gam(FSPTAX6~s(pH6,k=7),data=data1,method="REML",select=TRUE)
GAM6_6 <- gam(FMMI6~s(pH6,k=7),data=data1,method="REML",select=TRUE)
GAM6_7 <- gam(BC456~s(pH6,k=7),data=data1,method="REML",select=TRUE)
GAM6_8 <- gam(BENTMMI6~s(DO6,k=6),data=data1,method="REML",select=TRUE)
GAM6_9 <- gam(FSPTAX6~s(DO6,k=7),data=data1,method="REML",select=TRUE)
GAM6_10 <- gam(FMMI6~s(DO6,k=7),data=data1,method="REML",select=TRUE)
GAM6_11 <- gam(BC456~s(DO6,k=7),data=data1,method="REML",select=TRUE)
GAM6_12 <- gam(BENTMMI6~s(TN6,k=6),data=data1,method="REML",select=TRUE)
GAM6_13 <- gam(FSPTAX6~s(TN6,k=7),data=data1,method="REML",select=TRUE)
GAM6_14 <- gam(FMMI6~s(TN6,k=7),data=data1,method="REML",select=TRUE)
GAM6_15 <- gam(BC456~s(TN6,k=7),data=data1,method="REML",select=TRUE)
GAM6_16 <- gam(BENTMMI6~s(TP6,k=6),data=data1,method="REML",select=TRUE)
GAM6_17 <- gam(FSPTAX6~s(TP6,k=7),data=data1,method="REML",select=TRUE)
GAM6_18 <- gam(FMMI6~s(TP6,k=7),data=data1,method="REML",select=TRUE)
GAM6_19 <- gam(BC456~s(TP6,k=7),data=data1,method="REML",select=TRUE)
GAM6_20 <- gam(BENTMMI6~s(temperature6,k=6),data=data1,method="REML",select=TRUE)
GAM6_21 <- gam(FSPTAX6~s(temperature6,k=7),data=data1,method="REML",select=TRUE)
GAM6_22 <- gam(FMMI6~s(temperature6,k=7),data=data1,method="REML",select=TRUE)
GAM6_23 <- gam(BC456~s(temperature6,k=7),data=data1,method="REML",select=TRUE)
GAM6_24 <- gam(BENTMMI6~s(conductivity6,k=6),data=data1,method="REML",select=TRUE)
GAM6_25 <- gam(FSPTAX6~s(conductivity6,k=7),data=data1,method="REML",select=TRUE)
GAM6_26 <- gam(FMMI6~s(conductivity6,k=7),data=data1,method="REML",select=TRUE)
GAM6_27 <- gam(BC456~s(conductivity6,k=7),data=data1,method="REML",select=TRUE)
#case7 from Scarce-consolider project
GAM7 <- gam(log(inverabunt7)~s(number7,k=7),data=data1,method="REML",select=TRUE)
GAM7_1 <- gam(log(inverabunt7)~s(pH7,k=7),data=data1,method="REML",select=TRUE)
GAM7_2 <- gam(log(inverabunt7)~s(temperature7,k=7),data=data1,method="REML",select=TRUE)
GAM7_3 <- gam(log(inverabunt7)~s(DO7,k=7),data=data1,method="REML",select=TRUE)
GAM7_4 <- gam(log(inverabunt7)~s(conductivity7,k=7),data=data1,method="REML",select=TRUE)
GAM7_5 <- gam(log(inverabunt7)~s(TP7,k=7),data=data1,method="REML",select=TRUE)
GAM7_6 <- gam(log(inverabunt7)~s(DOC7,k=7),data=data1,method="REML",select=TRUE)
#case8 from Wang et al., Organic Micropollutants in New York Lakes: A Statewide Citizen Science Occurrence Study. Environ. Sci. Technol. 2020, 54, 13759???13770
GAM8 <- gam(log(nor_chloro8)~s(number8, bs="cr"),data=data1,method="REML",select=TRUE)
GAM8_1 <- gam(log(chlorophyll8)~s(pH8, bs="cr"),data=data1,method="REML",select=TRUE)
GAM8_2 <- gam(log(chlorophyll8)~s(conductivity8, bs="cr"),data=data1,method="REML",select=TRUE)
GAM8_3 <- gam(log(chlorophyll8)~s(Nox_N8, bs="cr"),data=data1,method="REML",select=TRUE)
GAM8_4 <- gam(log(chlorophyll8)~s(NH3_N8, bs="cr"),data=data1,method="REML",select=TRUE)
GAM8_5 <- gam(log(chlorophyll8)~s(TDN8, bs="cr"),data=data1,method="REML",select=TRUE)
GAM8_6 <- gam(log(chlorophyll8)~s(TN8, bs="cr"),data=data1,method="REML",select=TRUE)
GAM8_7 <- gam(log(chlorophyll8)~s(agricularea8, bs="cr"),data=data1,method="REML",select=TRUE)
GAM8_8 <- gam(log(chlorophyll8)~s(urbanarea8, bs="cr"),data=data1,method="REML",select=TRUE)
GAM8_9 <- gam(log(chlorophyll8)~s(forestarea8, bs="cr"),data=data1,method="REML",select=TRUE)
GAM8_10 <- gam(log(chlorophyll8)~s(lakearea8, bs="cr"),data=data1,method="REML",select=TRUE)
#case9 from Tornes et al., Diatom responses to sewage inputs and hydrological alteration in Mediterranean streams. Environmental Pollution 238 (2018) 369-378
GAM9 <- gam(cyanobacteria_abundant9~s(number9, bs="cr"),data=data1,method="REML",select=TRUE)
GAM9_1 <- gam(algae_richness9~s(number9, bs="cr"),data=data1,method="REML",select=TRUE)
GAM9_2 <- gam(cyanobacteria_abundant9~s(N_NH49, bs="cr"),data=data1,method="REML",select=TRUE)
GAM9_3 <- gam(algae_richness9~s(N_NH49, bs="cr"),data=data1,method="REML",select=TRUE)
GAM9_4 <- gam(cyanobacteria_abundant9~s(N_NO39, bs="cr"),data=data1,method="REML",select=TRUE)
GAM9_5 <- gam(algae_richness9~s(N_NO39, bs="cr"),data=data1,method="REML",select=TRUE)
GAM9_6 <- gam(cyanobacteria_abundant9~s(conductivity9, bs="cr"),data=data1,method="REML",select=TRUE)
GAM9_7 <- gam(algae_richness9~s(conductivity9, bs="cr"),data=data1,method="REML",select=TRUE)
GAM9_8 <- gam(cyanobacteria_abundant9~s(DO9, bs="cr"),data=data1,method="REML",select=TRUE)
GAM9_9 <- gam(algae_richness9~s(DO9, bs="cr"),data=data1,method="REML",select=TRUE)
GAM9_10 <- gam(cyanobacteria_abundant9~s(temperature9, bs="cr"),data=data1,method="REML",select=TRUE)
GAM9_11 <- gam(algae_richness9~s(temperature9, bs="cr"),data=data1,method="REML",select=TRUE)
GAM9_12 <- gam(cyanobacteria_abundant9~s(pH9, bs="cr"),data=data1,method="REML",select=TRUE)
GAM9_13 <- gam(algae_richness9~s(pH9, bs="cr"),data=data1,method="REML",select=TRUE)
GAM9_14 <- gam(cyanobacteria_abundant9~s(stream_width9, bs="cr"),data=data1,method="REML",select=TRUE)
GAM9_15 <- gam(algae_richness9~s(stream_width9, bs="cr"),data=data1,method="REML",select=TRUE)
#case10 from LIorens et al., Occurrence of regulated pollutants in populated Mediterranean basins Ecotoxicological risk and effects on biological quality. Science of the Total Environment 747 (2020) 141224
GAM10 <- gam(invertebrate_indices10~s(number10, k=5),data=data1,method="REML",select=TRUE)
GAM10_1 <- gam(diatoms_indices10~s(number10, k=5),data=data1,method="REML",select=TRUE)
#case11 from Peng et al., Benthic invertebrate and microbial biodiversity in sub-tropical urban rivers: Correlations with environmental variables and emerging chemicals. Science of the Total Environment 709 (2020) 136281
GAM11 <- gam(log(inverrichness_condu)~s(number11, k=5),data=data1,method="REML",select=TRUE)
GAM11_1 <- gam(log(inver_richess)~s(pH11, k=5),data=data1,method="REML",select=TRUE)
GAM11_2 <- gam(log(inver_richess)~s(DO11, k=5),data=data1,method="REML",select=TRUE)
GAM11_3 <- gam(log(inver_richess)~s(conductivity11, k=5),data=data1,method="REML",select=TRUE)
GAM11_4 <- gam(log(inver_richess)~s(TP11, k=5),data=data1,method="REML",select=TRUE)
GAM11_5 <- gam(log(inver_richess)~s(TN11, k=5),data=data1,method="REML",select=TRUE)
#case12 from Fisch et al., Seasonal variability, long-term distribution (2001每2014), and risk assessment of polar organic micropollutants in the Baltic Sea. Environmental Science and Pollution Research (2021) 28:39296每39309
GAM12 <- gam(log(NPP12)~s(number12, bs="cr"),data=data1,method="REML",select=TRUE)
#case13 from Peng et al., Double constrained ordination for assessing biological trait responses to multiple stressors: A case study with benthic macroinvertebrate communities. Science of the Total Environment 754 (2021) 142171
GAM13 <- gam(chloro_TN_NO3_TP13~s(number13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_1 <- gam(inver_abund_TN13~s(number13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_2 <- gam(inver_richness_TN13~s(number13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_3 <- gam(chlorophyll13~s(pH13, k=5),data=data1,method="REML",select=TRUE)
GAM13_4 <- gam(invertebrate_abunt13~s(pH13, k=5),data=data1,method="REML",select=TRUE)
GAM13_5 <- gam(invertebrate_richness13~s(pH13, k=5),data=data1,method="REML",select=TRUE)
GAM13_6 <- gam(chlorophyll13~s(temperature13, k=5),data=data1,method="REML",select=TRUE)
GAM13_7 <- gam(invertebrate_abunt13~s(temperature13, k=5),data=data1,method="REML",select=TRUE)
GAM13_8 <- gam(invertebrate_richness13~s(temperature13, k=5),data=data1,method="REML",select=TRUE)
GAM13_9 <- gam(chlorophyll13~s(DO13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_10 <- gam(invertebrate_abunt13~s(DO13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_11 <- gam(invertebrate_richness13~s(DO13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_12 <- gam(chlorophyll13~s(conductivity13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_13 <- gam(invertebrate_abunt13~s(conductivity13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_14 <- gam(invertebrate_richness13~s(conductivity13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_15 <- gam(chlorophyll13~s(TP13, k=5),data=data1,method="REML",select=TRUE)
GAM13_16 <- gam(invertebrate_abunt13~s(TP13, k=5),data=data1,method="REML",select=TRUE)
GAM13_17 <- gam(invertebrate_richness13~s(TP13, k=5),data=data1,method="REML",select=TRUE)
GAM13_18 <- gam(chlorophyll13~s(DOC13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_19 <- gam(invertebrate_abunt13~s(DOC13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_20 <- gam(invertebrate_richness13~s(DOC13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_21 <- gam(chlorophyll13~s(TN13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_22 <- gam(invertebrate_abunt13~s(TN13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_23 <- gam(invertebrate_richness13~s(TN13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_24 <- gam(chlorophyll13~s(NO3_N13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_25 <- gam(invertebrate_abunt13~s(NO3_N13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_26 <- gam(invertebrate_richness13~s(NO3_N13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_27 <- gam(chlorophyll13~s(riverarea13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_28 <- gam(invertebrate_abunt13~s(riverarea13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_29 <- gam(invertebrate_richness13~s(riverarea13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_30 <- gam(chlorophyll13~s(riverdepth13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_31 <- gam(invertebrate_abunt13~s(riverdepth13, bs="cr"),data=data1,method="REML",select=TRUE)
GAM13_32 <- gam(invertebrate_richness13~s(riverdepth13, bs="cr"),data=data1,method="REML",select=TRUE)

GAM1$converged
GAM2$converged
GAM3$converged
GAM4$converged
GAM4_1$converged
GAM4_2$converged
GAM4_3$converged
GAM4_4$converged
GAM4_5$converged
GAM4_6$converged
GAM4_7$converged
GAM5$converged
GAM5_1$converged
GAM5_2$converged
GAM5_3$converged
GAM5_4$converged
GAM5_5$converged
GAM6$converged
GAM6_1$converged
GAM6_2$converged
GAM6_3$converged
GAM6_4$converged
GAM6_5$converged
GAM6_6$converged
GAM6_7$converged
GAM6_8$converged
GAM6_9$converged
GAM6_10$converged
GAM6_11$converged
GAM6_12$converged
GAM6_13$converged
GAM6_14$converged
GAM6_15$converged
GAM6_16$converged
GAM6_17$converged
GAM6_18$converged
GAM6_19$converged
GAM6_20$converged
GAM6_21$converged
GAM6_22$converged
GAM6_23$converged
GAM6_24$converged
GAM6_25$converged
GAM6_26$converged
GAM6_27$converged
GAM7$converged
GAM7_1$converged
GAM7_2$converged
GAM7_3$converged
GAM7_4$converged
GAM7_5$converged
GAM7_6$converged
GAM8$converged
GAM8_1$converged
GAM8_2$converged
GAM8_3$converged
GAM8_4$converged
GAM8_5$converged
GAM8_6$converged
GAM8_7$converged
GAM8_8$converged
GAM8_9$converged
GAM8_10$converged
GAM9$converged
GAM9_1$converged
GAM9_2$converged
GAM9_3$converged
GAM9_4$converged
GAM9_5$converged
GAM9_6$converged
GAM9_7$converged
GAM9_8$converged
GAM9_9$converged
GAM9_10$converged
GAM9_11$converged
GAM9_12$converged
GAM9_13$converged
GAM9_14$converged
GAM9_15$converged
GAM10$converged
GAM10_1$converged
GAM11$converged
GAM11_1$converged
GAM11_2$converged
GAM11_3$converged
GAM11_4$converged
GAM11_5$converged
GAM12$converged
GAM13$converged
GAM13_1$converged
GAM13_2$converged
GAM13_3$converged
GAM13_4$converged
GAM13_5$converged
GAM13_6$converged
GAM13_7$converged
GAM13_8$converged
GAM13_9$converged
GAM13_10$converged
GAM13_11$converged
GAM13_12$converged
GAM13_13$converged
GAM13_14$converged
GAM13_15$converged
GAM13_16$converged
GAM13_17$converged
GAM13_18$converged
GAM13_19$converged
GAM13_20$converged
GAM13_21$converged
GAM13_22$converged
GAM13_23$converged
GAM13_24$converged
GAM13_25$converged
GAM13_26$converged
GAM13_27$converged
GAM13_28$converged
GAM13_29$converged
GAM13_30$converged
GAM13_31$converged
GAM13_32$converged

summary(GAM1)
summary(GAM2)
summary(GAM3)
summary(GAM4)
summary(GAM4_1)
summary(GAM4_2)
summary(GAM4_3)
summary(GAM4_4)
summary(GAM4_5)
summary(GAM4_6)
summary(GAM4_7)
summary(GAM5)
summary(GAM5_1)
summary(GAM5_2)
summary(GAM5_3)
summary(GAM5_4)
summary(GAM5_5)
summary(GAM6)
summary(GAM6_1)
summary(GAM6_2)
summary(GAM6_3)
summary(GAM6_4)
summary(GAM6_5)
summary(GAM6_6)
summary(GAM6_7)
summary(GAM6_8)
summary(GAM6_9)
summary(GAM6_10)
summary(GAM6_11)
summary(GAM6_12)
summary(GAM6_13)
summary(GAM6_14)
summary(GAM6_15)
summary(GAM6_16)
summary(GAM6_17)
summary(GAM6_18)
summary(GAM6_19)
summary(GAM6_20)
summary(GAM6_21)
summary(GAM6_22)
summary(GAM6_23)
summary(GAM6_24)
summary(GAM6_25)
summary(GAM6_26)
summary(GAM6_27)
summary(GAM7)
summary(GAM7_1)
summary(GAM7_2)
summary(GAM7_3)
summary(GAM7_4)
summary(GAM7_5)
summary(GAM7_6)
summary(GAM8)
summary(GAM8_1)
summary(GAM8_2)
summary(GAM8_3)
summary(GAM8_4)
summary(GAM8_5)
summary(GAM8_6)
summary(GAM8_7)
summary(GAM8_8)
summary(GAM8_9)
summary(GAM8_10)
summary(GAM9)
summary(GAM9_1)
summary(GAM9_2)
summary(GAM9_3)
summary(GAM9_4)
summary(GAM9_5)
summary(GAM9_6)
summary(GAM9_7)
summary(GAM9_8)
summary(GAM9_9)
summary(GAM9_10)
summary(GAM9_11)
summary(GAM9_12)
summary(GAM9_13)
summary(GAM9_14)
summary(GAM9_15)
summary(GAM10)
summary(GAM10_1)
summary(GAM11)
summary(GAM11_1)
summary(GAM11_2)
summary(GAM11_3)
summary(GAM11_4)
summary(GAM11_5)
summary(GAM12)
summary(GAM13)
summary(GAM13_1)
summary(GAM13_2)
summary(GAM13_3)
summary(GAM13_4)
summary(GAM13_5)
summary(GAM13_6)
summary(GAM13_7)
summary(GAM13_8)
summary(GAM13_9)
summary(GAM13_10)
summary(GAM13_11)
summary(GAM13_12)
summary(GAM13_13)
summary(GAM13_14)
summary(GAM13_15)
summary(GAM13_16)
summary(GAM13_17)
summary(GAM13_18)
summary(GAM13_19)
summary(GAM13_20)
summary(GAM13_21)
summary(GAM13_22)
summary(GAM13_23)
summary(GAM13_24)
summary(GAM13_25)
summary(GAM13_26)
summary(GAM13_27)
summary(GAM13_28)
summary(GAM13_29)
summary(GAM13_30)
summary(GAM13_31)
summary(GAM13_32)

#color,#00abf0,#d75427,#3cb346,#e20612,#ffd401,#00b0eb
par(mfrow = c(3,3))
plot(GAM1, pch = 19, col="#3cb346",cex=1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM2, pch = 19, col="red",cex=1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM3, pch = 19, col="blue",cex=1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM4, pch = 17, col="#2DB600FF",cex=1.1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM4_1, pch = 17, col="#2DB600FF",cex=1.1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM5, pch = 18,col="#408DC3",cex=1.1,lwd=2, shade = TRUE, residuals = TRUE,tck=-0.02)
topptx(filename ="18EPGAM1.pptx")
par(mfrow = c(3,3))
plot(GAM6, pch = 17, col="#e8490f",cex=1.1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM6_1, pch = 17, col="#e8490f",cex=1.1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM6_2, pch = 17, col="#e8490f",cex=1.1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM6_3, pch = 17, col="#e8490f",cex=1.1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM7, pch = 19, col="#443B84FF",cex=1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM8, pch = 17, col="#75D054FF",cex=1.1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
topptx(filename ="18EPGAM2.pptx")
par(mfrow = c(3,3))
plot(GAM9, pch = 15, col="#75D054FF",cex=1.1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM9_1, pch = 15, col="#75D054FF",cex=1.1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM10, pch = 17, col="#00abf0",cex=1.1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM10_1, pch = 17, col="#00abf0",cex=1.1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM11, pch = 18, col="#FF80FFFF",cex=1.1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM12, pch = 3, col="#75D054FF",cex=1.1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM13, pch = 19, col="#942d8d",cex=1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM13_1, pch = 19, col="#942d8d",cex=1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
plot(GAM13_2, pch = 19, col="#942d8d",cex=1,lwd=2,shade = TRUE, residuals = TRUE,tck=-0.02)
topptx(filename ="18EPGAM3.pptx")


