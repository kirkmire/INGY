######Site Index Variables######
#SI #

soverhist<-soverhist[! soverhist$Damage %in% dead.words,]

sinst.pipo<-sinst[! sinst$Installation %in% drp60,]

#all PIPO installations have a PIPO SI except for 1, GC

annual.gr4<-merge(annual.gr4,sinst.pipo, by="Installation")



#GAM for Site Index
gam.SI<-gam(ht_annual~s(srHeight_Total)+s(SiteIndex_Value),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.SI)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.SI,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

#GAM for Slope
gam.slope<-gam(ht_annual~s(srHeight_Total)+s(Slope.x),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.slope)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.slope,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


#GAM for Elevation
gam.elev<-gam(ht_annual~s(srHeight_Total)+s(Elevation.x),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.elev)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.elev,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

#GAM for Aspect
gam.aspect<-gam(ht_annual~s(srHeight_Total)+s(annual.gr4$Aspect_Deg.x),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.aspect)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.aspect,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)




#OS Quantreg (Carrying forward CW/sTPA and shrub transect, TPA)
library(quantreg)

qr.SI<-rq(ht_annual~srHeight_Total+CrownWidth+diff.S+TPA.OS+SiteIndex_Value,tau=c(1:9/10),data=annual.gr4)
summary(qr.SI)
AIC(qr.SI)[1]

#SI qr has an AIC> 4741 (OS TPA)


plot(summary(qr.SI, se = "nid"), level = 0.95)
#illustrates how the effects of predictors 
#cary over quantiles  and how the magnitude of 
#the effects at varouis quantiles differ considerably fromthe OLS coefficients
#even in terms of the CI 



qr.SI.5<-  rq(ht_annual~srHeight_Total+CrownWidth+diff.S+TPA.OS+SiteIndex_Value,tau=c(.5),data=annual.gr4)
qr.SI.1 <- rq(ht_annual~srHeight_Total+CrownWidth+diff.S+TPA.OS+SiteIndex_Value,tau=c(.1),data=annual.gr4)
qr.SI.9 <- rq(ht_annual~srHeight_Total+CrownWidth+diff.S+TPA.OS+SiteIndex_Value,tau=c(.9),data=annual.gr4)

anova(qr.SI.1,qr.SI.5,qr.SI.9)

#strong evidence that the predictors are not the same between at least two of the models





