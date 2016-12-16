
#Reads in previous scripts required
source(paste(getwd(),'/1.readdatabase.2016jun2.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.1annualizedht.2016.dec16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.2.UT.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.3.UV.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.4.OS.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.5.SQ.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)



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

