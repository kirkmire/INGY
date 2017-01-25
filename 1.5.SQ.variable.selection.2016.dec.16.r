#Import google earth generated slop_elev_aspect csv#
ge_sea<-read.csv("slope_elev_aspect.csv")

names(ge_sea)[2]<-"InstPlot"


######Site Index Variables######
#SI #

soverhist<-soverhist[! soverhist$Damage %in% dead.words,]

sinst.pipo<-sinst[! sinst$Installation %in% drp60,
                  c("Installation","SiteIndex_Value")]

#all PIPO installations have a PIPO SI except for 1, GC

annual.gr4<-merge(annual.gr4,sinst.pipo, by="Installation")

annual.gr4$InstPlot<-paste(annual.gr4$Installation,annual.gr4$Plot,sep="")

#Transform Aspect Variable#
ge_sea$sin_rad_asp<-sin((ge_sea$aspect*3.141)/180) 

ge_sea$cos_rad_asp<-cos((ge_sea$aspect*3.141)/180)


ge_sea$asp1<-(ge_sea$sin_rad_asp+ge_sea$cos_rad_asp)

annual.gr4<-merge(annual.gr4, ge_sea, by="InstPlot")





#GAM for Site Index
gam.SI<-gam(ht_annual~s(srHeight_Total)+s(SiteIndex_Value),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.SI)

par(mfrow=c(2,4),mar=c(4,4,1,2))
plot(gam.SI,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

#GAM for Slope
gam.slope<-gam(ht_annual~s(srHeight_Total)+s(slope),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.slope)


plot(gam.slope,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


#GAM for Elevation
gam.elev<-gam(ht_annual~s(srHeight_Total)+s(elevation),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.elev)

plot(gam.elev,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

#GAM for Aspect
gam.aspect<-gam(ht_annual~s(srHeight_Total)+s(annual.gr4$cos_rad_asp),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.aspect)

plot(gam.aspect,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)




#Slope Quantreg (Carrying forward CW and shrub transect, TPA)
library(quantreg)

qr.slope<-rq(ht_annual~srHeight_Total+CrownLength+diff.G.1m+TPA.OS+slope,tau=c(.5),data=annual.gr4)
summary(qr.slope)
aic.list.SQ<-AIC(qr.slope)[1]
nlist.SQ<-length(qr.slope$y)


#SI Quantreg (Carrying forward CW and shrub transect, TPA)
library(quantreg)

qr.SI<-rq(ht_annual~srHeight_Total+CrownLength+diff.G.1m+TPA.OS+SiteIndex_Value,tau=c(.5),data=annual.gr4)
summary(qr.SI)
aic.list.SQ<-c(aic.list.SQ,AIC(qr.SI)[1])
nlist.SQ<-c(nlist.SQ,length(qr.SI$y))

#Elev Quantreg (Carrying forward CW and shrub transect, TPA)
library(quantreg)

qr.elev<-rq(ht_annual~srHeight_Total+CrownLength+diff.G.1m+TPA.OS+elevation,tau=c(.5),data=annual.gr4)
summary(qr.elev)
aic.list.SQ<-c(aic.list.SQ,AIC(qr.elev)[1])
nlist.SQ<-c(nlist.SQ,length(qr.elev$y))

#Asp Quantreg (Carrying forward CW and shrub transect, TPA)
library(quantreg)

qr.asp<-rq(ht_annual~srHeight_Total+CrownLength+diff.G.1m+TPA.OS+cos_rad_asp,tau=c(.5),data=annual.gr4)
summary(qr.asp)
aic.list.SQ<-c(aic.list.SQ,AIC(qr.asp)[1])
nlist.SQ<-c(nlist.SQ,length(qr.asp$y))




#SI qr has an AIC> 4741 (OS TPA)


SQ.variable<-c("Slope","SI","Elevation","Aspect")

SQ.variable<-as.data.frame(SQ.variable)

SQ.aic<-as.data.frame(cbind(nlist.SQ,aic.list.SQ))


SQ.aic<-cbind(SQ.variable,SQ.aic)

is.numeric(SQ.aic$aic.list.SQ)
