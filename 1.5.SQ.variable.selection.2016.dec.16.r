#Import google earth generated slop_elev_aspect csv#
ge_sea<-read.csv("slope_elev_aspect.csv")

names(ge_sea)[2]<-"InstPlot"


######Site Index Variables######
#SI #

soverhist<-soverhist[! soverhist$Damage %in% dead.words,]

sinst.pipo<-sinst[! sinst$Installation %in% drp60,
                  c("Installation","SiteIndex_Value")]

#all PIPO installations have a PIPO SI except for 1, GC
sinst.pipo$Installation<-as.character(sinst.pipo$Installation)
annual.gr4<-merge(annual.gr4,sinst.pipo, by="Installation")

#Transform Aspect Variable#
ge_sea$sin_rad_asp<-sin((ge_sea$aspect*3.141)/180) 

ge_sea$cos_rad_asp<-cos((ge_sea$aspect*3.141)/180)


ge_sea$asp1<-(ge_sea$sin_rad_asp+ge_sea$cos_rad_asp)

annual.gr4$InstPlot<-paste(annual.gr4$Installation,annual.gr4$Plot,sep="")
annual.gr4$InstPlot<-as.character(annual.gr4$InstPlot)
ge_sea$InstPlot<-as.character(ge_sea$InstPlot)
annual.gr4<-merge(annual.gr4, ge_sea, by="InstPlot")



# #GAM for Site Index
# gam.SI<-gam(ht_annual~s(srHeight_Total)+s(SiteIndex_Value),data=annual.gr4, family=gaussian(link="identity"))
# summary(gam.SI)
# 
# par(mfrow=c(2,4),mar=c(4,4,1,2))
# plot(gam.SI,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# 
# #GAM for Slope
# gam.slope<-gam(ht_annual~s(srHeight_Total)+s(slope),data=annual.gr4, family=gaussian(link="identity"))
# summary(gam.slope)
# 
# 
# plot(gam.slope,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# 
# 
# #GAM for Elevation
# gam.elev<-gam(ht_annual~s(srHeight_Total)+s(elevation),data=annual.gr4, family=gaussian(link="identity"))
# summary(gam.elev)
# 
# plot(gam.elev,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# 
# #GAM for Aspect
# gam.aspect<-gam(ht_annual~s(srHeight_Total)+s(annual.gr4$cos_rad_asp),data=annual.gr4, family=gaussian(link="identity"))
# summary(gam.aspect)
# 
# plot(gam.aspect,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# 


#SI Quantreg LQMM
library(quantreg)
library(lqmm)

qr.SI<-lqmm(ht_annual~srHeight_Total+cratio+
            TPA.OS+SiteIndex_Value,random=~1,nK=100,
            control=list(LP_tol_ll=1e-01,LP_max_iter=1000,method="df"),
            group=conc,tau=c(.5),data=annual.gr4)
# summary(qr.SI)
aic.list.lqmm.SQ<-AIC(qr.SI)[1]
nlist.lqmm.SQ<-length(qr.SI$y)

#Slope Quantreg (Carrying forward CW and shrub transect, TPA)

qr.slope<-lqmm(ht_annual~srHeight_Total+cratio+
               # treeminus+
               TPA.OS+slopePercent,
               control=list(LP_tol_ll=1e-01,LP_max_iter=1000,method="df"),
               random=~1,nK=100,group=conc,tau=c(.5),data=annual.gr4)
# summary(qr.slope)
aic.list.lqmm.SQ<-c(aic.list.lqmm.SQ,AIC(qr.slope)[1])
nlist.lqmm.SQ<-c(nlist.lqmm.SQ,length(qr.slope$y))

#Elev Quantreg (Carrying forward CW and shrub transect, TPA)

qr.elev<-lqmm(ht_annual~srHeight_Total+cratio+
              # treeminus+
              TPA.OS+elevation,
              control=list(LP_tol_ll=1e-01,LP_max_iter=1000,method="df"),
              random=~1,nK=100,group=conc,tau=c(.5),data=annual.gr4)
# summary(qr.elev)
aic.list.lqmm.SQ<-c(aic.list.lqmm.SQ,AIC(qr.elev)[1])
nlist.lqmm.SQ<-c(nlist.lqmm.SQ,length(qr.elev$y))

#Asp Quantreg (Carrying forward CW and shrub transect, TPA)

qr.asp<-lqmm(ht_annual~srHeight_Total+cratio+
             # treeminus+
             TPA.OS+cos_rad_asp,control=list(LP_tol_ll=1e-01,LP_max_iter=1000,method="df"),
             random=~1,nK=100,group=conc,tau=c(.5),data=annual.gr4)
# summary(qr.asp)
aic.list.lqmm.SQ<-c(aic.list.lqmm.SQ,AIC(qr.asp)[1])
nlist.lqmm.SQ<-c(nlist.lqmm.SQ,length(qr.asp$y))

#Sea Interact.
qr.sea<-lqmm(ht_annual~srHeight_Total+cratio+
             TPA.OS+
             slopePercent + # goes with coefficient b1
             slopePercent:cos_rad_asp+#with b2
             slopePercent:sin_rad_asp+ #with b3
             slopePercent:log(elevation+1)+ #b4
             slopePercent:log(elevation+1):cos_rad_asp+ #b5
             slopePercent:log(elevation+1):sin_rad_asp+ #b6
             slopePercent:I(elevation^2)+  #b7
             slopePercent:I(elevation^2):cos_rad_asp+   #b8
             slopePercent:I(elevation^2):sin_rad_asp+   #b9
             elevation+ # b10
             I(elevation^2),
             control=list(LP_tol_ll=1e-01,LP_max_iter=3000,method="df"),#b11
           random=~1,group=conc,nK=100,
           tau=c(.5) ,  data=annual.gr4)


# summary(qr.sea)
aic.list.lqmm.SQ<-c(aic.list.lqmm.SQ,AIC(qr.sea)[1])
nlist.lqmm.SQ<-c(nlist.lqmm.SQ,length(qr.sea$y))




SQ.variable<-c("SI","Slope","Elevation","Aspect","SEA Int")

SQ.variable<-as.data.frame(SQ.variable)

SQ.aic<-as.data.frame(cbind(nlist.lqmm.SQ,aic.list.lqmm.SQ))

SQ.aic<-cbind(SQ.variable,SQ.aic)






# #SI Quantreg (Carrying forward CW and shrub transect, TPA)
# library(quantreg)
# 
# qr.SI<-rq(ht_annual~srHeight_Total+cratio+
#             # treeminus+
#             TPA.OS+SiteIndex_Value,tau=c(.5),data=annual.gr4)
# summary(qr.SI)
# aic.list.SQ<-AIC(qr.SI)[1]
# nlist.SQ<-length(qr.SI$y)
# 
# #Slope Quantreg (Carrying forward CW and shrub transect, TPA)
# 
# qr.slope<-rq(ht_annual~srHeight_Total+cratio+
#                # treeminus+
#                TPA.OS+slopePercent,tau=c(.5),data=annual.gr4)
# summary(qr.slope)
# aic.list.SQ<-c(aic.list.SQ,AIC(qr.slope)[1])
# nlist.SQ<-c(nlist.SQ,length(qr.slope$y))
# 
# #Elev Quantreg (Carrying forward CW and shrub transect, TPA)
# 
# qr.elev<-rq(ht_annual~srHeight_Total+cratio+
#               # treeminus+
#               TPA.OS+elevation,tau=c(.5),data=annual.gr4)
# summary(qr.elev)
# aic.list.SQ<-c(aic.list.SQ,AIC(qr.elev)[1])
# nlist.SQ<-c(nlist.SQ,length(qr.elev$y))
# 
# #Asp Quantreg (Carrying forward CW and shrub transect, TPA)
# 
# qr.asp<-rq(ht_annual~srHeight_Total+cratio+
#              # treeminus+
#              TPA.OS+cos_rad_asp,tau=c(.5),data=annual.gr4)
# summary(qr.asp)
# aic.list.SQ<-c(aic.list.SQ,AIC(qr.asp)[1])
# nlist.SQ<-c(nlist.SQ,length(qr.asp$y))
# 
# #Sea Interact.
# qr.sea<-rq(ht_annual~srHeight_Total+cratio+
#              # treeminus+
#              TPA.OS+
#              slopePercent + # goes with coefficient b1
#              slopePercent:cos_rad_asp + #with b2
#              slopePercent:sin_rad_asp + #with b3
#              slopePercent:log(elevation+1) + #b4
#              slopePercent:log(elevation+1):cos_rad_asp + #b5
#              slopePercent:log(elevation+1):sin_rad_asp + #b6
#              slopePercent:I(elevation^2) +   #b7
#              slopePercent:I(elevation^2):cos_rad_asp +   #b8
#              slopePercent:I(elevation^2):sin_rad_asp +   #b9
#              elevation + # b10
#              I(elevation^2) , #b11
#            tau=c(.5) ,  data=annual.gr4)
# 
# 
# summary(qr.sea)
# aic.list.SQ<-c(aic.list.SQ,AIC(qr.sea)[1])
# nlist.SQ<-c(nlist.SQ,length(qr.sea$y))
# 
# 
# 
# 
# SQ.variable<-c("SI","Slope","Elevation","Aspect","SEA Int")
# 
# SQ.variable<-as.data.frame(SQ.variable)
# 
# SQ.aic<-as.data.frame(cbind(nlist.SQ,aic.list.SQ))
# 
# SQ.aic<-cbind(SQ.variable,SQ.aic)
# 

