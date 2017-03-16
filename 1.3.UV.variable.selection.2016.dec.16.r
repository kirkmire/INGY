######Understory Non-Tree Variables######
#Percentage cover 1m, upper and lower extent of height


#Abbreviates row headings in veg plot measurements
names(sstp1)[7:8]<-c("Cov","Bas")

#Reshaped 1 meter small tree plot veg measurements so that each stp record year has its
#own associated Forb, Grass, Low Shrub, High Shrub and Polyveg measurements

stp<-reshape(sstp1, direction="wide",idvar=
                     c("Installation","Plot","STP","Year_Measurement"),
                   timevar="Lifeform",v.names=c("Cov","Bas","Top"))

#Note: no base and top height measurements for polyveg lifeform in 1m and 4m

#Makes NAs within coverage, base, and top veg meas columns = 0
veg.names<-names(stp[,substring(names(stp),4,4)=="."])

for(i in veg.names) {
  stp[i][is.na(stp[i])] <- 0
}

#Merges plot history (trt etc) and stp 1m veg meas
veg_record<- merge(splot, stp,by=c("Installation","Plot"))


#Merges annual small tree growth records with 1m veg records for each year
annual.gr3<- merge(annual.gr, veg_record,by=c("Installation","Plot","STP","Year_Measurement"))
veg_record$STP[1]

#4m data not collected until 2007, ok to compare to 1m data 
#that was colected throughout study (1998)?
#cuts tree records from ~5,500 to ~1,000

#Reshapes 4 meter veg measurements
#names(sstp4)[7:9]<-c("Cov4","Bas4","Top4")

#stp4<-reshape(sstp4, direction="wide",idvar=
#               c("Installation","Plot","STP","Year_Measurement"),
#             timevar="Lifeform",v.names=c("Cov4","Bas4","Top4"))

#Makes NAs within coverage, base, and top veg meas columns = 0
#veg.names<-names(stp4[,substring(names(stp4),4,4)=="."])

#for(i in veg.names) {
#  stp4[i][is.na(stp4[i])] <- 0
#}

#Merges plot history (trt etc) and stp 1m veg meas
#veg_record4<- merge(splot, stp4,by=c("Installation","Plot"))



#Merges annual small tree growth records with 4m veg records for each year
#annual.gr3<- merge(annual.gr3, veg_record4,by=c("Installation","Plot","STP","Year_Measurement"))



##1m S, F, and PLOV diffs##

sstp1$diff.1m<-sstp1$Top-sstp1$Bas

#Assigns zeros to NA values (where no veg present)

veg.names<-"diff.1m"

for(i in veg.names) {
  sstp1[i][is.na(sstp1[i])] <- 0
}

#Aggragates 1m F diff to the plot level
agg.1m.data <-aggregate(sstp1$diff.1m,
                          by=list("Installation"=sstp1$Installation,
                                  "Plot"=sstp1$Plot,
                                  "Year_Measurement"=sstp1$Year_Measurement,
                                  "Lifeform"=sstp1$Lifeform),FUN=mean)


agg.1m.data1<-reshape(agg.1m.data, direction="wide",idvar=
                          c("Installation","Plot","Year_Measurement"),
                        timevar="Lifeform",v.names="x")

veg.names<-names(agg.1m.data1[,substring(names(agg.1m.data1),2,2)=="."])

for(i in veg.names) {
  agg.1m.data1[i][is.na(agg.1m.data1[i])] <- 0
}


names(agg.1m.data1)[4:7]<-c("diff.F.1m","diff.G.1m","diff.HS.1m","diff.LS.1m")
names(agg.1m.data1)[9]<-c("diff.POLV.1m")


#Merges aggregated 1m data to the "big" df

annual.gr4<-merge(annual.gr3,agg.1m.data1,by=c("Installation","Plot","Year_Measurement"))







##Transect Data##
names(stran)[9:10]<-c("basT","topT")

#Assigns zeros to NA values (transect points where no veg present)
veg.T.names<-names(stran[,substring(names(stran),4,4)=="T"])

for(i in veg.T.names) {
  stran[i][is.na(stran[i])] <- 0
}
#Count transect observation number
#If not figure out missing
#probably supposed to be zero


#calculate difference in top and base meas
stran$diffT<-stran$topT-stran$basT

#Aggregates transect data to the STP level
agg.tran.data <-aggregate(stran$diffT,
                    by=list("Installation"=stran$Installation,
                            "Plot"=stran$Plot,
                            "STP"=stran$Transect,
                            "Year_Measurement"=stran$Year_Measurement,
                            "Lifeform"=stran$Lifeform1),FUN=mean)#total/number of points

#Reshapes transect data so each stp is a row
agg.tran.data1<-reshape(agg.tran.data, direction="wide",idvar=
                c("Installation","Plot","STP","Year_Measurement"),
              timevar="Lifeform",v.names="x")

names(agg.tran.data1)[5:8]<-c("diff.F","diff.G","diff.HS","diff.LS")

tran.names<-names(agg.tran.data1[,substring(names(agg.tran.data1),5,5)=="."])

for(i in tran.names) {
  agg.tran.data1[i][is.na(agg.tran.data1[i])] <- 0
}


#Merges aggregated transect data to the "big" df

annual.gr4<-merge(annual.gr4,agg.tran.data1,by=c("Installation","Plot","STP","Year_Measurement"))
               
   
##Transect Grass Cover Data##



agg.tran.data.G <-aggregate(stranco$Pct_Grass,
                          by=list("Installation"=stranco$Installation,
                                  "Plot"=stranco$Plot,
                                  "STP"=stranco$Transect,
                                  "Year_Measurement"=stranco$Year_Measurement
                                  ),FUN=mean)#total/number of points


names(agg.tran.data.G)[5]<-("tran.G")



#Merges aggregated transect data to the "big" df

annual.gr4<-merge(annual.gr4,agg.tran.data.G,by=c("Installation","Plot","STP","Year_Measurement"))




#code to remove all .y variables from df 

y.names<-numeric(0)

for(i in 3:18){
  y.names<-c(y.names,names(annual.gr4[,substring(names(annual.gr4),i-1,i)==".y"]))
}

annual.gr4<-annual.gr4[,! names(annual.gr4) %in% y.names]



#unsure what "NA" or "NULL" lifeforms translates to
#protocol seems tohave changed in later years of the study in
#favor of not distinguishing between shrubs and forbs

#Transect grass data
agg.grass.data <-aggregate(strangr$Top,
                          by=list("Installation"=strangr$Installation,
                                  "Plot"=strangr$Plot,
                                  "STP"=strangr$Transect,
                                  "Year_Measurement"=strangr$Year_Measurement),FUN=mean)

names(agg.grass.data)[5]<-"grass.ht"

agg.grass.data[5][is.na(agg.grass.data[5])] <- 0


annual.gr4<-merge(annual.gr4,agg.grass.data,by=c("Installation","Plot","STP","Year_Measurement"))


#Removes trees with -inf ht_ annual...check the annual ht function function
annual.gr4$inf.ht<-is.infinite(annual.gr4$ht_annual)
annual.gr4<-annual.gr4[!annual.gr4$inf.ht==TRUE,]

#Function for height difference between top height of tallest shrub on each 
#stp vegplot
#Find diff between init tree height and max shrub height

init_tree_shrub_ht_diff<-function(Installation,Plot,STP,Year,height){
  #Installation<-"KC"
  #Plot<-1
  #STP<-1
  #Year<-2010
  #height<-10
  shrub_ht<-veg_record[veg_record$Installation==Installation&
                         veg_record$Plot==Plot&
                         veg_record$STP==STP&
                         veg_record$Year_Measurement==Year,]
  
  max.ht.shrub<-max(shrub_ht$Top.LS,shrub_ht$Top.HS,shrub_ht$Top.F)
  tree.ht.minus.shrub<-height-max.ht.shrub
  tree.ht.minus.shrub
  }

annual.gr4$treeminus<-0
annual.gr4$Installation<-as.character(annual.gr4$Installation)
veg_record$Installation<-as.character(veg_record$Installation)

for(i in 1:nrow(annual.gr4)){
  annual.gr4$treeminus[i]<-init_tree_shrub_ht_diff(
    annual.gr4$Installation[i], 
    annual.gr4$Plot[i],
    annual.gr4$STP[i],
    annual.gr4$Year_Measurement[i],
    annual.gr4$Height_Total[i]
    )
}

mean(annual.gr4$treeminus)

#Function for height difference between top height of tallest shrub on each 
#stp vegplot
#Find diff between init tree height and max shrub height

agg.tran.data.max <-aggregate(stran$topT,
                          by=list("Installation"=stran$Installation,
                                  "Plot"=stran$Plot,
                                  "STP"=stran$Transect,
                                  "Year_Measurement"=stran$Year_Measurement,
                                  "Lifeform"=stran$Lifeform1),FUN=max)#total/number of points

#Reshapes transect data so each stp is a row
agg.tran.data.max1<-reshape(agg.tran.data.max, direction="wide",idvar=
                          c("Installation","Plot","STP","Year_Measurement"),
                        timevar="Lifeform",v.names="x")

#Assigns zeros to NA values (transect points where no veg present)
agg.T.names<-names(agg.tran.data.max1[,substring(names(agg.tran.data.max1),1,1)=="x"])

for(i in agg.T.names) {
  agg.tran.data.max1[i][is.na(agg.tran.data.max1[i])] <- 0
}

init_tree_shrub_ht_diff_trans<-function(Installation,Plot,STP,Year,height){
  #Installation<-"KC"
  #Plot<-1
  #STP<-1
  #Year<-2010
  #height<-10
  shrub_ht<-agg.tran.data.max1[agg.tran.data.max1$Installation==Installation&
                         agg.tran.data.max1$Plot==Plot&
                         agg.tran.data.max1$STP==STP&
                         agg.tran.data.max1$Year_Measurement==Year,]
  
  max.ht.shrub<-max(shrub_ht$x.F,shrub_ht$x.HS,shrub_ht$x.LS,shrub_ht$x.S)
  # tree.ht.minus.shrub<-height-max.ht.shrub
  tree.ht.minus.shrub<-max.ht.shrub
  tree.ht.minus.shrub
}

annual.gr4$treeminus_trans<-0
agg.tran.data.max1$Installation<-as.character(agg.tran.data.max1$Installation)

for(i in 1:nrow(annual.gr4)){
  annual.gr4$treeminus_trans[i]<-init_tree_shrub_ht_diff_trans(
    annual.gr4$Installation[i], 
    annual.gr4$Plot[i],
    annual.gr4$STP[i],
    annual.gr4$Year_Measurement[i],
    annual.gr4$Height_Total[i]
  )
}

mean(annual.gr4$treeminus_trans)
mean(annual.gr4$treeminus)



#Removes 6th stp plots from analysis
#Makes seperate dataframe for witheld data

annual.gr6<-annual.gr4[annual.gr4$STP_rand==6,]

annual.gr4<-annual.gr4[!annual.gr4$STP_rand==6,]

#Creates crown ratio variable for annual gr4
annual.gr4$cratio<- annual.gr4$CrownLength/annual.gr4$Height_Total

#Selects tree record of annual growth from similar installations
annual.grUV<-annual.gr4[annual.gr4$Installation %in% sim, ]



# 
# #GAM for 1m polyveg cover
# gam.1m.polv<-gam(ht_annual~s(srHeight_Total)+s(Cov.POLV),data=annual.grUV, family=gaussian(link="identity"))
# summary(gam.1m.polv)
# 
# par(mfrow=c(2,4),mar=c(4,4,1,2))
# plot(gam.1m.polv,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# 
# #GAM for 1m polyveg diff
# #gam.1m.polv.diff<-gam(ht_annual~s(srHeight_Total)+s(annual.grUV$diff.POLV.1m),data=annual.grUV, family=gaussian(link="identity"))
# #summary(gam.4m.polv)
# #Not enough (non-NA) data to do anything meaningful
# 
# #GAM for 1m F diff
# gam.1m.F.diff<-gam(ht_annual~s(srHeight_Total)+s(annual.grUV$diff.F.1m),data=annual.grUV, family=gaussian(link="identity"))
# summary(gam.1m.F.diff)
# 
# plot(gam.1m.F.diff,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# 
# #GAM for 1m LS diff
# gam.1m.LS.diff<-gam(ht_annual~s(srHeight_Total)+s(annual.grUV$diff.LS.1m),data=annual.grUV, family=gaussian(link="identity"))
# summary(gam.1m.LS.diff)
# 
# plot(gam.1m.LS.diff,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# 
# #GAM for 1m HS diff
# gam.1m.HS.diff<-gam(ht_annual~s(srHeight_Total)+s(annual.grUV$diff.HS.1m),data=annual.grUV, family=gaussian(link="identity"))
# summary(gam.1m.HS.diff)
# 
# plot(gam.1m.HS.diff,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# #Print#
# 
# #GAM for LS transect data
# gam.tran.LS<-gam(ht_annual~s(srHeight_Total)+s(diff.LS),data=annual.grUV, family=gaussian(link="identity"))
# summary(gam.tran.LS)
# 
# par(mfrow=c(2,4),mar=c(4,4,1,2))
# plot(gam.tran.LS,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# 
# #GAM for HS transect data
# gam.tran.HS<-gam(ht_annual~s(srHeight_Total)+s(diff.HS),data=annual.grUV, family=gaussian(link="identity"))
# summary(gam.tran.HS)
# 
# par(mfrow=c(2,4),mar=c(4,4,1,2))
# plot(gam.tran.HS,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# 
# #GAM for Forb transect data
# gam.tran.F<-gam(ht_annual~s(srHeight_Total)+s(diff.F),data=annual.grUV, family=gaussian(link="identity"))
# summary(gam.tran.F)
# 
# plot(gam.tran.F,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# 
# #GAM for Grass transect data
# gam.tran.GR<-gam(ht_annual~s(srHeight_Total)+s(grass.ht),data=annual.grUV, family=gaussian(link="identity"))
# summary(gam.tran.GR)
# 
# plot(gam.tran.GR,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# #Print#
# 
# #GAM for Grass transect data
# gam.tran.GR.cov<-gam(ht_annual~s(srHeight_Total)+s(tran.G),data=annual.grUV, family=gaussian(link="identity"))
# summary(gam.tran.GR.cov)
# 
# plot(gam.tran.GR.cov,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# 
# 
# 
# #GAM for 1m HS cover
# gam.tran.HS.cov<-gam(ht_annual~s(srHeight_Total)+s(Cov.HS),data=annual.grUV, family=gaussian(link="identity"))
# summary(gam.tran.HS.cov)
# 
# par(mfrow=c(2,4),mar=c(4,4,1,2))
# plot(gam.tran.HS.cov,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# 
# #GAM for 1m Grass cover
# gam.G.cov<-gam(ht_annual~s(srHeight_Total)+s(Cov.G),data=annual.grUV, family=gaussian(link="identity"))
# summary(gam.G.cov)
# 
# plot(gam.G.cov,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# 
# #GAM for 1m Forb cover
# gam.F.cov<-gam(ht_annual~s(srHeight_Total)+s(Cov.F),data=annual.grUV, family=gaussian(link="identity"))
# summary(gam.F.cov)
# 
# plot(gam.F.cov,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# 
# #GAM for 1m LS cover
# gam.LS.cov<-gam(ht_annual~s(srHeight_Total)+s(Cov.LS),data=annual.grUV, family=gaussian(link="identity"))
# summary(gam.LS.cov)
# 
# plot(gam.LS.cov,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# ##Print##
# 
# #GAM for 1m Grass diff
# gam.G.diff.cov<-gam(ht_annual~s(srHeight_Total)+s(diff.G.1m),data=annual.grUV, family=gaussian(link="identity"))
# summary(gam.G.diff.cov)
# 
# par(mfrow=c(2,4),mar=c(4,4,1,2))
# plot(gam.G.diff.cov,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# 
# #GAM for init tree ht veg diff
# init.diff.1m<-gam(ht_annual~srHeight_Total+annual.grUV$cratio+s(annual.grUV$Cov.POLV),data=annual.grUV, family=gaussian(link="identity"))
# summary(init.diff.1m)
# 
# 
# AIC(init.diff.1m)
# par(mfrow=c(2,4),mar=c(4,4,1,2))
# plot(init.diff.1m,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
# 
# #GAM for init tree ht veg diff trans
# init.diff.1m.trans<-gam(ht_annual~s(srHeight_Total)+s(treeminus_trans),data=annual.grUV, family=gaussian(link="identity"))
# summary(init.diff.1m.trans)
# 
# par(mfrow=c(2,4),mar=c(4,4,1,2))
# plot(init.diff.1m.trans,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)




veg.variable<-c("Nothing","POLV.cov","F.cov","LS.cov","HS.cov","G.cov",
               "LS.diff","HS.diff",
                "LS.tran","HS.tran","G.tran.diff",
                "G.tran.cov","mx.vg.diff1m","mx.vg.diff.tr"
               )


###CR VAR LQMM
library(lqmm)



qrCW.noth<-rq(ht_annual~srHeight_Total+cratio,random=~1,group=conc,tau=c(.5),data=annual.grUV)
summary(qrCW.noth)
aic.lqmm.list.vegCW<-AIC(qrCW.noth)[1]
nlqmm.list.UV<-length(qrCW.noth$y)


#QR for 1m polyveg cover
qrCW.1m.polv<-rq(ht_annual~srHeight_Total+Cov.POLV+cratio,random=~1,group=conc,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.polv)
aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qrCW.1m.polv)[1])
nlqmm.list.UV<-c(nlqmm.list.UV,length(qrCW.1m.polv$y))


#QR for 1m Forb cover
qrCW.1m.F<-rq(ht_annual~srHeight_Total+Cov.F+cratio,random=~1,group=conc,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.F)
aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qrCW.1m.F)[1])
nlqmm.list.UV<-c(nlqmm.list.UV, length(qrCW.1m.F$y))

#QR for 1m LOW Shrub cover
qrCW.1m.LS<-rq(ht_annual~srHeight_Total+Cov.LS+cratio,random=~1,group=conc,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.LS)
aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qrCW.1m.LS)[1])
nlqmm.list.UV<-c(nlqmm.list.UV, length(qrCW.1m.LS$y))

#QR for 1m High Shrub cover
qrCW.1m.HS<-rq(ht_annual~srHeight_Total+Cov.HS+cratio,random=~1,group=conc,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.HS)
aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qrCW.1m.HS)[1])
nlqmm.list.UV<-c(nlqmm.list.UV, length(qrCW.1m.HS$y))

#QR for 1m Grass cover
qrCW.1m.G<-rq(ht_annual~srHeight_Total+Cov.G+cratio,random=~1,group=conc,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.G)
aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qrCW.1m.G)[1])
nlqmm.list.UV<-c(nlqmm.list.UV, length(qrCW.1m.G$y))

#QR for 1m Forb diff
#qrCW.1m.F<-rq(ht_annual~srHeight_Total+diff.F.1m+cratio,tau=c(.5),data=annual.grUV)
#summary(qrCW.1m.F)
#aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qrCW.1m.F)[1])
#nlqmm.list.UV<-c(nlqmm.list.UV, length(qrCW.1m.F$y))

#QR for 1m LOW Shrub diff
qrCW.1m.LS<-rq(ht_annual~srHeight_Total+diff.LS.1m+cratio,random=~1,group=conc,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.LS)
aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qrCW.1m.LS)[1])
nlqmm.list.UV<-c(nlqmm.list.UV, length(qrCW.1m.LS$y))

#QR for 1m High Shrub diff
qrCW.1m.HS<-rq(ht_annual~srHeight_Total+diff.HS.1m+cratio,random=~1,group=conc,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.HS)
aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qrCW.1m.HS)[1])
nlqmm.list.UV<-c(nlqmm.list.UV, length(qrCW.1m.HS$y))

#QR for 1m Grass diff
#qrCW.1m.G<-rq(ht_annual~srHeight_Total+diff.G.1m+cratio,tau=c(.5),data=annual.grUV)
#summary(qrCW.1m.G)
#aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qrCW.1m.G)[1])
#nlqmm.list.UV<-c(nlqmm.list.UV, length(qrCW.1m.G$y))


#QR for Forb transect cover
#qrCW.forb.tran<-rq(ht_annual~srHeight_Total+diff.F+cratio,tau=c(.5),data=annual.grUV)
#summary(qrCW.forb.tran)
#aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qrCW.forb.tran)[1])
#nlqmm.list.UV<-c(nlqmm.list.UV, length(qrCW.forb.tran$y))

#QR for LS transect cover
qr.LS.tran<-rq(ht_annual~srHeight_Total+diff.LS+cratio,random=~1,group=conc,tau=c(.5),data=annual.grUV)
summary(qr.LS.tran)
aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qr.LS.tran)[1])
nlqmm.list.UV<-c(nlqmm.list.UV, length(qr.LS.tran$y))

#QR for HS transect cover
qr.HS.tran<-rq(ht_annual~srHeight_Total+diff.HS+cratio,random=~1,group=conc,tau=c(.5),data=annual.grUV)
summary(qr.HS.tran)
aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qr.HS.tran)[1])
nlqmm.list.UV<-c(nlqmm.list.UV, length(qr.HS.tran$y))

#QR for both Forb and Shrub Transect
#qrCW.forb.shrub.tran<-rq(ht_annual~srHeight_Total+diff.F+diff.S+cratio,tau=c(.5),data=annual.grUV)
#summary(qrCW.forb.shrub.tranCW)
#aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qrCW.forb.shrub.tran)[1])

#QR for transect grass height
qrCW.tran.gr<-rq(ht_annual~srHeight_Total+grass.ht+cratio,random=~1,group=conc,tau=c(.5),data=annual.grUV)
summary(qrCW.tran.gr)
aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qrCW.tran.gr)[1])
nlqmm.list.UV<-c(nlqmm.list.UV, length(qrCW.tran.gr$y))

#QR for transect grass cover
qrCW.tran.gr.cov<-rq(ht_annual~srHeight_Total+tran.G+cratio,random=~1,group=conc,tau=c(.5),data=annual.grUV)
summary(qrCW.tran.gr.cov)
aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qrCW.tran.gr.cov)[1])
nlqmm.list.UV<-c(nlqmm.list.UV, length(qrCW.tran.gr.cov$y))

#QR for init tree height max veg difference (1m veg plot)
qrCW.1m.max.vg.diff<-rq(ht_annual~srHeight_Total+annual.grUV$treeminus+cratio,random=~1,group=conc,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.max.vg.diff)
aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qrCW.1m.max.vg.diff)[1])
nlqmm.list.UV<-c(nlqmm.list.UV, length(qrCW.1m.max.vg.diff$y))

#QR for init tree height max veg difference (transect)
qrCW.1m.max.vg.diff.tran<-rq(ht_annual~srHeight_Total+annual.grUV$treeminus+cratio,random=~1,group=conc,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.max.vg.diff.tran)
aic.lqmm.list.vegCW<-c(aic.lqmm.list.vegCW,AIC(qrCW.1m.max.vg.diff.tran)[1])
nlqmm.list.UV<-c(nlqmm.list.UV, length(qrCW.1m.max.vg.diff.tran$y))


length(aic.lqmm.list.vegCW)
length(nlqmm.list.UV)
length(veg.variable)

UV.aic<-as.data.frame(cbind(nlqmm.list.UV,aic.lqmm.list.vegCW))
UV.aic$aic.lqmm.list.vegCW<-as.numeric(UV.aic$aic.lqmm.list.vegCW)

veg.variable<-as.data.frame(veg.variable)

UV.aic<-cbind(veg.variable,UV.aic)

###CR VAR##########################################################################
#QR for 1m Nothing
qrCW.noth<-rq(ht_annual~srHeight_Total+cratio,tau=c(.5),data=annual.grUV)
summary(qrCW.noth)
aic.list.vegCW<-AIC(qrCW.noth)[1]
nlist.UV<-length(qrCW.noth$y)


#QR for 1m polyveg cover
qrCW.1m.polv<-rq(ht_annual~srHeight_Total+Cov.POLV+cratio,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.polv)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.polv)[1])
nlist.UV<-c(nlist.UV,length(qrCW.1m.polv$y))


#QR for 1m Forb cover
qrCW.1m.F<-rq(ht_annual~srHeight_Total+Cov.F+cratio,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.F)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.F)[1])
nlist.UV<-c(nlist.UV, length(qrCW.1m.F$y))

#QR for 1m LOW Shrub cover
qrCW.1m.LS<-rq(ht_annual~srHeight_Total+Cov.LS+cratio,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.LS)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.LS)[1])
nlist.UV<-c(nlist.UV, length(qrCW.1m.LS$y))

#QR for 1m High Shrub cover
qrCW.1m.HS<-rq(ht_annual~srHeight_Total+Cov.HS+cratio,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.HS)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.HS)[1])
nlist.UV<-c(nlist.UV, length(qrCW.1m.HS$y))

#QR for 1m Grass cover
qrCW.1m.G<-rq(ht_annual~srHeight_Total+Cov.G+cratio,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.G)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.G)[1])
nlist.UV<-c(nlist.UV, length(qrCW.1m.G$y))

#QR for 1m Forb diff
#qrCW.1m.F<-rq(ht_annual~srHeight_Total+diff.F.1m+cratio,tau=c(.5),data=annual.grUV)
#summary(qrCW.1m.F)
#aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.F)[1])
#nlist.UV<-c(nlist.UV, length(qrCW.1m.F$y))

#QR for 1m LOW Shrub diff
qrCW.1m.LS<-rq(ht_annual~srHeight_Total+diff.LS.1m+cratio,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.LS)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.LS)[1])
nlist.UV<-c(nlist.UV, length(qrCW.1m.LS$y))

#QR for 1m High Shrub diff
qrCW.1m.HS<-rq(ht_annual~srHeight_Total+diff.HS.1m+cratio,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.HS)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.HS)[1])
nlist.UV<-c(nlist.UV, length(qrCW.1m.HS$y))

#QR for 1m Grass diff
#qrCW.1m.G<-rq(ht_annual~srHeight_Total+diff.G.1m+cratio,tau=c(.5),data=annual.grUV)
#summary(qrCW.1m.G)
#aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.G)[1])
#nlist.UV<-c(nlist.UV, length(qrCW.1m.G$y))


#QR for Forb transect cover
#qrCW.forb.tran<-rq(ht_annual~srHeight_Total+diff.F+cratio,tau=c(.5),data=annual.grUV)
#summary(qrCW.forb.tran)
#aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.forb.tran)[1])
#nlist.UV<-c(nlist.UV, length(qrCW.forb.tran$y))

#QR for LS transect cover
qr.LS.tran<-rq(ht_annual~srHeight_Total+diff.LS+cratio,tau=c(.5),data=annual.grUV)
summary(qr.LS.tran)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qr.LS.tran)[1])
nlist.UV<-c(nlist.UV, length(qr.LS.tran$y))

#QR for HS transect cover
qr.HS.tran<-rq(ht_annual~srHeight_Total+diff.HS+cratio,tau=c(.5),data=annual.grUV)
summary(qr.HS.tran)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qr.HS.tran)[1])
nlist.UV<-c(nlist.UV, length(qr.HS.tran$y))

#QR for both Forb and Shrub Transect
#qrCW.forb.shrub.tran<-rq(ht_annual~srHeight_Total+diff.F+diff.S+cratio,tau=c(.5),data=annual.grUV)
#summary(qrCW.forb.shrub.tranCW)
#aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.forb.shrub.tran)[1])

#QR for transect grass height
qrCW.tran.gr<-rq(ht_annual~srHeight_Total+grass.ht+cratio,tau=c(.5),data=annual.grUV)
summary(qrCW.tran.gr)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.tran.gr)[1])
nlist.UV<-c(nlist.UV, length(qrCW.tran.gr$y))

#QR for transect grass cover
qrCW.tran.gr.cov<-rq(ht_annual~srHeight_Total+tran.G+cratio,tau=c(.5),data=annual.grUV)
summary(qrCW.tran.gr.cov)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.tran.gr.cov)[1])
nlist.UV<-c(nlist.UV, length(qrCW.tran.gr.cov$y))

#QR for init tree height max veg difference (1m veg plot)
qrCW.1m.max.vg.diff<-rq(ht_annual~srHeight_Total+annual.grUV$treeminus+cratio,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.max.vg.diff)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.max.vg.diff)[1])
nlist.UV<-c(nlist.UV, length(qrCW.1m.max.vg.diff$y))

#QR for init tree height max veg difference (transect)
qrCW.1m.max.vg.diff.tran<-rq(ht_annual~srHeight_Total+annual.grUV$treeminus+cratio,tau=c(.5),data=annual.grUV)
summary(qrCW.1m.max.vg.diff.tran)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.max.vg.diff.tran)[1])
nlist.UV<-c(nlist.UV, length(qrCW.1m.max.vg.diff.tran$y))


length(aic.list.vegCW)
length(nlist.UV)
length(veg.variable)

UV.aic<-as.data.frame(cbind(nlist.UV,aic.list.vegCW))
UV.aic$aic.list.vegCW<-as.numeric(UV.aic$aic.list.vegCW)

veg.variable<-as.data.frame(veg.variable)

UV.aic<-cbind(veg.variable,UV.aic)



