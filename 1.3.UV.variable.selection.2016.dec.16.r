US
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

#Aggregates transect data to the plot level
agg.tran.data <-aggregate(stran$diffT,
                    by=list("Installation"=stran$Installation,
                            "Plot"=stran$Plot,
                            "Year_Measurement"=stran$Year_Measurement,
                            "Lifeform"=stran$Lifeform),FUN=mean)#total/number of points

#Reshapes transect data so each plot is a row
agg.tran.data1<-reshape(agg.tran.data, direction="wide",idvar=
                c("Installation","Plot","Year_Measurement"),
              timevar="Lifeform",v.names="x")

names(agg.tran.data1)[4:6]<-c("diff.F","diff.null","diff.S")



#Merges aggregated transect data to the "big" df

annual.gr4<-merge(annual.gr4,agg.tran.data1,by=c("Installation","Plot","Year_Measurement"))
               
   
##Transect Grass Cover Data##



agg.tran.data.G <-aggregate(stranco$Pct_Grass,
                          by=list("Installation"=stranco$Installation,
                                  "Plot"=stranco$Plot,
                                  "Year_Measurement"=stranco$Year_Measurement
                                  ),FUN=mean)#total/number of points


names(agg.tran.data.G)[4]<-("tran.G")



#Merges aggregated transect data to the "big" df

annual.gr4<-merge(annual.gr4,agg.tran.data.G,by=c("Installation","Plot","Year_Measurement"))




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
                                  "Year_Measurement"=strangr$Year_Measurement),FUN=mean)

names(agg.grass.data)[4]<-"grass.ht"

agg.grass.data[4][is.na(agg.grass.data[4])] <- 0


annual.gr4<-merge(annual.gr4,agg.grass.data,by=c("Installation","Plot","Year_Measurement"))


#Removes trees with -inf ht_ annual...check the annual ht function function
annual.gr4$inf.ht<-is.infinite(annual.gr4$ht_annual)
annual.gr4<-annual.gr4[!annual.gr4$inf.ht==TRUE,]


#Removes 6th stp plots from analysis
annual.gr6<-annual.gr4[annual.gr4$STP==6,]

annual.gr4<-annual.gr4[!annual.gr4$STP==6,]





#GAM for 1m polyveg cover
gam.1m.polv<-gam(ht_annual~s(srHeight_Total)+s(Cov.POLV),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.1m.polv)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.1m.polv,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

#GAM for 1m polyveg diff
#gam.1m.polv.diff<-gam(ht_annual~s(srHeight_Total)+s(annual.gr4$diff.POLV.1m),data=annual.gr4, family=gaussian(link="identity"))
#summary(gam.4m.polv)
#Not enough (non-NA) data to do anything meaningful

#GAM for 1m F diff
gam.1m.F.diff<-gam(ht_annual~s(srHeight_Total)+s(annual.gr4$diff.F.1m),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.1m.F.diff)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.1m.F.diff,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

#GAM for 1m LS diff
gam.1m.LS.diff<-gam(ht_annual~s(srHeight_Total)+s(annual.gr4$diff.LS.1m),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.1m.LS.diff)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.1m.LS.diff,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

#GAM for 1m HS diff
gam.1m.HS.diff<-gam(ht_annual~s(srHeight_Total)+s(annual.gr4$diff.HS.1m),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.1m.HS.diff)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.1m.HS.diff,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


#GAM for Shrub transect data
gam.tran.S<-gam(ht_annual~s(srHeight_Total)+s(diff.S),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.tran.S)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.tran.S,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


#GAM for Forb transect data
gam.tran.F<-gam(ht_annual~s(srHeight_Total)+s(diff.F),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.tran.F)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.tran.F,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

#GAM for Grass transect data
gam.tran.GR<-gam(ht_annual~s(srHeight_Total)+s(grass.ht),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.tran.GR)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.tran.GR,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

#GAM for Grass transect data
gam.tran.GR.cov<-gam(ht_annual~s(srHeight_Total)+s(tran.G),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.tran.GR.cov)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.tran.GR.cov,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

#Veg Quantreg
library(quantreg)

#QR for 1m POLV cover
qr.1m.polv<-rq(ht_annual~srHeight_Total+Cov.POLV+STP,tau=c(.5),data=annual.gr4)
summary(qr.1m.polv)
aic.list.veg<-AIC(qr.1m.polv)[1]

#QR for 1m Forb cover
qr.1m.F<-rq(ht_annual~srHeight_Total+Cov.F+STP,tau=c(.5),data=annual.gr4)
summary(qr.1m.F)
aic.list.veg<-c(aic.list.veg,AIC(qr.1m.F)[1])

#QR for 1m LS cover
qr.1m.LS<-rq(ht_annual~srHeight_Total+Cov.LS+STP,tau=c(.5),data=annual.gr4)
summary(qr.1m.LS)
aic.list.veg<-c(aic.list.veg,AIC(qr.1m.LS)[1])

#QR for 1m HS cover
qr.1m.HS<-rq(ht_annual~srHeight_Total+Cov.HS+STP,tau=c(.5),data=annual.gr4)
summary(qr.1m.HS)
aic.list.veg<-c(aic.list.veg,AIC(qr.1m.HS)[1])

#QR for 1m Grass cover
qr.1m.G<-rq(ht_annual~srHeight_Total+Cov.G+STP,tau=c(.5),data=annual.gr4)
summary(qr.1m.G)
aic.list.veg<-c(aic.list.veg,AIC(qr.1m.G)[1])

#QR for 1m Forb diff
qr.1m.F<-rq(ht_annual~srHeight_Total+diff.F.1m+STP,tau=c(.5),data=annual.gr4)
summary(qr.1m.F)
aic.list.veg<-c(aic.list.veg,AIC(qr.1m.F)[1])

#QR for 1m LOW Shrub diff
qr.1m.S<-rq(ht_annual~srHeight_Total+diff.LS.1m+STP,tau=c(.5),data=annual.gr4)
summary(qr.1m.S)
aic.list.veg<-c(aic.list.veg,AIC(qr.1m.S)[1])

#QR for 1m High Shrub diff
qr.1m.HS<-rq(ht_annual~srHeight_Total+diff.HS.1m+STP,tau=c(.5),data=annual.gr4)
summary(qr.1m.HS)
aic.list.veg<-c(aic.list.veg,AIC(qr.1m.HS)[1])

#QR for 1m Grass diff
qr.1m.G<-rq(ht_annual~srHeight_Total+diff.G.1m+STP,tau=c(.5),data=annual.gr4)
summary(qr.1m.G)
aic.list.veg<-c(aic.list.veg,AIC(qr.1m.G)[1])


#QR for Forb transect cover
qr.forb.tran<-rq(ht_annual~srHeight_Total+diff.F+STP,tau=c(.5),data=annual.gr4)
summary(qr.forb.tran)
aic.list.veg<-c(aic.list.veg,AIC(qr.forb.tran)[1])

#QR for Shrub transect cover
qr.shrub.tran<-rq(ht_annual~srHeight_Total+diff.S+STP,tau=c(.5),data=annual.gr4)
summary(qr.shrub.tran)
aic.list.veg<-c(aic.list.veg,AIC(qr.shrub.tran)[1])


#QR for both Forb and Shrub Transect
#qr.forb.shrub.tran<-rq(ht_annual~srHeight_Total+diff.F+diff.S+STP,tau=c(.5),data=annual.gr4)
#summary(qr.forb.shrub.tranCW)
#aic.list.veg<-c(aic.list.veg,AIC(qr.forb.shrub.tran)[1])

#QR for transect grass height
qr.tran.gr<-rq(ht_annual~srHeight_Total+grass.ht+STP,tau=c(.5),data=annual.gr4)
summary(qr.tran.gr)
aic.list.veg<-c(aic.list.veg,AIC(qr.tran.gr)[1])

#QR for transect grass cover
qr.tran.gr.cov<-rq(ht_annual~srHeight_Total+tran.G+STP,tau=c(.5),data=annual.gr4)
summary(qr.tran.gr.cov)
aic.list.veg<-c(aic.list.veg,AIC(qr.tran.gr.cov)[1])

#surpising that shrub transect cover has the lowest aic for veg

veg.variable<-c("POLV.cov","F.cov","LS.cov","HS.cov","G.cov",
                "F.diff","LS.diff","HS.diff","G.diff",
                "F.tran","S.tran","G.tran.diff",
                "G.tran.cov"
               )

aic.list.veg<-t(as.data.frame(aic.list.veg))

colnames(aic.list.veg)<-(veg.variable)



#Four meter veg data appears to have a marginally lower AIC,
#Issue arrises when considering that this variable was only collected for half of the study 
#Perhaps not worth utilizing this variable and in doing so losing half of tree records...

#Shrub and forb transects have much lower aic than cover measurements, more involved from 
#practical 


#surpising that shrub transect cover has the lowest aic for veg


###CW STP VAR##########################################################################

#QR for 1m polyveg cover
qrCW.1m.polv<-rq(ht_annual~srHeight_Total+Cov.POLV+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qrCW.1m.polv)
aic.list.vegCW<-AIC(qrCW.1m.polv)[1]
nlist.UV<-length(qrCW.1m.polv$y)


#QR for 1m Forb cover
qrCW.1m.F<-rq(ht_annual~srHeight_Total+Cov.F+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qrCW.1m.F)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.F)[1])
nlist.UV<-c(nlist.UV, length(qrCW.1m.F$y))

#QR for 1m LOW Shrub cover
qrCW.1m.LS<-rq(ht_annual~srHeight_Total+Cov.LS+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qrCW.1m.LS)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.LS)[1])
nlist.UV<-c(nlist.UV, length(qrCW.1m.LS$y))

#QR for 1m High Shrub cover
qrCW.1m.HS<-rq(ht_annual~srHeight_Total+Cov.HS+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qrCW.1m.HS)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.HS)[1])
nlist.UV<-c(nlist.UV, length(qrCW.1m.HS$y))

#QR for 1m Grass cover
qrCW.1m.G<-rq(ht_annual~srHeight_Total+Cov.G+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qrCW.1m.G)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.G)[1])
nlist.UV<-c(nlist.UV, length(qrCW.1m.G$y))

#QR for 1m Forb diff
qrCW.1m.F<-rq(ht_annual~srHeight_Total+diff.F.1m+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qrCW.1m.F)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.F)[1])
nlist.UV<-c(nlist.UV, length(qrCW.1m.F$y))

#QR for 1m LOW Shrub diff
qrCW.1m.S<-rq(ht_annual~srHeight_Total+diff.LS.1m+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qrCW.1m.S)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.S)[1])
nlist.UV<-c(nlist.UV, length(qrCW.1m.S$y))

#QR for 1m High Shrub diff
qrCW.1m.HS<-rq(ht_annual~srHeight_Total+diff.HS.1m+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qrCW.1m.HS)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.HS)[1])
nlist.UV<-c(nlist.UV, length(qrCW.1m.HS$y))

#QR for 1m Grass diff
qrCW.1m.G<-rq(ht_annual~srHeight_Total+diff.G.1m+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qrCW.1m.G)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.1m.G)[1])
nlist.UV<-c(nlist.UV, length(qrCW.1m.G$y))


#QR for Forb transect cover
qrCW.forb.tran<-rq(ht_annual~srHeight_Total+diff.F+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qrCW.forb.tran)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.forb.tran)[1])
nlist.UV<-c(nlist.UV, length(qrCW.forb.tran$y))

#QR for Shrub transect cover
qrCW.shrub.tran<-rq(ht_annual~srHeight_Total+diff.S+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qrCW.shrub.tran)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.shrub.tran)[1])
nlist.UV<-c(nlist.UV, length(qrCW.shrub.tran$y))

#QR for both Forb and Shrub Transect
#qrCW.forb.shrub.tran<-rq(ht_annual~srHeight_Total+diff.F+diff.S+CrownWidth,tau=c(.5),data=annual.gr4)
#summary(qrCW.forb.shrub.tranCW)
#aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.forb.shrub.tran)[1])

#QR for transect grass height
qrCW.tran.gr<-rq(ht_annual~srHeight_Total+grass.ht+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qrCW.tran.gr)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.tran.gr)[1])
nlist.UV<-c(nlist.UV, length(qrCW.tran.gr$y))

#QR for transect grass cover
qrCW.tran.gr.cov<-rq(ht_annual~srHeight_Total+tran.G+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qrCW.tran.gr.cov)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qrCW.tran.gr.cov)[1])
nlist.UV<-c(nlist.UV, length(qrCW.tran.gr.cov$y))

length(aic.list.vegCW)
length(nlist.UV)

UV.aic<-as.data.frame(cbind(nlist.UV,aic.list.vegCW))
UV.aic$aic.list.vegCW<-as.numeric(UV.aic$aic.list.vegCW)

veg.variable<-as.data.frame(veg.variable)

UV.aic<-cbind(veg.variable,UV.aic)

is.numeric(UV.aic$aic.list.vegCW)


sum(is.na(annual.gr4$diff.F.1m)==TRUE)

#veg.aic.list<-rbind(aic.list.vegCW,aic.list.veg)

#AIC with CW much lower accross all vegetation measurements

is.na.list<-sum(is.na(annual.gr4$Cov.POLV)==TRUE)
is.na.list<-c(is.na.list, sum(is.na(annual.gr4$Cov.F)==TRUE))
is.na.list<-c(is.na.list, sum(is.na(annual.gr4$Cov.LS)==TRUE))
is.na.list<-c(is.na.list, sum(is.na(annual.gr4$Cov.HS)==TRUE))
is.na.list<-c(is.na.list, sum(is.na(annual.gr4$Cov.G)==TRUE))

is.na.list<-c(is.na.list, sum(is.na(annual.gr4$diff.F.1m)==TRUE))
is.na.list<-c(is.na.list, sum(is.na(annual.gr4$diff.LS.1m)==TRUE))
is.na.list<-c(is.na.list, sum(is.na(annual.gr4$diff.HS.1m)==TRUE))
is.na.list<-c(is.na.list, sum(is.na(annual.gr4$diff.G.1m)==TRUE))

is.na.list<-c(is.na.list, sum(is.na(annual.gr4$diff.F)==TRUE))
is.na.list<-c(is.na.list, sum(is.na(annual.gr4$diff.LS)==TRUE))
is.na.list<-c(is.na.list, sum(is.na(annual.gr4$grass.ht)==TRUE))
is.na.list<-c(is.na.list, sum(is.na(annual.gr4$tran.G)==TRUE))

#veg.aic.list<-rbind(is.na.list,veg.aic.list)
#colnames(veg.aic.list)<-(veg.variable)

#AIC of HS volume is much lower than others, however,
#it also missing for most records



