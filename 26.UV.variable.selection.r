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

#Reshapes 4 meter veg measurements
names(sstp4)[7:9]<-c("Cov4","Bas4","Top4")

stp4<-reshape(sstp4, direction="wide",idvar=
               c("Installation","Plot","STP","Year_Measurement"),
             timevar="Lifeform",v.names=c("Cov4","Bas4","Top4"))

#Makes NAs within coverage, base, and top veg meas columns = 0
veg.names<-names(stp4[,substring(names(stp4),4,4)=="."])

for(i in veg.names) {
  stp4[i][is.na(stp4[i])] <- 0
}

#Merges plot history (trt etc) and stp 1m veg meas
veg_record4<- merge(splot, stp4,by=c("Installation","Plot"))



#Merges annual small tree growth records with 4m veg records for each year
annual.gr3<- merge(annual.gr3, veg_record4,by=c("Installation","Plot","STP","Year_Measurement"))


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

annual.gr4<-merge(annual.gr3,agg.tran.data1,by=c("Installation","Plot","Year_Measurement"))
                  
#Investigate loss of rows in merging and lack of visibility in diff columns

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





#Removes 6th stp plots from analysis
annual.gr4<-annual.gr4[!annual.gr4$STP==6,]


#GAM for 1m polyveg cover
gam.1m.polv<-gam(ht_annual~s(srHeight_Total)+s(Cov.POLV),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.1m.polv)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.1m.polv,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

#GAM for 4m polyveg cover
gam.4m.polv<-gam(ht_annual~s(srHeight_Total)+s(Cov4.POLV),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.4m.polv)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.4m.polv,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


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
gam.tran.GR<-gam(ht_annual~s(srHeight_Total)+s(grass.ht.x),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.tran.GR)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.tran.GR,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


#Veg Quantreg
library(quantreg)

#QR for 1m polyveg cover
qr.1m.polv<-rq(ht_annual~srHeight_Total+Cov.POLV+STP,tau=c(.5),data=annual.gr4)
summary(qr.1m.polv)
aic.list.veg<-AIC(qr.1m.polv)[1]

#QR for 4m polyveg cover
qr.4m.polv<-rq(ht_annual~srHeight_Total+Cov4.POLV+STP,tau=c(.5),data=annual.gr4)
summary(qr.4m.polv)
aic.list.veg<-c(aic.list.veg,AIC(qr.4m.polv)[1])

#QR for 1m Forb cover
qr.1m.F<-rq(ht_annual~srHeight_Total+Cov.F+STP,tau=c(.5),data=annual.gr4)
summary(qr.1m.F)
aic.list.veg<-c(aic.list.veg,AIC(qr.1m.F)[1])

#QR for 4m Forb cover
qr.4m.F<-rq(ht_annual~srHeight_Total+Cov4.F+STP,tau=c(.5),data=annual.gr4)
summary(qr.4m.F)
aic.list.veg<-c(aic.list.veg,AIC(qr.4m.F)[1])

#QR for 1m Shrub cover
qr.1m.S<-rq(ht_annual~srHeight_Total+Cov.LS+STP,tau=c(.5),data=annual.gr4)
summary(qr.1m.S)
aic.list.veg<-c(aic.list.veg,AIC(qr.1m.S)[1])

#QR for 4m Shrub cover
qr.4m.S<-rq(ht_annual~srHeight_Total+Cov4.LS+STP,tau=c(.5),data=annual.gr4)
summary(qr.4m.S)
aic.list.veg<-c(aic.list.veg,AIC(qr.4m.S)[1])

#QR for transect grass height
qr.tran.gr<-rq(ht_annual~srHeight_Total+grass.ht.x+STP,tau=c(.5),data=annual.gr4)
summary(qr.tran.gr)
aic.list.veg<-c(aic.list.veg,AIC(qr.tran.gr)[1])

#QR for Shrub transect cover
qr.shrub.tran<-rq(ht_annual~srHeight_Total+diff.S+STP,tau=c(.5),data=annual.gr4)
summary(qr.shrub.tran)
aic.list.veg<-c(aic.list.veg,AIC(qr.shrub.tran)[1])

#QR for Forb transect cover
qr.forb.tran<-rq(ht_annual~srHeight_Total+diff.F+STP,tau=c(.5),data=annual.gr4)
summary(qr.forb.tran)
aic.list.veg<-c(aic.list.veg,AIC(qr.forb.tran)[1])

#QR for both Forb and Shrub Transect
qr.forb.shrub.tran<-rq(ht_annual~srHeight_Total+diff.F+diff.S+STP,tau=c(.5),data=annual.gr4)
summary(qr.forb.shrub.tranCW)
aic.list.veg<-c(aic.list.veg,AIC(qr.forb.shrub.tran)[1])

#surpising that shrub transect cover has the lowest aic for veg

veg.variable<-c("1m.poly","4m.poly","1m.forb","4m.forb",
                "1m.shrub","4m.shrub","Gr.tran","S.tran","F.tran",
                "FandS")

aic.list.veg<-t(as.data.frame(aic.list.veg))

colnames(aic.list.veg)<-(veg.variable)



#Four meter veg data appears to have a marginally lower AIC,
#Issue arrises when considering that this variable was only collected for half of the study 
#Perhaps not worth utilizing this variable and in doing so losing half of tree records...

#Shrub and forb transects have much lower aic than cover measurements, more involved from 
#practical 



#QR for 1m polyveg cover CW
qr.1m.polvCW<-rq(ht_annual~srHeight_Total+Cov.POLV+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qr.1m.polvCW)
aic.list.vegCW<-AIC(qr.1m.polvCW)[1]

#QR for 4m polyveg cover
qr.4m.polvCW<-rq(ht_annual~srHeight_Total+Cov4.POLV+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qr.4m.polvCW)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qr.4m.polvCW)[1])

#QR for 1m Forb cover
qr.1m.FCW<-rq(ht_annual~srHeight_Total+Cov.F+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qr.1m.FCW)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qr.1m.FCW)[1])

#QR for 4m Forb cover
qr.4m.FCW<-rq(ht_annual~srHeight_Total+Cov4.F+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qr.4m.FCW)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qr.4m.FCW)[1])

#QR for 1m Shrub cover
qr.1m.SCW<-rq(ht_annual~srHeight_Total+Cov.LS+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qr.1m.SCW)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qr.1m.SCW)[1])

#QR for 4m Shrub cover
qr.4m.SCW<-rq(ht_annual~srHeight_Total+Cov4.LS+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qr.4m.SCW)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qr.4m.SCW)[1])

#QR for transect grass height
qr.tran.grCW<-rq(ht_annual~srHeight_Total+grass.ht.x+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qr.tran.grCW)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qr.tran.grCW)[1])

#QR for Shrub transect cover
qr.shrub.tranCW<-rq(ht_annual~srHeight_Total+diff.S+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qr.shrub.tranCW)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qr.shrub.tranCW)[1])

#QR for Forb transect cover
qr.forb.tranCW<-rq(ht_annual~srHeight_Total+diff.F+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qr.forb.tranCW)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qr.forb.tranCW)[1])

#QR for both Forb and Shrub Transect
qr.forb.shrub.tranCW<-rq(ht_annual~srHeight_Total+diff.F+diff.S+CrownWidth,tau=c(.5),data=annual.gr4)
summary(qr.forb.shrub.tranCW)
aic.list.vegCW<-c(aic.list.vegCW,AIC(qr.forb.shrub.tranCW)[1])


#surpising that shrub transect cover has the lowest aic for veg



aic.list.vegCW<-t(as.data.frame(aic.list.vegCW))

colnames(aic.list.vegCW)<-(veg.variable)


rbind(aic.list.vegCW,aic.list.veg)

#AIC with CW much lower accross all vegetation measurements

