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

#calculate difference in top and base meas
stran$diffT<-stran$topT-stran$basT

stran1<-ddply(stran, c("Installation","Plot","Transect","Year_Measurement","Point","Lifeform"),
              basT=mean(basT),topT=mean(topT))

aggdata <-aggregate(stran$diffT,
                    by=list("Installation","Plot","Transect","Year_Measurement","Point","Lifeform"),
                 FUN=mean)

library(reshape)
stran1<-melt(stran,id.vars=c("Installation","Plot","Transect","Year_Measurement","Point","Lifeform"),
                       measure.vars="diffT")



cast.test<-cast(stran1,ID~value,mean) 
  
  
  

#Reshapes vegetation transect data


veg.tran<-reshape(stran, direction="wide",idvar=
               c("Installation","Plot","Year_Measurement"),
             timevar="Lifeform",v.names=c("basT","topT"))

#Calculating diff bt top and base heights on transect
veg.tran$tran.diff.F<-veg.tran$topT.F-veg.tran$basT.F

veg.tran$tran.diff.S<-veg.tran$topT.S-veg.tran$basT.S

veg.tran$tran.diff.NA<-veg.tran$topT.NA-veg.tran$basT.NA

veg.tran$tran.diff.null<-veg.tran$topT.NULL-veg.tran$basT.NULL
#Finding average height diff by species group, Installation, Plot and Year Measurement

library(plyr)

veg.tran1<-ddply(veg.tran, c("Installation","Plot","Year_Measurement"),summarize,
      mean.F=mean(tran.diff.F),mean.S=mean(tran.diff.S),mean.NA=mean(tran.diff.NA),
      mean.null=mean(tran.diff.null))

#unsure what "NA" or "NULL" lifeforms translates to
#protocol seems tohave changed in later years of the study in
#favor of not distinguishing between shrubs and forbs




#Merge transect data with annual.gr3
annual.gr3<-merge(annual.gr3,veg.tran1,by=c("Installation","Plot","Year_Measurement"))


#Removes 6th stp plots from analysis
annual.gr3<-annual.gr3[!annual.gr3$STP==6,]


#GAM for 1m polyveg cover
gam.1m.polv<-gam(ht_annual~srHeight_Total+s(Cov.POLV),data=annual.gr3, family=gaussian(link="log"))
summary(gam.1m.polv)


plot(gam.1m.polv,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.1m.polv),residuals(gam.1m.polv),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.1m.polv),main="")


#GAM for 4m polyveg cover
gam.4m.polv<-gam(ht_annual~srHeight_Total+s(Cov4.POLV),data=annual.gr3, family=gaussian(link="log"))
summary(gam.4m.polv)

plot(gam.4m.polv,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.4m.polv),residuals(gam.4m.polv),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.4m.polv),main="")

#QR for 1m polyveg cover
qr.1m.polv<-rq(ht_annual~srHeight_Total+Cov.POLV+fourteen,tau=c(.5),data=annual.gr3)
summary(qr.1m.polv)
aic.list.veg<-AIC(qr.1m.polv)[1]

#QR for 4m polyveg cover
qr.4m.polv<-rq(ht_annual~srHeight_Total+Cov4.POLV+fourteen,tau=c(.5),data=annual.gr3)
summary(qr.4m.polv)
aic.list.veg<-c(aic.list.veg,AIC(qr.4m.polv)[1])

#Four meter veg data appears to have a marginally lower AIC,
#Issue arrises when considering that this variable was only collected for half of the study 
#Perhaps not worth utilizing this variable and in doing so losing half of tree records...





