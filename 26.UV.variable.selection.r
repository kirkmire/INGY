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



#Removes 6th stp plots from analysis
annual.gr3<-annual.gr3[!annual.gr3$STP==6,]


#GAM for 
gam.stdbh<-gam(ht_annual~srHeight_Total+s(DBH),data=annual.gr2, family=gaussian(link="log"))
summary(gam.stdbh)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.stdbh,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.stdbh),residuals(gam.stdbh),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.stdbh),main="")











