###GLM Take1###

library(mgcv)

#Exclude the 6th small tree plots of each installation
annual.gr<-annual.gr[! annual.gr$STP==6,]

#Selecting installations of similar overstory basal area and SI
sim<-c("EM","BC","TJ","RM","CM","TC")

annual.gr1<-annual.gr[annual.gr$Installation %in% sim, ]



#Select all control plots 
#annual.gr<-annual.gr[annual.gr$Treatment=="CTRL",]

######Understory Tree Variables######
#TPA, ---Species---, ---Top height---, basal diameter, diameter at breast height, crown length, crown width
#number per height class per acre


#Number per height class/acre
ht.class<-aggregate(Count~Installation+Plot+STP+Year_Measurement+HeightClass,data=sstpt,sum)

ht.class2<-reshape(ht.class, direction="wide",idvar=
          c("Installation","Plot","STP","Year_Measurement"),
          timevar="HeightClass",v.names="Count")

#make NA tally =0
count.names<-names(ht.class2[,substring(names(ht.class2),1,6)=="Count."])

for(i in count.names) {
  ht.class2[i][is.na(ht.class2[i])] <- 0
}

annual.gr2<-merge(annual.gr1,ht.class2, all.x=T)

#Variable for all tree
annual.gr2$small.tpa<- 138.66*rowSums(annual.gr2[,substring(names(annual.gr2),1,6)=="Count."],na.rm=T)
  
#make a function that takes inst, year, plot and deletes all other inst, plots and any height 
#classes that arent relavant, adds up whatevers left

tpa.gtr.than<-function(inst,year,plot,stp,tree,height){
  tally.df<-annual.gr2[annual.gr2$Installation %in% inst,]
  tally.df<-tally.df[tally.df$Year_Measurement %in% year,]
  tally.df<-tally.df[tally.df$Plot %in% plot,]
  tally.df<-tally.df[tally.df$STP %in% stp,]
  tally.df<-tally.df[tally.df$Tree %in% tree,]
  counts.df<-tally.df[,substring(names(annual.gr2),1,6)=="Count."]
  counts.df<-rbind(counts.df,lower.bound=c(15,2,4,6,8,10,12,14))
  counts.df<-t(counts.df)
  counts.df<-as.data.frame(counts.df)
  counts.df<-subset(counts.df,counts.df$lower.bound>height)
  names(counts.df)[1]<-"tally"
  trees.grtr<-sum(counts.df$tally)
  tpa.grtr<-138.66*trees.grtr
  tpa.grtr
}

#use midpoints as bounds?

###Example on a single tree record
tpa.gtr.than("BC",2006,1,3,305,1)

annual.gr2$tpa.gt<-0

for(i in 1:nrow(annual.gr2)){
  annual.gr2$tpa.gt[i]<-tpa.gtr.than(
    annual.gr2$Installation[i], 
    annual.gr2$Year_Measurement[i],
    annual.gr2$Plot[i],
    annual.gr2$STP[i],
    annual.gr2$Tree[i],
    annual.gr2$Height_Total[i])
}


ht.class <- cbind(Installation = rownames(ht.class), ht.class)
rownames(ht.class) <- NULL
ht.class<-ht.class[ht.class$Installation %in% sim,]

#TPA Variable
#Utilize Tree Tally Data
ht.class$small.tpa<-rowSums(ht.class[2:11])


#Substitute numeric height classes for character headings
colnames(annual.gr2)[substring(colnames(annual.gr2),1,6)=="Count."]<-c("other","two","four","six","eight","ten","twelve","fourteen")

annual.gr2$srHeight_Total<-sqrt(annual.gr2$Height_Total)

#GAM for 2 ht class
gam.st2<-gam(ht_annual~srHeight_Total+s(two),data=annual.gr2, family=gaussian(link="log"))
summary(gam.st1)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.st2,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.st2),residuals(gam.st2),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.st2),main="")


#GAM for 4 ht class
gam.st4<-gam(ht_annual~srHeight_Total+s(four),data=annual.gr2, family=gaussian(link="log"))
summary(gam.st4)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.st1,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.st1),residuals(gam.st1),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.st1),main="")

#GAM for 6 ht class
gam.st6<-gam(ht_annual~srHeight_Total+s(six),data=annual.gr2, family=gaussian(link="log"))
summary(gam.st6)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.st6,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.st6),residuals(gam.st6),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.st6),main="")

#GAM for 8 ht class
gam.st8<-gam(ht_annual~srHeight_Total+s(eight),data=annual.gr2, family=gaussian(link="log"))
summary(gam.st8)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.st8,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.st8),residuals(gam.st8),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.st8),main="")

#GAM for 10 ht class
gam.st10<-gam(ht_annual~srHeight_Total+s(ten),data=annual.gr2, family=gaussian(link="log"))
summary(gam.st10)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.st10,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.st10),residuals(gam.st10),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.st10),main="")

#GAM for 12 ht class
gam.st12<-gam(ht_annual~srHeight_Total+s(twelve),data=annual.gr2, family=gaussian(link="log"))
summary(gam.st12)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.st12,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.st12),residuals(gam.st12),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.st12),main="")

#GAM for 14 ht class
gam.st14<-gam(ht_annual~srHeight_Total+s(fourteen),data=annual.gr2, family=gaussian(link="log"))
summary(gam.st14)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.st14,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.st14),residuals(gam.st14),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.st14),main="")

#GAM for 15 "other" ht class
gam.st15<-gam(ht_annual~srHeight_Total+s(other),data=annual.gr2, family=gaussian(link="log"))
summary(gam.st12)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.st15,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.st15),residuals(gam.st12),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.st15),main="")


#GAM for small.tpa
gam.sttpa<-gam(ht_annual~srHeight_Total+s(small.tpa),data=annual.gr2, family=gaussian(link="log"))
summary(gam.sttpa)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.sttpa,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.sttpa),residuals(gam.sttpa),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.sttpa),main="")

#GAM for tally of trees greater than subject tree
gam.sttgt<-gam(ht_annual~srHeight_Total+s(tpa.gt),data=annual.gr2, family=gaussian(link="log"))
summary(gam.sttgt)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.sttgt,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.sttgt),residuals(gam.sttgt),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.sttgt),main="")


#GAM for Basal Diam

gam.stbd<-gam(ht_annual~srHeight_Total+s(BasalDiameter),data=annual.gr2, family=gaussian(link="log"))
summary(gam.stbd)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.stbd,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.stbd),residuals(gam.stbd),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.stbd),main="")
    
#GAM for DBH
gam.stdbh<-gam(ht_annual~srHeight_Total+s(DBH),data=annual.gr2, family=gaussian(link="log"))
summary(gam.stdbh)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.stdbh,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.stdbh),residuals(gam.stdbh),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.stdbh),main="")


#GAM for Crown Length
#Need to make variable
annual.gr2$CrownLength<-annual.gr2$Height_Total-annual.gr2$Height_CrownBase

gam.stcl<-gam(ht_annual~srHeight_Total+s(CrownLength),data=annual.gr2, family=gaussian(link="log"))
summary(gam.stcl)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.stcl,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.stcl),residuals(gam.stcl),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.stcl),main="")


#GAM for Crown Width
gam.stcw<-gam(ht_annual~srHeight_Total+s(CrownWidth),data=annual.gr2, family=gaussian(link="log"))
summary(gam.stcw)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.stcl,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

par(mfrow=c(1,2))
plot(predict(gam.stcw),residuals(gam.stcw),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.stcw),main="")


#####Quantile Regression
library(quantreg)

#QR for small.tpa
qr.stpa<-rq(ht_annual ~ srHeight_Total+small.tpa,tau=c(.5),data=annual.gr2)
summary(qr.stpa)
aic.list<-AIC(qr.stpa)[1]

#QR for height class 2
qr.stp2<-rq(ht_annual~srHeight_Total+two,tau=c(.5),data=annual.gr2)
summary(qr.stp2)
aic.list<-c(aic.list,AIC(qr.stp2)[1])

#QR for height class 4
qr.stp4<-rq(ht_annual~srHeight_Total+four,tau=c(.5),data=annual.gr2)
summary(qr.stp4)
aic.list<-c(aic.list,AIC(qr.stp4)[1])

#QR for height class 6
qr.stp6<-rq(ht_annual~srHeight_Total+six,tau=c(.5),data=annual.gr2)
summary(qr.stp6)
aic.list<-c(aic.list,AIC(qr.stp6)[1])

#QR for height class 8
qr.stp8<-rq(ht_annual~srHeight_Total+eight,tau=c(.5),data=annual.gr2)
summary(qr.stp8)
aic.list<-c(aic.list,AIC(qr.stp8)[1])

#QR for height class 10
qr.stp10<-rq(ht_annual~srHeight_Total+ten,tau=c(.5),data=annual.gr2)
summary(qr.stp10)
aic.list<-c(aic.list,AIC(qr.stp10)[1])

#QR for height class 12
qr.stp12<-rq(ht_annual~srHeight_Total+twelve,tau=c(.5),data=annual.gr2)
summary(qr.stp12)
aic.list<-c(aic.list,AIC(qr.stp12)[1])

#QR for height class 14
qr.stp14<-rq(ht_annual~srHeight_Total+fourteen,tau=c(.5),data=annual.gr2)
summary(qr.stp14)
aic.list<-c(aic.list,AIC(qr.stp14)[1])

#QR for height class 15
qr.stp15<-rq(ht_annual~srHeight_Total+other,tau=c(.5),data=annual.gr2)
summary(qr.stp15)
aic.list<-c(aic.list,AIC(qr.stp15)[1])

#QR for trees greater than
qr.sttgt<-rq(ht_annual~srHeight_Total+tpa.gt,tau=c(.5),data=annual.gr2)
summary(qr.sttgt)
aic.list<-c(aic.list,AIC(qr.sttgt)[1])


#QR for basal diameter
qr.stbd<-rq(ht_annual~srHeight_Total+BasalDiameter,tau=c(.5),data=annual.gr2)
summary(qr.stbd)
aic.list<-c(aic.list,AIC(qr.stbd)[1])


#QR for DBH
qr.stdbh<-rq(ht_annual~srHeight_Total+DBH,tau=c(.5),data=annual.gr2)
summary(qr.stdbh)
aic.list<-c(aic.list,AIC(qr.stdbh)[1])

#QR for Crown Width
qr.stcw<-rq(ht_annual~srHeight_Total+CrownWidth,tau=c(.5),data=annual.gr2)
summary(qr.stcw)
aic.list<-c(aic.list,AIC(qr.stcw)[1])


#QR for Crown Length
qr.stcl<-rq(ht_annual~srHeight_Total+CrownLength,tau=c(.5),data=annual.gr2)
summary(qr.stcl)
aic.list<-c(aic.list,AIC(qr.stcl)[1])

aic.list<-t(as.data.frame(aic.list))
variable<-c("SmallTPA","Two","Four","Six","Eight",
                      "Ten","Twelve","Fourteen","Fifteen",
                      "TGT","BD","DBH","CrownWidth","CrownLength")

colnames(aic.list)<-(variable)

aic.list[,aic.list==min(aic.list)]






    