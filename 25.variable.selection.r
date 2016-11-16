###GLM Take1###

library(mgcv)

#Selecting installations of similar overstory basal area and SI
sim<-c("EM","BC","TJ","RM","CM","TC")

annual.gr<-annual.gr[annual.gr$Installation %in% sim, ]

#Exclude the 6th small tree plots of each installation
annual.gr<-annual.gr[! annual.gr$STP==6,]

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

annual.gr2<-merge(annual.gr,ht.class2, all.x=T)

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
  counts.df<-rbind(counts.df,lower.bound=c(15,1,3,5,7,9,11,13,15,17))
  counts.df<-t(counts.df)
  counts.df<-as.data.frame(counts.df)
  counts.df<-subset(counts.df,counts.df$lower.bound>height)
  names(counts.df)[1]<-"tally"
  trees.grtr<-sum(counts.df$tally)
  tpa.grtr<-138.66*trees.grtr
  tpa.grtr
}

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
names(annual.gr)[2:11]<-c("one","two","four","six","eight","ten","twelve","fourteen","sixteen","eighteen")


#GAM for -1 ht class
annual.gr$srHeight_Total<-sqrt(annual.gr$Height_Total)



gam.st1<-gam(ht_annual~srHeight_Total+s(one),data=annual.gr)
summary(gam.st1)



par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.st1,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


par(mfrow=c(1,2))
plot(predict(gam.st1),residuals(gam.st1),xlab="predicted",ylab="residuals")
qqnorm(residuals(gam.st1),main="")


plot(annual.gr$ht_annual, fitted(gam.st1))
#GAM for 2 ht class
gam.st2<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$two),family=gaussian(link="log"))

plot(annual.gr$ht_annual, fitted(gam.st2))

#GAM for 4 ht class
gam.st4<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$four),family=gaussian(link="log"))

plot(annual.gr$ht_annual, fitted(gam.st4))

#GAM for 6 ht class
gam.st6<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$six),family=gaussian(link="log"))

library(car)
crPlot(annual.gr$ht_annual, fitted(gam.st6))
crPlot(gam.st6,variable=sqrt(annual.gr$Height_Total))

#GAM for 8 ht class
gam.st8<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$eight),family=gaussian(link="log"))


plot(annual.gr$ht_annual, fitted(gam.st8))

#GAM for 10 ht class
gam.st10<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$ten),family=gaussian(link="log"))

plot(annual.gr$ht_annual, fitted(gam.st10))

#GAM for 12 ht class
gam.st12<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$twelve),family=gaussian(link="log"))

plot(annual.gr$ht_annual, fitted(gam.st12))

#GAM for 14 ht class
gam.st14<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$fourteen),family=gaussian(link="log"))


identify(annual.gr$ht_annual, fitted(gam.st12))

#GAM for 16 ht class
gam.st16<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$sixteen),family=gaussian(link="log"))

plot(annual.gr$ht_annual, fitted(gam.st16))

#GAM for 18 ht class
gam.st18<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$eighteen),family=gaussian(link="log"))

plot(annual.gr$ht_annual, fitted(gam.st18))

#GAM for small.tpa
gam.stpa<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
                smooth(annual.gr$small.tpa),family=gaussian(link="log"))
    
plot(annual.gr$ht_annual, fitted(gam.stpa))

#need to figure out how to visualize GAM 

#GAM for Basal Diam
annual.grbd<-annual.gr[!annual.gr$BasalDiameter=="NA",]

gam.stbd<-gam(annual.grbd$ht_annual~sqrt(annual.grbd$Height_Total)+
                  smooth(annual.grbd$BasalDiameter),family=gaussian(link="log"))    

plot(annual.grbd$ht_annual, fitted(gam.stbd))
    
#GAM for DBH
annual.grdbh<-annual.gr[!annual.gr$DBH=="NA",]

gam.stdbh<-gam(annual.grdbh$ht_annual~sqrt(annual.grdbh$Height_Total)+
                  smooth(annual.grdbh$DBH),family=gaussian(link="log"))       

plot(annual.grdbh$ht_annual, fitted(gam.stdbh))


#GAM for Crown Length
#Need to make variable
annual.gr$Crown_Length<-annual.gr$Height_Total-annual.gr$Height_CrownBase

gam.stcl<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
                  smooth(annual.gr$Crown_Length),family=gaussian(link="log"))     


    
#GAM for Crown Width
gam.stcw<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
                  smooth(annual.gr$CrownWidth),family=gaussian(link="log"))       


#####Quantile Regression
library(quantreg)

#QR for small.tpa
qr.stpa<-rq(annual.gr$ht_annual ~ sqrt(annual.gr$Height_Total)+annual.gr$small.tpa,tau=c(.5))
summary(qr.stpa)
AIC(qr.stpa)

#Need to do QR for height classes


#QR for basal.diameter
qr.stbd<-rq(annual.gr$ht_annual ~ sqrt(annual.gr$Height_Total)+annual.gr$BasalDiameter,tau=c(.5))
summary(qr.stbd)
AIC(qr.stbd)

#QR for dbh
qr.stdbh<-rq(annual.gr$ht_annual ~ sqrt(annual.gr$Height_Total)+annual.gr$DBH,tau=c(.5))
summary(qr.stdbh)
AIC(qr.stdbh)

#QR for crown.length
qr.stcl<-rq(annual.gr$ht_annual ~ sqrt(annual.gr$Height_Total)+annual.gr$Crown_Length,tau=c(.5))
summary(qr.stcl)
AIC(qr.stcl)

#QR for crown.width
qr.stcw<-rq(annual.gr$ht_annual ~ sqrt(annual.gr$Height_Total)+annual.gr$CrownWidth,tau=c(.5))
summary(qr.stcw)
AIC(qr.stcw)



















    