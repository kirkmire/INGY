###GLM Take1###

library(mgcv)

#Selecting installations of similar overstory basal area and SI
sim<-c("EM","BC","TJ","RM","CM","TC")

annual.gr<-annual.gr[annual.gr$Installation %in% sim, ]

#Exclude the 6th small tree plots of each installation
annual.gr<-annual.gr[! annual.gr$STP==6,]

#Select all control plots 
annual.gr<-annual.gr[annual.gr$Treatment=="CTRL",]

######Understory Tree Variables######
#TPA, ---Species---, ---Top height---, basal diameter, diameter at breast height, crown length, crown width
#number per height class per acre


#Number per height class/acre
#Remove 6th plot tally data
sstpt<-sstpt[!sstpt$Plot==6,]
ht.class<-as.data.frame.matrix(xtabs(sstpt$Count~sstpt$Installation+sstpt$HeightClass, data=sstpt))
ht.class<-ht.class/.2164#acres in 5x6=30 small tree plots
ht.class <- cbind(Installation = rownames(ht.class), ht.class)
rownames(ht.class) <- NULL
ht.class<-ht.class[ht.class$Installation %in% sim,]

#TPA Variable
#Utilize Tree Tally Data
ht.class$small.tpa<-rowSums(ht.class[2:11])

#Merge with annual.growth
annual.gr<-merge(ht.class,annual.gr,by="Installation")

#Substitute numeric height classes for character headings
names(annual.gr)[2:11]<-c("one","two","four","six","eight","ten","twelve","fourteen","sixteen","eighteen")


#GAM for -1 ht class
gam.st1<-gam(ht_annual~sqrt(Height_Total)+smooth(one),data=annual.gr,family="gaussian")
summary(gam.st1)



par(mfrow=c(2,2),mar=c(4,4,1,2))

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



















    