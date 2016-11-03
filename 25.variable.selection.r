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
gam.st1<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
                smooth(annual.gr$one),family=gaussian(link="log"))

#GAM for 2 ht class
gam.st2<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$two),family=gaussian(link="log"))

#GAM for 4 ht class
gam.st4<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$four),family=gaussian(link="log"))

#GAM for 6 ht class
gam.st4<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$six),family=gaussian(link="log"))

#GAM for 8 ht class
gam.st4<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$eight),family=gaussian(link="log"))

#GAM for 10 ht class
gam.st4<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$ten),family=gaussian(link="log"))

#GAM for 12 ht class
gam.st4<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$twelve),family=gaussian(link="log"))

#GAM for 14 ht class
gam.st4<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$fourteen),family=gaussian(link="log"))

#GAM for 16 ht class
gam.st4<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$sixteen),family=gaussian(link="log"))

#GAM for small.tpa
gam.stpa<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
                smooth(annual.gr$small.tpa),family=gaussian(link="log"))
    
vis.gam(gam.stpa,data=annual.gr)

#need to figure out how to visualize GAM 

#GAM for Basal Diam
gam.stbd<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
                  smooth(annual.gr$BasalDiameter),family=gaussian(link="log"))    
    
#GAM for DBH
gam.stdbh<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
                  smooth(annual.gr$DBH),family=gaussian(link="log"))       

#GAM for Crown Length
#Need to make variable
annual.gr$Crown_Length<-annual.gr$Height_Total-annual.gr$Height_CrownBase

gam.stcl<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
                  smooth(annual.gr$Crown_Length),family=gaussian(link="log"))     
library(ggplot2)
c<-ggplot(data =annual.gr,
          formula = ht_annual~sqrt(Height_Total))
c


thegam<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total),family=gaussian(link="log"))    

preds<-predict(thegam, type="terms", newdata=temperature.seq,
               se.fit=TRUE)

# set up the temperature, the fit and the upper and lower
# confidence interval

temperature<-temperature.seq$temp
fit<-preds$fit
fit.up95<-fit-1.96*preds$se.fit
fit.low95<-fit+1.96*preds$se.fit

# plot the temperature smooth but leave blank for
# now so that we can add the line on top of the polygon
plot(temperature, fit, type="n", lwd=3, xlim=c(-3,90), ylim=c(-20,30),
     main="Ahhh, definitely better",
     ylab=paste("s(temp,", round(sum(thegam$edf[-1]),2), ")", sep=""))

# If you want confidence lines instead of a grey poly you can
# use this code
#lines(temperature, fit.up95, lty="dotted")
#lines(temperature, fit.low95, lty="dotted")

# For the confidence grey polygon
polygon(c(temperature, rev(temperature)), 
        c(fit.low95,rev(fit.up95)), col="grey",
        border=NA)

lines(temperature, fit,  lwd=2)

















    
#GAM for Crown Width
gam.stcw<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
                  smooth(annual.gr$CrownWidth),family=gaussian(link="log"))       


#####Quantile Regression
library(quantreg)

#QR for small.tpa
qr.stpa<-rq(annual.gr$ht_annual ~ sqrt(annual.gr$Height_Total)+annual.gr$small.tpa,tau=c(.5))
summary(qr.stpa)
AIC(qr.stpa)

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



















    