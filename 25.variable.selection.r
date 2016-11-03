###GLM Take1###

library(gam)

#Selecting installations of similar overstory basal area and SI
sim<-c("EM","BC","TJ","RM","CM","TC")

annual.gr<-annual.gr[annual.gr$Installation %in% sim, ]

#Exclude the 6th plots of each installation
annual.gr<-annual.gr[! annual.gr$Plot==6,]

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

#GAM for -1 ht class
gam.st1<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
                smooth(annual.gr$`-1`),family=gaussian(link="log"))

#GAM for 2 ht class
gam.st2<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$`2`),family=gaussian(link="log"))

#GAM for 4 ht class
gam.st4<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
               smooth(annual.gr$`4`),family=gaussian(link="log"))

#Error in eval(expr, envir, enclos) : 
 # cannot find valid starting values: please specify some


#TPA Variable
#Utilize Tree Tally Data
ht.class$small.tpa<-rowSums(ht.class[2:11])

#Merge with annual.growth
annual.gr<-merge(ht.class,annual.gr,by="Installation")


#GAM for small.tpa
gam.stpa<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
                smooth(annual.gr$small.tpa),family=gaussian(link="log"))
    
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



















    