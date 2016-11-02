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
ht.class <- cbind(Installation = rownames(ht.class), ht.class)
rownames(ht.class) <- NULL
ht.class<-ht.class[ht.class$Installation %in% sim,]
ht.class#/acres in 5x6=30 small tree plots

#TPA Variable
#Utilize Tree Tally Data
sum(ht.class, by="Installation")
ht.class$small.tpa<-rowSums(ht.class[2:11])#/acres in 5x6 small tree plots

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
    
#GAM for Crown Width
gam.stcw<-gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+
                  smooth(annual.gr$CrownWidth),family=gaussian(link="log"))       


#####Quantile Regression
library(quantreg)

#QR for small.tpa
qr.stpa<-rq(annual.gr$ht_annual ~ sqrt(annual.gr$Height_Total)+annual.gr$small.tpa,tau=c(.5))
summary(qr.stpa)




    