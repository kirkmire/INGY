

######Understory Tree Variables######
#TPA, ---Species---, ---Top height---, basal diameter, diameter at breast height, crown length, crown width
#number per height class per acre


#Aggregates the number of small trees in count plots by 
#Inst, Plot, STP, Year Meas and height class
ht.class<-aggregate(Count~Installation+Plot+STP+Year_Measurement+HeightClass,data=sstpt,sum)
#ht.class<-ht.class[ht.class$Installation %in% sim,]

#Reshapes aggregated dataframe so that each Inst,Plot,STP,YM row 
#contains every associated height class
ht.class2<-reshape(ht.class, direction="wide",idvar=
          c("Installation","Plot","STP","Year_Measurement"),
          timevar="HeightClass",v.names="Count")

#Makes NA tallies =0
count.names<-names(ht.class2[,substring(names(ht.class2),1,6)=="Count."])

for(i in count.names) {
  ht.class2[i][is.na(ht.class2[i])] <- 0
}

#Merges annual growth records with height class tally data
annual.gr<-merge(annual.gr,ht.class2, all.x=T)

#Creates variable for all trees counted regardless of height class
annual.gr$small.tpa<- 138.66*rowSums(annual.gr[,substring(names(annual.gr),1,6)=="Count."],na.rm=T)
  
#Function takes inst, year, plot, stp, tree and height
#deletes all other inst, plots and any height 
#classes that aren't relevant, sums whatevers left to create 
#a tpa-greater-than variable

tpa.gtr.than<-function(inst,year,plot,stp,tree,height){
  tally.df<-annual.gr[annual.gr$Installation %in% inst,]
  tally.df<-tally.df[tally.df$Year_Measurement %in% year,]
  tally.df<-tally.df[tally.df$Plot %in% plot,]
  tally.df<-tally.df[tally.df$STP %in% stp,]
  tally.df<-tally.df[tally.df$Tree %in% tree,]
  counts.df<-tally.df[,substring(names(annual.gr),1,6)=="Count."]
  counts.df<-rbind(counts.df,lower.bound=c(15,2,4,6,8,10,12,14))
  counts.df<-t(counts.df)
  counts.df<-as.data.frame(counts.df)
  counts.df<-subset(counts.df,counts.df$lower.bound>height)
  names(counts.df)[1]<-"tally"
  trees.grtr<-sum(counts.df$tally)
  tpa.grtr<-138.66*trees.grtr
  tpa.grtr
}

#use midpoints of height classes as bounds? this would
#provide a balance between considering tally trees within 
#the subject tree's height class as either all smaller or 
#all larger that the subject tree 

#Also, since -1 ("other") height class represents tally trees larger 
#than 14.9 feet tall, is it reasonable to assume that a subject tree
#greater than 14.9 would have zero trees greater than it?
#alternative would be to move "other" class lower bound to 17,18, etc ft


###Example on a single tree record
tpa.gtr.than("BC",2006,1,3,305,1)

#Assigns a tpa greater than variable to dataframe
annual.gr$tpa.gt<-0

#For loop that runs tpa greater than function
#on all rows of df

for(i in 1:nrow(annual.gr)){
  annual.gr$tpa.gt[i]<-tpa.gtr.than(
    annual.gr$Installation[i], 
    annual.gr$Year_Measurement[i],
    annual.gr$Plot[i],
    annual.gr$STP[i],
    annual.gr$Tree[i],
    annual.gr$Height_Total[i])
}

#Crown Length Variable#
annual.gr$CrownLength<-annual.gr$Height_Total-annual.gr$Height_CrownBase


#Substitute numeric height classes for character 
#headings for ease of modeling
colnames(annual.gr)[substring(colnames(annual.gr),1,6)=="Count."]<-c("other","two","four","six","eight","ten","twelve","fourteen")

annual.gr$srHeight_Total<-sqrt(annual.gr$Height_Total)


###GAM Take1###
library(mgcv)

#Selects installations of similar overstory basal area and SI
#see figure
sim<-c("EM","BC","TJ","RM","CM","TC")

#Selects tree record of annual growth from similar installations
annual.gr2<-annual.gr[annual.gr$Installation %in% sim, ]

#Removes 6th stp plots from analysis
annual.gr2<-annual.gr2[!annual.gr2$STP_rand==6,]

#GAM for Crownwidth ht class
gam.stCW<-gam(ht_annual~s(srHeight_Total)+s(CrownWidth),data=annual.gr2, family=gaussian(link="identity"))
summary(gam.stCW)

par(mfrow=c(2,4),mar=c(4,4,1,2))
plot(gam.stCW,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

#GAM for 2 ht class
gam.st2<-gam(ht_annual~s(srHeight_Total)+s(two),data=annual.gr2, family=gaussian(link="identity"))
summary(gam.st2)

#par(mfrow=c(2,2),mar=c(4,4,1,2))
plot(gam.st2,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


#GAM for 4 ht class
gam.st4<-gam(ht_annual~s(srHeight_Total)+s(four),data=annual.gr2, family=gaussian(link="identity"))
summary(gam.st4)


plot(gam.st4,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


#GAM for 6 ht class
gam.st6<-gam(ht_annual~s(srHeight_Total)+s(six),data=annual.gr2, family=gaussian(link="identity"))
summary(gam.st6)


plot(gam.st6,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
##Print##

#GAM for 8 ht class
gam.st8<-gam(ht_annual~s(srHeight_Total)+s(eight),data=annual.gr2, family=gaussian(link="identity"))
summary(gam.st8)

par(mfrow=c(2,4),mar=c(4,4,1,2))
plot(gam.st8,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

#GAM for 10 ht class
gam.st10<-gam(ht_annual~s(srHeight_Total)+s(ten),data=annual.gr2, family=gaussian(link="identity"))
summary(gam.st10)

plot(gam.st10,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


#GAM for 12 ht class
gam.st12<-gam(ht_annual~s(srHeight_Total)+s(twelve),data=annual.gr2, family=gaussian(link="identity"))
summary(gam.st12)


plot(gam.st12,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


#GAM for 14 ht class
gam.st14<-gam(ht_annual~s(srHeight_Total)+s(fourteen),data=annual.gr2, family=gaussian(link="identity"))
summary(gam.st14)


plot(gam.st14,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
#Print#

#GAM for 15 "other" ht class
gam.st15<-gam(ht_annual~s(srHeight_Total)+s(other),data=annual.gr2, family=gaussian(link="identity"))
summary(gam.st12)

par(mfrow=c(2,4),mar=c(4,4,1,2))
plot(gam.st15,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)



#GAM for small.tpa
gam.sttpa<-gam(ht_annual~s(srHeight_Total)+s(small.tpa),data=annual.gr2, family=gaussian(link="identity"))
summary(gam.sttpa)


plot(gam.sttpa,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


#GAM for tally of trees greater than subject tree
gam.sttgt<-gam(ht_annual~s(srHeight_Total)+s(tpa.gt),data=annual.gr2, family=gaussian(link="identity"))
summary(gam.sttgt)

plot(gam.sttgt,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


#GAM for Basal Diam

gam.stbd<-gam(ht_annual~s(srHeight_Total)+s(BasalDiameter),data=annual.gr2, family=gaussian(link="identity"))
summary(gam.stbd)


plot(gam.stbd,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)
##Print##

#GAM for DBH
gam.stdbh<-gam(ht_annual~s(srHeight_Total)+s(DBH),data=annual.gr2, family=gaussian(link="identity"))
summary(gam.stdbh)

par(mfrow=c(2,4),mar=c(4,4,1,2))
plot(gam.stdbh,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


#GAM for Crown Length
#Need to make variable
annual.gr2$CrownLength<-annual.gr2$Height_Total-annual.gr2$Height_CrownBase

gam.stcl<-gam(ht_annual~s(srHeight_Total)+s(CrownLength),data=annual.gr2, family=gaussian(link="identity"))
summary(gam.stcl)


plot(gam.stcl,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


#GAM for Crown Width
gam.stcw<-gam(ht_annual~s(srHeight_Total)+s(CrownWidth),data=annual.gr2, family=gaussian(link="identity"))
summary(gam.stcw)

plot(gam.stcw,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

#GAM for Crown Ratio
annual.gr2$cratio<- annual.gr2$CrownLength/annual.gr2$Height_Total
gam.stcr<-gam(ht_annual~s(srHeight_Total)+s(cratio),data=annual.gr2, family=gaussian(link="identity"))
summary(gam.stcr)

plot(gam.stcr,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)



#####Quantile Regression
library(quantreg)



#QR for small.tpa
qr.nothing<-rq(ht_annual~srHeight_Total,tau=c(.5),data=annual.gr2)
summary(qr.nothing)
aic.list<-AIC(qr.nothing)[1]
nlist<-length(qr.nothing$y)

#QR for small.tpa
qr.stpa<-rq(ht_annual ~ srHeight_Total+small.tpa,tau=c(.5),data=annual.gr2)
summary(qr.stpa)
aic.list<-c(aic.list,AIC(qr.stpa)[1])

nlist<-c(nlist,length(qr.stpa$y))

#QR for height class 2
qr.stp2<-rq(ht_annual~srHeight_Total+two,tau=c(.5),data=annual.gr2)
summary(qr.stp2)
#aic.list<-c(aic.list,AIC(qr.stp2)[1])

#nlist<-c(nlist,length(qr.stp2$y))


#QR for height class 4
qr.stp4<-rq(ht_annual~srHeight_Total+four,tau=c(.5),data=annual.gr2)
summary(qr.stp4)
#aic.list<-c(aic.list,AIC(qr.stp4)[1])
#nlist<-c(nlist,length(qr.stp4$y))

#QR for height class 6
qr.stp6<-rq(ht_annual~srHeight_Total+six,tau=c(.5),data=annual.gr2)
summary(qr.stp6)
#aic.list<-c(aic.list,AIC(qr.stp6)[1])
#nlist<-c(nlist,length(qr.stp6$y))

#QR for height class 8
qr.stp8<-rq(ht_annual~srHeight_Total+eight,tau=c(.5),data=annual.gr2)
summary(qr.stp8)
#aic.list<-c(aic.list,AIC(qr.stp8)[1])
#nlist<-c(nlist,length(qr.stp8$y))

#QR for height class 10
qr.stp10<-rq(ht_annual~srHeight_Total+ten,tau=c(.5),data=annual.gr2)
summary(qr.stp10)
#aic.list<-c(aic.list,AIC(qr.stp10)[1])
#nlist<-c(nlist,length(qr.stp10$y))

#QR for height class 12
qr.stp12<-rq(ht_annual~srHeight_Total+twelve,tau=c(.5),data=annual.gr2)
summary(qr.stp12)
#aic.list<-c(aic.list,AIC(qr.stp12)[1])
#nlist<-c(nlist,length(qr.stp12$y))

#QR for height class 14
qr.stp14<-rq(ht_annual~srHeight_Total+fourteen,tau=c(.5),data=annual.gr2)
summary(qr.stp14)
#aic.list<-c(aic.list,AIC(qr.stp14)[1])
#nlist<-c(nlist,length(qr.stp14$y))

#QR for height class 15
qr.stp15<-rq(ht_annual~srHeight_Total+other,tau=c(.5),data=annual.gr2)
summary(qr.stp15)
aic.list<-c(aic.list,AIC(qr.stp15)[1])
nlist<-c(nlist,length(qr.stp15$y))


#QR for trees greater than
qr.sttgt<-rq(ht_annual~srHeight_Total+tpa.gt,tau=c(.5),data=annual.gr2)
summary(qr.sttgt)
aic.list<-c(aic.list,AIC(qr.sttgt)[1])
nlist<-c(nlist,length(qr.sttgt$y))

#QR for basal diameter
qr.stbd<-rq(ht_annual~srHeight_Total+BasalDiameter,tau=c(.5),data=annual.gr2)
summary(qr.stbd)
aic.list<-c(aic.list,AIC(qr.stbd)[1])
nlist<-c(nlist,length(qr.stbd$y))

#QR for DBH
qr.stdbh<-rq(ht_annual~srHeight_Total+DBH,tau=c(.5),data=annual.gr2)
summary(qr.stdbh)
aic.list<-c(aic.list,AIC(qr.stdbh)[1])
nlist<-c(nlist,length(qr.stdbh$y))

#QR for Crown Width
qr.stcw<-rq(ht_annual~srHeight_Total+CrownWidth,tau=c(.5),data=annual.gr2)
summary(qr.stcw)
aic.list<-c(aic.list,AIC(qr.stcw)[1])
nlist<-c(nlist,length(qr.stcw$y))


#QR for Crown Length
qr.stcl<-rq(ht_annual~srHeight_Total+CrownLength,tau=c(.5),data=annual.gr2)
summary(qr.stcl)
aic.list<-c(aic.list,AIC(qr.stcl)[1])
nlist<-c(nlist,length(qr.stcl$y))

#QR for Crown Ratio
qr.stcl<-rq(ht_annual~srHeight_Total+cratio,tau=c(.5),data=annual.gr2)
summary(qr.stcl)
aic.list<-c(aic.list,AIC(qr.stcl)[1])
nlist<-c(nlist,length(qr.stcl$y))




UT.aic<-as.data.frame(cbind(nlist,aic.list))

UT.aic$aic.list<-as.numeric(UT.aic$aic.list)

variable<-c("Nothing","SmallTPA","Trees15+",
                      "TGT","BD","DBH","CrownWidth","CrownLength","CrownRatio")

variableUT<-as.data.frame(variable)

UT.aic<-cbind(variableUT,UT.aic)





#Initial DBH has the lowest aic, however, it is not really a measure of other small tree
#competition. The number of trees in the fourteen foot ht. class is and has the next lowest 
#aic

#Maintain both smallTPA and CrownWidth as predictors of small tree growth
#going forward. These variables are related because CW reflects the effects
#of other small tree competition



