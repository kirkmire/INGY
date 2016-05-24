#selecting Installations
#LL has most PIPO tagged trees
#CTRL Plots are 7 and 3

merged_stagm_stag <- merge(stagm, stag,by=c("Installation","Plot","STP","Tree"))

LL<-merged_stagm_stag[merged_stagm_stag$Installation=="LL"&merged_stagm_stag$Species=="PIPO"&merged_stagm_stag$Plot%in%c(1:7)&
                        merged_stagm_stag$Year_Measurement%in%c(2004,2008),]
#Appears to be errors in data entry
#Tree 663 goes from 8.5ft to 1.6ft, should read 9.6ft

LL$Height_Total[LL$Year_Measurement==2008&LL$Plot==1&LL$Tree==663]<-9.6

#Tree 53 goes from 8.6ft to 1.5ft, should read 9.5ft

LL$Height_Total[LL$Plot==3&LL$Tree==53&LL$Year_Measurement==2008]<-9.5

#Tree 19, Plot 3 has Height total of 0 for 2008 measurement, removed#

LL<-LL[!(LL$Plot=="3"&LL$Tree=="19"),]

#Tree 242, Plot 6 has Height total of 0 for 2008 measurement, removed#

LL<-LL[!(LL$Plot=="6"&LL$Tree=="242"),]


LL_min<-LL[which(LL$Year_Measurement==min(LL$Year_Measurement)),]

LL_max<-LL[which(LL$Year_Measurement==max(LL$Year_Measurement)),]

LL_both<-merge(LL_min,LL_max, by=c("Installation","Plot","STP","Tree"))

#Merge with splot for treatment column#
LL_both<-merge(LL_both,splot,by=c("Installation","Plot"))

#Create Column with growth increment#
LL_both$inc<-LL_both$Height_Total.y-LL_both$Height_Total.x

boxplot(LL_both$inc)



#Quantreg with default plotting#
library(quantreg)

help(rq)


plot(LL_both$Height_Total.x,LL_both$inc ,cex=.5,pch=19,xlab="Initial Height", ylab="Height Growth Increment",
     col=LL_both$Treatment)
points(LL_both$Height_Total.x,LL_both$inc,cex=.8)
abline(rq(LL_both$inc~LL_both$Height_Total.x, tau=.5), col="blue")
abline(lm(LL_both$inc~LL_both$Height_Total.x),lty=2, col="red")
taus<-c(.05,.1,.25,.75,.9,.95)
for(i in 1:length(taus)){
  abline(rq(LL_both$inc~LL_both$Height_Total.x,tau=taus[i],data=LL_both),col="gray")
}
fit2<-summary(rq(LL_both$inc~LL_both$Height_Total.x,tau=c(.05,.25,.5,.75,.95)))


#Quantile Regression Plot on GGplot#
library(ggplot2)


ggplot(LL_both, aes(x=LL_both$Height_Total.x,y=LL_both$inc))+
  geom_point(size=2,aes(col=LL_both$Treatment))+
  ggtitle("Growth Inc vs Initial Height")+
  xlab("Initial Height")+
  ylab("Growth Inc")+
  geom_smooth(method=lm,aes(x=LL_both$Height_Total.x,y=LL_both$inc,col=LL_both$Treatment))+
  stat_quantile(quantiles = taus)

#Warning generated from removal of Height-HeightNA#
#Okay#

#For changing quantiles to color scaled
#stat_quantile(aes(colour = ..quantile..), quantiles = taus)





