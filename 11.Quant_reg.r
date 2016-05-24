#selecting Installations
#LL has most PIPO tagged trees
#CTRL Plots are 7 and 3

merged_stagm_stag <- merge(stagm, stag,by=c("Installation","Plot","STP","Tree"))

LL<-merged_stagm_stag[merged_stagm_stag$Installation=="LL"&merged_stagm_stag$Species=="PIPO"&merged_stagm_stag$Plot%in%c(1:7)&
                        merged_stagm_stag$Year_Measurement%in%c(2004,2008),]

LL_min<-LL[which(LL$Year_Measurement==min(LL$Year_Measurement)),]

LL_max<-LL[which(LL$Year_Measurement==max(LL$Year_Measurement)),]

LL_both<-merge(LL_min,LL_max, by=c("Installation","Plot","STP","Tree"))

#Merge with splot for treatment column#
LL_both<-merge(LL_both,splot,by=c("Installation","Plot"))

#Create Column with growth increment#
LL_both$inc<-LL_both$Height_Total.y-LL_both$Height_Total.x

boxplot(LL_both$inc)

#Appears to be errors in data entry
#Tree 663 goes from 8.5ft to 1.6ft, should read 9.6ft

LL$Height_Total[LL$Year_Measurement==2008&LL$Tree==663]<-9.6





#Quantreg with default plotting#
library(quantreg)

help(rq)

LL2$Treatment<-as.factor(LL2$Treatment)

plot(LL_both$,LL2$..1,cex=.5,pch=19,xlab="Initial Height", ylab="Height Growth",col=LL2$Treatment)
points(LL2$LL_initial,LL2$..1,cex=.5)
abline(rq(LL2$..1~LL2$LL_initial, tau=.5), col="blue")
abline(lm(LL2$..1~LL2$LL_initial),lty=2, col="red")
taus<-c(.05,.1,.25,.75,.9,.95)
for(i in 1:length(taus)){
  abline(rq(LL2$..1~LL2$LL_initial,tau=taus[i],data=LL_trees),col="gray")
}
fit2<-summary(rq(LL2$..1~LL2$LL_initial,tau=c(.05,.25,.5,.75,.95)))


#Quantile Regression Plot on GGplot#
library(ggplot2)


ggplot(LL2, aes(x=LL2$LL_initial,y=LL2$..1))+
  geom_point(size=2,aes(col=LL2$Treatment))+
  ggtitle("Growth Inc vs Initial Height")+
  xlab("Initial Height")+
  ylab("Growth Inc")+
  geom_smooth(method=lm,aes(x=LL2$LL_initial,y=LL2$..1,col=LL2$Treatment))+
  stat_quantile(quantiles = taus)

#Warning generated from removal of Height-HeightNA#
#Okay#

stat_quantile(aes(colour = ..quantile..), quantiles = taus)





