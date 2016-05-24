#selecting Installations
#LL has most PIPO tagged trees
#CTRL Plots are 7 and 3

merged_stagm_stag <- merge(stagm, stag,by=c("Installation","Plot","STP","Tree"))

LL<-merged_stagm_stag[merged_stagm_stag$Installation=="LL"&merged_stagm_stag$Species=="PIPO"&merged_stagm_stag$Plot%in%c(1:7)&
                        merged_stagm_stag$Year_Measurement%in%c(2004,2008),]


                   

library(plyr)

test_df<-ddply(LL, .(Installation, Plot, STP, Tree), summarize, abs(diff(Height_Total)))

LL2<-cbind(test_df,LL_initial)

LL2<-merge(LL2,splot, by=c("Installation","Plot"))

dev.off()


#LL_initial<-LL$Height_Total[LL$Year_Measurement==2004]
#LL_growth<-LL$Height_Total[LL$Year_Measurement==2008]


library(quantreg)

help(rq)

LL2$Treatment<-as.factor(LL2$Treatment)

plot(LL2$LL_initial,LL2$..1,cex=.5,pch=19,xlab="Initial Height", ylab="Height Growth",col=LL2$Treatment)
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





