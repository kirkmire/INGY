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
  ggtitle("LL Growth Inc vs Initial Height")+
  xlab("Initial Height")+
  ylab("Growth Inc")+
  geom_smooth(method=lm,aes(x=LL_both$Height_Total.x,y=LL_both$inc,col=LL_both$Treatment))+
  stat_quantile(quantiles = taus)

#Warning generated from removal of Height-HeightNA#
#Okay#

#For changing quantiles to color scaled
#stat_quantile(aes(colour = ..quantile..), quantiles = taus)



#To see ALL distinct quantile regression solutions for a particular model
#Specify a tau outside the range [0,1]

z<-rq(LL_both$Height_Total.x~LL_both$inc,tau=-1)


#Formal Testing of estimated quantile regression relationships#

fit1<-rq(LL_both$Height_Total.x~LL_both$inc,tau=.25)
fit2<-rq(LL_both$Height_Total.x~LL_both$inc,tau=.50)
fit3<-rq(LL_both$Height_Total.x~LL_both$inc,tau=.75)

#Quantile Regression Analysis of Deviance Table#
anova(fit1, fit2, fit3)


x.poor <- quantile(LL_both$inc,.1,na.rm=T) #Poor is defined as at the .1 quantile of the sample distn
x.rich <- quantile(LL_both$inc,.9,na.rm=T) #Rich is defined as at the .9 quantile of the sample distn

ps <- z$sol[1,]
qs.poor <- c(c(1,x.poor)%*%z$sol[4:5,])
qs.rich <- c(c(1,x.rich)%*%z$sol[4:5,])

dev.off()
#now plot the two quantile functions to compare
par(mfrow = c(1,2))
plot(c(ps,ps),c(qs.poor,qs.rich), type="n",
xlab = expression(tau), ylab = "quantile")
plot(stepfun(ps,c(qs.poor[1],qs.poor)), do.points=FALSE, add=TRUE)
plot(stepfun(ps,c(qs.poor[1],qs.rich)), do.points=FALSE, add=TRUE,
col.hor = "gray", col.vert = "gray")

## now plot associated conditional density estimates
## weights from ps (process)
ps.wts <- (c(0,diff(ps)) + c(diff(ps),0)) / 2
ap <- akj(qs.poor, z=qs.poor, p = ps.wts)
ar <- akj(qs.rich, z=qs.rich, p = ps.wts)
plot(c(qs.poor,qs.rich),c(ap$dens,ar$dens),type="n",
    xlab= "Incremental Growth", ylab= "Density")
lines(qs.rich, ar$dens, col="gray")
lines(qs.poor, ap$dens, col="black")
legend("topright", c("poor","rich"), lty = c(1,1), col=c("black","gray"))

