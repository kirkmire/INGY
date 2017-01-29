#selecting Installations
#EM has 2nd most PIPO tagged trees
#CTRL Plots are 7 and 3

merged_stagm_stag <- merge(stagm, stag,by=c("Installation","Plot","STP","Tree"))

#Refer to meast (measurement timeline for appropriate meas years)
EM<-merged_stagm_stag[merged_stagm_stag$Installation=="EM"&merged_stagm_stag$Species=="PIPO"&merged_stagm_stag$Plot%in%c(1:7)&
                        merged_stagm_stag$Year_Measurement%in%c(2002,2006),]



#Tree 7, Plot 732 goes from 10.2 to 7ft, removed

EM<-EM[!(EM$Plot=="7"&EM$Tree=="732"),]

EM_min<-EM[which(EM$Year_Measurement==min(EM$Year_Measurement)),]

EM_max<-EM[which(EM$Year_Measurement==max(EM$Year_Measurement)),]

EM_both<-merge(EM_min,EM_max, by=c("Installation","Plot","STP","Tree"))

#Merge with splot for treatment column#
EM_both<-merge(EM_both,splot,by=c("Installation","Plot"))

#Create Column with growth increment#
EM_both$inc<-EM_both$Height_Total.y-EM_both$Height_Total.x

boxplot(EM_both$inc)



#Quantreg with default plotting#
library(quantreg)

help(rq)


plot(EM_both$Height_Total.x,EM_both$inc ,cex=.5,pch=19,xlab="Initial Height", ylab="Height Growth Increment",
     col=EM_both$Treatment)
points(EM_both$Height_Total.x,EM_both$inc,cex=.8)
abline(rq(EM_both$inc~EM_both$Height_Total.x, tau=.5), col="blue")
abline(lm(EM_both$inc~EM_both$Height_Total.x),lty=2, col="red")
taus<-c(.05,.1,.25,.75,.9,.95)
for(i in 1:length(taus)){
  abline(rq(EM_both$inc~EM_both$Height_Total.x,tau=taus[i],data=LL_both),col="gray")
}
fit2<-summary(rq(EM_both$inc~EM_both$Height_Total.x,tau=c(.05,.25,.5,.75,.95)))


#Quantile Regression Plot on GGplot#
library(ggplot2)


ggplot(EM_both, aes(x=EM_both$Height_Total.x,y=EM_both$inc))+
  geom_point(size=2,aes(col=EM_both$Treatment))+
  ggtitle("EM Growth Inc vs Initial Height (02-06)")+
  xlab("Initial Height")+
  ylab("Growth Inc")+
  geom_smooth(method=lm,aes(x=EM_both$Height_Total.x,y=EM_both$inc,col=EM_both$Treatment))+
  stat_quantile(quantiles = taus)

#Warning generated from removal of Height-HeightNA#
#Okay#

#For changing quantiles to color scaled
#stat_quantile(aes(colour = ..quantile..), quantiles = taus)


#Coefficient Plots

giEM <- rq(EM_both$inc~EM_both$Height_Total.x, tau= 1:9/10)


## visualizations
plot(giEM)
plot(giEM, parm = 2, mar = c(5.1, 4.1, 2.1, 2.1), main = "", xlab = "tau", 
     ylab = "Initial Height Coefficient", cex = 1, pch = 19)






#To see ALL distinct quantile regression solutions for a particular model
#Specify a tau outside the range [0,1]

z<-rq(EM_both$Height_Total.x~EM_both$inc,tau=-1)


#Formal Testing of estimated quantile regression relationships#

EMfit1<-rq(EM_both$Height_Total.x~EM_both$inc,tau=.25)
EMfit2<-rq(EM_both$Height_Total.x~EM_both$inc,tau=.50)
EMfit3<-rq(EM_both$Height_Total.x~EM_both$inc,tau=.75)

#Quantile Regression Analysis of Deviance Table#
anova(EMfit1, EMfit2, EMfit3)


EM.x.poor <- quantile(EM_both$inc,.1,na.rm=T) #Poor is defined as at the .1 quantile of the sample distn
EM.x.rich <- quantile(EM_both$inc,.9,na.rm=T) #Rich is defined as at the .9 quantile of the sample distn

EMps <- z$sol[1,]
EM.qs.poor <- c(c(1,EM.x.poor)%*%z$sol[4:5,])
EM.qs.rich <- c(c(1,EM.x.rich)%*%z$sol[4:5,])

dev.off()
#now plot the two quantile functions to compare
par(mfrow = c(1,2))
plot(c(EMps,EMps),c(EM.qs.poor,EM.qs.rich), type="n",
     xlab = expression(tau), ylab = "quantile")
plot(stepfun(EMps,c(EM.qs.poor[1],EM.qs.poor)), do.points=FALSE, add=TRUE)
plot(stepfun(EMps,c(EM.qs.poor[1],EM.qs.rich)), do.points=FALSE, add=TRUE,
     col.hor = "gray", col.vert = "gray")

## now plot associated conditional density estimates
## weights from ps (process)
EM.ps.wts <- (c(0,diff(EMps)) + c(diff(EMps),0)) / 2
ap <- akj(EM.qs.poor, z=EM.qs.poor, p = EM.ps.wts)
ar <- akj(EM.qs.rich, z=EM.qs.rich, p = EM.ps.wts)
plot(c(EM.qs.poor,EM.qs.rich),c(ap$dens,ar$dens),type="n",
     xlab= "Incremental Growth", ylab= "Density")
lines(EM.qs.rich, ar$dens, col="gray")
lines(EM.qs.poor, ap$dens, col="black")
#legend("topright", c("poor","rich"), lty = c(1,1), col=c("black","gray"))




