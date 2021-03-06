selecting Installations
LL has most PIPO tagged trees
CTRL Plots are 7 and 3


source('C:/Users/Colin/Desktop/R-Projects/INGY/1.readdatabase.2016jun2.r', echo=F)

merged_stagm_stag <- merge(stagm, stag,by=c("Installation","Plot","STP","Tree"))

LL<-merged_stagm_stag[merged_stagm_stag$Installation=="LL"&merged_stagm_stag$Species=="PIPO"&merged_stagm_stag$Plot%in%c(1:7)&merged_stagm_stag$Year_Measurement%in%c(2004,2008),]


LL_min<-LL[which(LL$Year_Measurement==min(LL$Year_Measurement)),]
LL_max<-LL[which(LL$Year_Measurement==max(LL$Year_Measurement)),]
LL_both<-merge(LL_min,LL_max, by=c("Installation","Plot","STP","Tree"))

#Merge with splot for treatment column#
LL_both<-merge(LL_both,splot,by=c("Installation","Plot"))

#Create Column with growth increment#
LL_both$inc<-LL_both$Height_Total.y-LL_both$Height_Total.x

#Create column with associated veg coverage#
LL_both$veg<- ifelse(LL_both$Treatment=="GE",LL_veg_2004$Coverage,NA)


LL_both$veg<-with(LL_both, ifelse(LL_both$Treatment=="GE",LL_veg_2004$Coverage[LL_veg_2004$Treatment=="GE"],
                                  ifelse(LL_both$Treatment=="1X",LL_veg_2004$Coverage[LL_veg_2004$Treatment=="1X"],
                                  LL_veg_2004$Coverage[LL_veg_2004$Treatment=="CTRL"] )))

#Make an interaction column#
LL_both$int<-LL_both$veg+LL_both$Height_Total.x


boxplot(LL_both$inc)

#Merging with veg info

new.veg<-LL_plots_veg[LL_plots_veg$Lifeform=="POLV",]

LL_both_plus<-merge(LL_both,new.veg, by.x=c("Plot","STP","Year_Measurement.x"),by.y=c("Plot","STP","Year_Measurement"),all.x=T)



```


Quantreg with default plotting

library(quantreg)

plot(LL_both$Height_Total.x,LL_both$inc ,cex=.5,pch=19,xlab="Initial Height", ylab="Height Growth Increment",
     col=LL_both$Treatment)
points(LL_both$Height_Total.x,LL_both$inc,cex=.8)
abline(rq(LL_both$inc~LL_both$Height_Total.x, tau=.5), col="blue")
abline(lm(LL_both$inc~LL_both$Height_Total.x),lty=2, col="red")
taus<-c(.05,.1,.25,.50,.75,.9,.95)
for(i in 1:length(taus)){
  abline(rq(LL_both$inc~LL_both$Height_Total.x,tau=taus[i],data=LL_both),col="gray")
}
fit2<-summary(rq(LL_both$inc~LL_both$Height_Total.x,tau=c(.05,.25,.5,.75,.95)))
```

Quantile Regression Plot on GGplot


library(ggplot2)

taus<-c(.05,.1,.25,.50,.75,.9,.95)

ggplot(LL_both, aes(x=LL_both$Height_Total.x,y=LL_both$inc))+
  geom_point(size=2,aes(col=LL_both$Treatment))+
  ggtitle("Loon Lake Installation Growth Increment vs Initial Height (2004-2008)")+
  xlab("Initial Height (ft)")+
  ylab("Growth Increment (ft)")+
  #geom_smooth(method=lm,aes(x=LL_both$Height_Total.x,y=LL_both$inc))+
  stat_quantile(quantiles = taus)

#Warning generated from removal of Height-HeightNA#
#Okay#

#For changing quantiles to color scaled
#stat_quantile(aes(colour = ..quantile..), quantiles = taus)



#add veg#


ggplot(LL_both, aes(x=LL_both$int,y=LL_both$inc))+
  geom_point(size=2,aes(col=LL_both$Treatment))+
  ggtitle("Loon Lake Installation Growth Increment vs Initial Height (2004-2008)")+
  xlab("Initial Height (ft)")+
  ylab("Growth Increment (ft)")+
  #geom_smooth(method=lm,aes(x=LL_both$Height_Total.x,y=LL_both$inc))+
  stat_quantile(quantiles = taus)


#Coefficient Plots

gi <- rq(LL_both$inc~LL_both$Height_Total.x, tau= 1:9/10)


## visualizations
plot(gi)
plot(gi, parm = 2, mar = c(5.1, 4.1, 2.1, 2.1), main = "", xlab = "tau", 
     ylab = "Initial Height Coefficient", cex = 1, pch = 19)



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
legend("topright", c(".10 Quant",".90 Quant"), lty = c(1,1), col=c("black","gray"))





