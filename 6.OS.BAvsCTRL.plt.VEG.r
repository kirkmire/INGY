#Select Control and GE Plots from list of inst and plots#
CTRLandGE.plots <- splot[!(splot$Treatment=='1X'), ]

#Merge Control&GE plots with 4m^2 plot data#
CTRLandGE.plot.veg <- merge(CTRLandGE.plots,sstp1,by=c("Installation","Plot"))

#Aggregate the control and GE plots 4m^2 data by Installation and find the mean of top height and polyveg% cover#
CTRLandGE.volume.index<-aggregate(CTRLandGE.plot.veg[, 17:19], list(Installation=CTRLandGE.plot.veg$Installation,
                                                                    Trt=CTRLandGE.plot.veg$Treatment
                                                                 ), mean, na.rm=TRUE)

#Create a US Volume Estimate by multiplying mean ployveg% cover * mean top height for each treatment w/i each inst#
CTRLandGE.volume.index$volume<-(((CTRLandGE.volume.index$Coverage/100)*43.0556)*
                                  ((CTRLandGE.volume.index$Top-CTRLandGE.volume.index$Base)))/43.0556
#the above is actually calculating veg volume per square foot#

#Merge volume df with Installation Info (contains estimate of SI)#
OS.ctrl.plot<- merge(CTRLandGE.volume.index,sinst, by.x = "Installation", by.y = "Installation",all=F)

#Below is from 4.explor.plots (describes the OS in terms of BAPA post harvest)#
library(plyr)

BAPA.by.inst<-ddply(OverS, "Installation", summarise, BAPA.inst= sum(BAPA))

BAPA.by.inst$ave.BAPA<-BAPA.by.inst$BAPA.inst/7


bapa.by.inst<-BAPA.by.inst[!(BAPA.by.inst$Installation %in% drp),]

#Merges OS data to form df with OS BAPA, SI, and US Volume#
threeDVeg<-merge(BAPA.by.inst,OS.ctrl.plot, by.x = "Installation", by.y = "Installation",all=F)

#Remove Installations#
threeDVeg<-threeDVeg[!(threeDVeg$Installation %in% drp),]



library(ggplot2)

ba.final<-merge(locdataX,ba.final,by="Installation")

plotcolor<-brewer.pal(3,"RdYlBu")


#Just SI vs BA#
ggplot(threeDVeg, aes(x =ave.BAPA , y = SiteIndex_Value,color=threeDVeg$SiteIndex_Species))+
  geom_point(size=2)+
  ggtitle("Site Index vs Residual Basal Area")+
  xlab("Residual Basal Area (ft^2/acre)")+
  ylab("Site Index")+
  labs(color="SI Species")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))







