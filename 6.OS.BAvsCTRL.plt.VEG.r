#Select Control and GE Plots from list of inst and plots#
CTRLandGE.plots <- splot[!(splot$Treatment=='1X'), ]

#Merge Control&GE plots with 4m^2 plot data#
CTRLandGE.plot.veg <- merge(CTRLandGE.plots,sstp1,by=c("Installation","Plot"))

#Aggregate the control and GE plots 4m^2 data by Installation and find the mean of top height and polyveg% cover#
CTRLandGE.volume.index<-aggregate(CTRLandGE.plot.veg[, 17:19], list(Installation=CTRLandGE.plot.veg$Installation,
                                                                    Trt=CTRLandGE.plot.veg$Treatment
                                                                 ), mean, na.rm=TRUE)
#Create a US Volume Estimate by multiplying mean ployveg% cover * mean top height for each treatment w/i each inst#
CTRLandGE.volume.index$volume<-((CTRLandGE.volume.index$Coverage/100)*4)*((CTRLandGE.volume.index$Top-CTRLandGE.volume.index$Base)/.3048)


#Merge volume df with Installation Info (contains estimate of SI)#
OS.ctrl.plot<- merge(CTRLandGE.volume.index,sinst, by.x = "Installation", by.y = "Installation",all=F)

#Below is from 4.explor.plots (describes the OS in terms of BAPA post harvest)#
library(plyr)
BAPA.by.inst<-ddply(OverS, "Installation", summarise, BAPA.inst= sum(BAPA))


#Merges OS data to form df with OS BAPA, SI, and US Volume#
threeDVeg<-merge(BAPA.by.inst,OS.ctrl.plot, by.x = "Installation", by.y = "Installation",all=F)


#Remove Installations#
threeDVeg<-threeDVeg[!(threeDVeg$Installation %in% drp),]

#Creates a 3-D Scatterplot (x=BAPA,y=SI, z=Volume#

library(scatterplot3d)
s3D<-scatterplot3d(threeDVeg$BAPA.inst,threeDVeg$SiteIndex_Value,threeDVeg$volume,
type="h", lty.hplot=2,       # lines to the horizontal plane
scale.y=.75,                 # scale y axis (reduce by 25%)
main="3-D Scatterplot",
xlab="Residual BAPA (ft^3/Ac)",
ylab="Site Index Value",
zlab="Volume Index",
pch=as.integer(threeDVeg$Trt)
)

#Extracts coordinates from 3D plot for installation labels#
s3D.coords <- s3D$xyz.convert(threeDVeg$BAPA.inst,threeDVeg$SiteIndex_Value,threeDVeg$volume)
text(s3D.coords$x, s3D.coords$y,     # x and y coordinates
     labels=(threeDVeg$Installation),       # text to plot
     pos=4, cex=.5)    



library(ggplot2)

ba.final<-merge(locdataX,ba.final,by="Installation")

plotcolor<-brewer.pal(3,"RdYlBu")










