#Select Control Plots from list of inst and plots#
CTRL.plots <- splot[ which(splot$Treatment=='CTRL'), ]

#Merge Control plots with 4m^2 plot data#
CTRL.plot.veg <- merge(CTRL.plots,sstp4,by=c("Installation","Plot"))


#Aggregate the control plots 4m^2 data by Installation and find the mean of top height and polyveg% cover#
volume.index<-aggregate(CTRL.plot.veg[, 17:19], list(CTRL.plot.veg$Installation), mean, na.rm=TRUE)

#Create additional column that contains avg plyveg% cover*avg top height for each inst#
volume.index$volume<-volume.index$Cover*volume.index$Top
names(volume.index)[1]<-paste("Installation")

#Merge volume df with Installation Info (contains estimate of SI)#
OS.ctrl.plot<- merge(volume.index,sinst, by.x = "Installation", by.y = "Installation",all=F)

#Below is from 4.explor.plots (describes the OS in terms of BAPA post harvest)#
library(plyr)
BAPA.by.inst<-ddply(OverS, "Installation", summarise, BAPA.inst= sum(BAPA))


#Merges OS data to form df with OS BAPA, SI, and US Volume#
threeDVeg<-merge(BAPA.by.inst,OS.ctrl.plot, by.x = "Installation", by.y = "Installation",all=F)


#Creates a 3-D Scatterplot (x=BAPA,y=SI, z=Volume#

library(scatterplot3d)
s3D<-scatterplot3d(threeDVeg$BAPA.inst,threeDVeg$SiteIndex_Value,threeDVeg$volume,

              pch=19,        
type="h", lty.hplot=2,       # lines to the horizontal plane
scale.y=.75,                 # scale y axis (reduce by 25%)
main="3-D Scatterplot",
xlab="BAPA (ft^3/Ac)",
ylab="Site Index Value",
zlab="Volume Index"
)

#Extracts coordinates from 3D plot for installation labels#
s3D.coords <- s3D$xyz.convert(threeDVeg$BAPA.inst,threeDVeg$SiteIndex_Value,threeDVeg$volume)
text(s3D.coords$x, s3D.coords$y,     # x and y coordinates
     labels=(threeDVeg$Installation),       # text to plot
     pos=4, cex=.5)    












