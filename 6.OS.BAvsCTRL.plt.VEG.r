
#

CTRL.plots <- splot[ which(splot$Treatment=='CTRL'), ]

CTRL.plot.veg <- merge(CTRL.plots,sstp4,by=c("Installation","Plot"))

volume.index<-aggregate(CTRL.plot.veg[, 17:19], list(CTRL.plot.veg$Installation), mean, na.rm=TRUE)
volume.index$volume<-volume.index$Cover*volume.index$Base*volume.index$Top

names(volume.index)[1]<-paste("Installation")


OS.ctrl.plot<- merge(volume.index,sinst, by.x = "Installation", by.y = "Installation",all=F)

#Below is from 4.explor.plots#

BAPA.by.inst<-ddply(OverS, "Installation", summarise, BAPA.inst= sum(BAPA))

threeDVeg<-merge(BAPA.by.inst,OS.ctrl.plot, by.x = "Installation", by.y = "Installation",all=F)


library(scatterplot3d)


s3D<-scatterplot3d(threeDVeg$BAPA.inst,threeDVeg$SiteIndex_Value,threeDVeg$volume,

              pch=19,        # circle color indicates no. of cylinders
type="h", lty.hplot=2,       # lines to the horizontal plane
scale.y=.75,                 # scale y axis (reduce by 25%)
main="3-D Scatterplot",
xlab="BAPA (ft^3/Ac)",
ylab="Site Index Value",
zlab="Volume Index"
)


s3D.coords <- s3D$xyz.convert(threeDVeg$BAPA.inst,threeDVeg$SiteIndex_Value,threeDVeg$volume)
text(s3D.coords$x, s3D.coords$y,     # x and y coordinates
     labels=(threeDVeg$Installation),       # text to plot
     pos=4, cex=.5)    












