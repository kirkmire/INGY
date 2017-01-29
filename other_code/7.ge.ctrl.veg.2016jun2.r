#Select Control and GE Plots from list of inst and plots#
CTRLandGE.plots <- splot[which(!splot$Treatment=='1X'),]

#Merge Control&GE plots with 4m^2 plot data#
CTRLandGE.plot.veg <- merge(CTRLandGE.plots,sstp1,by=c("Installation","Plot"))

#Find minimum year of measurement in timeline#
min_year<-ddply(timeline, .(Installation), summarise, Year_Measurement=min(Year_Measurement))

#merge based on Installation and minimum year of measurement
CTRLandGE.plot.veg<-merge(CTRLandGE.plot.veg,min_year, by=c("Installation","Year_Measurement"))


#Aggregate the control and GE plots 1m^2 data by Installation and find the mean of top height and polyveg% cover#
CTRLandGE.volume.index<-aggregate(CTRLandGE.plot.veg[, 17:19], list(CTRLandGE.plot.veg$Installation,
                                                                    CTRLandGE.plot.veg$Treatment), mean, na.rm=TRUE)

#Create a US Volume Estimate by multiplying mean ployveg% cover * mean top height for each treatment w/i each inst#
CTRLandGE.volume.index$volume<-CTRLandGE.volume.index$Cover*CTRLandGE.volume.index$Top


barchart(CTRLandGE.volume.index$volume~CTRLandGE.volume.index$Group.1,groups=CTRLandGE.volume.index$Group.2,
         xlab=list(label="Installation",cex=1.5),
         ylab=list(label="Vegetation Volume",cex=1.5),
         main=list(label="Vegetation Volume at First Measurement Year", cex=1.5))


#Plot Volumes of treatments over measurement years#
inst.veg.vol.timeline<-plot(CTRLandGE.volume.index$volume~jitter(CTRLandGE.volume.index$Group.3,1.5), col=CTRLandGE.volume.index$Group.1,pch=as.integer(CTRLandGE.volume.index$Group.2))

#Extracts coords for inst labels#
xy.coords <-xy.coords(CTRLandGE.volume.index$volume~jitter(CTRLandGE.volume.index$Group.3,1.5))
text(xy.coords$x, xy.coords$y,     # x and y coordinates
     labels=(CTRLandGE.volume.index$Group.1),       # text to plot
     pos=4, cex=.8)    










