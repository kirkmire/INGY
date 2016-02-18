##Select Control and GE Plots from list of inst and plots#
CTRLandGE.plots <- splot[which(splot$Treatment==c('CTRL','GE')), ]

#Merge Control&GE plots with 4m^2 plot data#
CTRLandGE.plot.veg <- merge(CTRLandGE.plots,sstp4,by=c("Installation","Plot"))

#Aggregate the control and GE plots 4m^2 data by Installation and find the mean of top height and polyveg% cover#
CTRLandGE.volume.index<-aggregate(CTRLandGE.plot.veg[, 17:19], list(CTRLandGE.plot.veg$Installation,
                                                                    CTRLandGE.plot.veg$Treatment,
                                                                    CTRLandGE.plot.veg$Year_Measurement
                                                                    ), mean, na.rm=TRUE)

#Create a US Volume Estimate by multiplying mean ployveg% cover * mean top height for each treatment w/i each inst#
CTRLandGE.volume.index$volume<-CTRLandGE.volume.index$Cover*CTRLandGE.volume.index$Top

#Plot Volumes of treatments over measurement years#
inst.veg.vol.timeline<-plot(CTRLandGE.volume.index$volume~jitter(CTRLandGE.volume.index$Group.3,1.5), col=CTRLandGE.volume.index$Group.1,pch=as.integer(CTRLandGE.volume.index$Group.2))

#Extracts coords for inst labels#
xy.coords <-xy.coords(CTRLandGE.volume.index$volume~jitter(CTRLandGE.volume.index$Group.3,1.5))
text(xy.coords$x, xy.coords$y,     # x and y coordinates
     labels=(CTRLandGE.volume.index$Group.1),       # text to plot
     pos=4, cex=.8)    










