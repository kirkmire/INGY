#Select Control and GE Plots from list of inst and plots#
CTRLandGE.plots <- splot

#Merge Control&GE plots with 1m^2 plot data#
CTRLandGE.plot.veg <- merge(CTRLandGE.plots,sstp1,by=c("Installation","Plot"))

#Find minimum year of measurement in timeline#
library(plyr)
min_year<-ddply(timelineJan, .(Installation), summarise, Year_Measurement=min(Year_Measurement))

#merge based on Installation and minimum year of measurement
CTRLandGE.plot.veg<-merge(CTRLandGE.plot.veg,min_year, by=c("Installation","Year_Measurement"))

#Aggregate the control and GE plots 1m^2 data by Installation and find the mean of top height and polyveg% cover#
CTRLandGE.volume.index<-aggregate(CTRLandGE.plot.veg[, 17:19], list(CTRLandGE.plot.veg$Installation,
                                                                    CTRLandGE.plot.veg$Treatment), mean, na.rm=TRUE)

#Create a US Volume Estimate by multiplying mean ployveg% cover * mean top height for each treatment w/i each inst#
CTRLandGE.volume.index$volume<-(CTRLandGE.volume.index$Cov/100)*CTRLandGE.volume.index$Top


# library(RColorBrewer)
# myColours <- brewer.pal(6,"Blues")
# 
# my.settings <- list(
#   superpose.polygon=list(col=myColours[2:5], border="transparent"),
#   strip.background=list(col=myColours[6]),
#   strip.border=list(col="black")
# )
# 
# CTRLandGE.volume.index$Group.2<-factor(CTRLandGE.volume.index$Group.2)
# library (lattice)
# 
# barchart(CTRLandGE.volume.index$volume ~ CTRLandGE.volume.index$Group.1, groups=CTRLandGE.volume.index$Group.2,
#          origin=0,
#          #main="Motor insurance claims frequency"
#          xlab="Installation", ylab="Vegetation Volume (ft.^3)",
#          scales=list(alternating=1),
#          auto.key=list(space="right", columns=1,
#                        points=FALSE, rectangles=TRUE,
#                        title="Treatment", cex.title=1),
#          par.settings = my.settings,
#          par.strip.text=list(col="white", font=2),
#          panel=function(x,y,...){
#            panel.grid(h=-1, v=0);
#            panel.barchart(x,y,...)
#          }
# )











