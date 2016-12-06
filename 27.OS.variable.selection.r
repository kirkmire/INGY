
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


#Aggregates OS ba data to the plot level 

soverhist$BAPA<-ifelse(soverhist$DBH<10.5,((soverhist$DBH^2)*.005454)/.26,((soverhist$DBH^2)*.005454)/.46) 

agg.over.data <-aggregate(soverhist$BAPA,
                          by=list("Installation"=soverhist$Installation,
                                  "Plot"=soverhist$Plot,
                                  "Year_MeasurementOS"=soverhist$Year_Measurement)
                                  ,FUN=sum)


names(agg.over.data)[4]<-c("over.sum.bapa")


#need to add a column to annual growth that assigns a OS reference year

  
