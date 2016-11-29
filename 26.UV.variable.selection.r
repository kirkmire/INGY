#Selecting installations 
veg<-c("EM","BC","TJ","RM","CM","TC")

annual.gr3<-annual.gr[annual.gr$Installation %in% sim, ]

#Merge Control&GE plots with 1m^2 plot data#
plots_veg<- merge(annual.gr3,sstp1,by=c("Installation","Plot","STP","Year_Measurement"))

#Aggregate the control and GE plots 4m^2 data by Installation and find the mean of top height and polyveg% cover#
veg.variables<-aggregate(annual.gr3[, 17:19], 
                                  list(LL_plots_veg$Installation,
                                       LL_plots_veg$Treatment,
                                       LL_plots_veg$Year_Measurement
                                  ), mean, na.rm=TRUE)

ht.class<-aggregate(Count~Installation+Plot+STP+Year_Measurement+HeightClass,data=sstpt,sum)