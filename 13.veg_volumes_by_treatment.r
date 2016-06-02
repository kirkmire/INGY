#Select Control and GE Plots from list of inst and plots#
all_plots <- splot[(splot$Installation=='LL'), ]

#Merge Control&GE plots with 4m^2 plot data#
all_plots_veg<- merge(all_plots,sstp1,by=c("Installation","Plot"))

#Aggregate the control and GE plots 4m^2 data by Installation and find the mean of top height and polyveg% cover#
all_plots_veg_vol_index<-aggregate(all_plots_veg[, 17:19], list(all_plots_veg$Installation,
                                                                    all_plots_veg$Treatment,
                                                                    all_plots_veg$Year_Measurement
), mean, na.rm=TRUE)

#Create a US Volume Estimate by multiplying mean ployveg% cover * mean top height for each treatment w/i each inst#
all_plots_veg_vol_index$volume<-(((all_plots_veg_vol_index$Coverage/100)*10.7639)*
                                  ((all_plots_veg_vol_index$Top-all_plots_veg_vol_index$Base)))/10.7639


#interesting that LL veg 4m^2 plot only taken in 2008, 2012#
  


#Plot Volumes of treatments over measurement years#
library(ggplot2)

ggplot(all_plots_veg_vol_index, aes(x=Group.3,y=volume))+
  geom_point(size=2,aes(col=Group.2))+
  ggtitle("Mean Veg volume on LL over years measured")+
  xlab("Year Measurement")+
  ylab("Vegetative volume (ft^3/ft^m from 1m^2)")
