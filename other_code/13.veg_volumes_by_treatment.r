#Select Control and GE Plots from list of inst and plots#
LL_plots <- splot[(splot$Installation=='LL'), ]

#Merge Control&GE plots with 4m^2 plot data#
LL_plots_veg<- merge(LL_plots,sstp1,by=c("Installation","Plot"))

#Aggregate the control and GE plots 4m^2 data by Installation and find the mean of top height and polyveg% cover#
LL_plots_veg_vol_index<-aggregate(LL_plots_veg[, 17:19], 
      list(LL_plots_veg$Installation,
      LL_plots_veg$Treatment,
      LL_plots_veg$Year_Measurement
), mean, na.rm=TRUE)


#Create a US Volume Estimate by multiplying mean ployveg% cover * mean top height for each treatment w/i each inst#
LL_plots_veg_vol_index$volume<-(((LL_plots_veg_vol_index$Coverage/100)*10.7639)*
                                  ((LL_plots_veg_vol_index$Top-LL_plots_veg_vol_index$Base)))/10.7639


#interesting that LL veg 4m^2 plot only taken in 2008, 2012#
  
LL_veg_2004<-LL_plots_veg_vol_index[LL_plots_veg_vol_index$Group.3=="2004",]
names(LL_veg_2004)[2]<-"Treatment"


#Plot Volumes of treatments over measurement years#
library(ggplot2)

ggplot(LL_plots_veg_vol_index, aes(x=Group.3,y=volume))+
  geom_point(size=2,aes(col=Group.2))+
  ggtitle("Mean Veg volume on LL over years measured")+
  xlab("Year Measurement")+
  ylab("Vegetative volume (ft^3/ft^m from 1m^2)")


###FOR EM####################################################


#Select Control and GE Plots from list of inst and plots#
EM_plots <- splot[(splot$Installation=='EM'), ]

#Merge Control&GE plots with 4m^2 plot data#
EM_plots_veg<- merge(EM_plots,sstp1,by=c("Installation","Plot"))

#Aggregate the control and GE plots 4m^2 data by Installation and find the mean of top height and polyveg% cover#
EM_plots_veg_vol_index<-aggregate(EM_plots_veg[, 17:19], 
                                  list(EM_plots_veg$Installation,
                                       EM_plots_veg$Treatment,
                                       EM_plots_veg$Year_Measurement
                                  ), mean, na.rm=TRUE)


#Create a US Volume Estimate by multiplying mean ployveg% cover * mean top height for each treatment w/i each inst#
EM_plots_veg_vol_index$volume<-(((EM_plots_veg_vol_index$Coverage/100)*10.7639)*
                                  ((EM_plots_veg_vol_index$Top-EM_plots_veg_vol_index$Base)))/10.7639


#interesting that LL veg 4m^2 plot only taken in 2008, 2012#

EM_veg_2002<-EM_plots_veg_vol_index[EM_plots_veg_vol_index$Group.3=="2002",]
names(EM_veg_2002)[2]<-"Treatment"


#Plot Volumes of treatments over measurement years#
library(ggplot2)

ggplot(LL_plots_veg_vol_index, aes(x=Group.3,y=volume))+
  geom_point(size=2,aes(col=Group.2))+
  ggtitle("Mean Veg volume on LL over years measured")+
  xlab("Year Measurement")+
  ylab("Vegetative volume (ft^3/ft^m from 1m^2)")
