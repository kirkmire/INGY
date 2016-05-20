###Western Installation BC###########################################################################

BC1 <- stagm[stagm$Installation=='BC'& stagm$Plot=="4",]

plot(BC1$Height_Total~BC1$Year_Measurement)

library(ggplot2)

xyplot(BC1$Height_Total ~ BC1$Year_Measurement,
       type=c("p","smooth"), ylab="Measurement Year Height-Previous Year Height (ft.)",
       xlab="Measurement Year Height (ft)", main="Height Growth From Previous Year vs. Measurement Year Height (ft)",
       pch=4,auto.key=T)+theme(tbp)

tbp<-theme(                              
  axis.title.x = element_text(face="bold", color="black", size=16),
  axis.title.y = element_text( face="bold",color="black", size=16),
  plot.title = element_text(face="bold", color = "black", size=18), 
  legend.position="bottom",axis.title.y=element_text(vjust=0.8))

bclp<-ggplot(data=BC1, aes(x=BC1$Year_Measurement, y=(BC1$Height_Total*.3048)))+geom_point()+
  labs(title="Western Plot Control (BC)",y="Height (m)", x = "Measurement Year")+ 
  scale_fill_brewer(palette="Dark2") +tbp+ geom_line(aes(group=BC1$Tree))+ylim(0,10)
bclp


##############Central Plot Controll################################

KC1 <- stagm[stagm$Installation=='KC'& stagm$Plot=="7",]

plot(KC1$Height_Total~KC1$Year_Measurement)

library(ggplot2)

kclp<-ggplot(data=KC1, aes(x=KC1$Year_Measurement, y=(KC1$Height_Total*.3048)))+geom_point()+
  labs(title="Central Plot Control (BC)",y="Height (m)", x = "Measurement Year")+ 
  scale_fill_brewer(palette="Dark2") +tbp+ geom_line(aes(group=KC1$Tree))+ylim(0,10)
kclp


##############Eastern Plot Controll################################

SG1 <- stagm[stagm$Installation=='SG'& stagm$Plot=="5",]

plot(SG1$Height_Total~SG1$Year_Measurement)

library(ggplot2)

sglp<-ggplot(data=SG1, aes(x=SG1$Year_Measurement, y=(SG1$Height_Total*.3048)))+geom_point()+
  labs(title="Eastern Plot Control (SG)",y="Height (m)", x = "Measurement Year")+ 
  scale_fill_brewer(palette="Dark2") +tbp+ geom_line(aes(group=SG1$Tree))+ylim(0,10)
sglp



