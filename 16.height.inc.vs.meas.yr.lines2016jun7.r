###

LL1<-merged_stagm_stag[merged_stagm_stag$Installation=="LL"&merged_stagm_stag$Species=="PIPO"&merged_stagm_stag$Plot%in%c(1:7)&merged_stagm_stag$Year_Measurement%in%c(1999,2001,2004,2008,2012),]


LL1_init<-aggregate(LL1$Height_Total ~ LL1$Tree+LL1$Plot+LL1$STP, LL1, function(x) min(x))


names(LL1_init)[1:4]<-c("Tree","Plot","STP","Height_Total")

LL1_both<-merge(LL1_init,LL1, by=c("Plot","STP","Tree"))

LL1_both$inc<-LL1_both$Height_Total.y-LL1_both$Height_Total.x

#Merge with splot for treatment column#
LL1_both<-merge(LL1_both,splot,by=c("Installation","Plot"))

LL1_both<-LL1_both[!LL1_both$BasalDiameter=="NA",]
boxplot(LL1_both$inc)

###Plotting

tbp<-theme(                              
  axis.title.x = element_text(face="bold", color="black", size=16),
  axis.title.y = element_text( face="bold",color="black", size=16),
  plot.title = element_text(face="bold", color = "black", size=18), 
  legend.position="bottom",axis.title.y=element_text(vjust=0.8))

lllp<-ggplot(data=LL1_both, aes(x=Year_Measurement, y=inc,color=Treatment))+geom_point()+
  labs(title="LL Tree Height Inc over Meas Years",y="Height (ft)", x = "Measurement Year")+ 
  scale_fill_brewer(palette="Dark2") +tbp+ geom_line(aes(group=LL1_both$Tree))+ylim(0,10)
lllp

##trying some 3D stuff

library(rgl)

# color ramp
myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

plot3d(threeDVeg$SiteIndex_Value,threeDVeg$ave.BAPA,threeDVeg$volume, 
       axes=FALSE,size=7,
       col=myColorRamp(c("blue","green","yellow","red"),threeDVeg$volume),
       xlab="", ylab="", zlab="")
axes3d(c("x+", "y-", "z-"))
grid3d(side=c('x+', 'y-', 'z'), col="gray")
title3d(
  #ylab = "Residual BAPA (ft/acre)",
  # zlab = "Vegetation Volume (ft^3/ft^2)",
  #xlab = "Site Index (ft)",
  col="red")

#Adding vertical droplines#
plot3d(threeDVeg$SiteIndex_Value,threeDVeg$ave.BAPA,threeDVeg$volume,type='h',add=T,
       col=myColorRamp(c("blue","green","yellow","red"),threeDVeg$volume))



                 



