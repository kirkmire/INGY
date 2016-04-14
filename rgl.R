
library(rgl)

# color ramp
myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

plot3d(threeDVeg$SiteIndex_Value,threeDVeg$BAPA.inst,threeDVeg$volume, 
       axes=FALSE,size=7,
       col=myColorRamp(c("blue","green","yellow","red"),threeDVeg$volume),
       xlab="", ylab="", zlab="")
axes3d(c("x+", "y-", "z-"))
grid3d(side=c('x+', 'y-', 'z'), col="gray")
title3d(xlab = "Site Index", ylab = "Residual BAPA",zlab = "Vegetation Volume (M^3)", col="red")

#Adding vertical droplines#
plot3d(threeDVeg$SiteIndex_Value,threeDVeg$BAPA.inst,threeDVeg$volume,type='h',add=T,
       col=myColorRamp(c("blue","green","yellow","red"),threeDVeg$volume))

#Fitting&Plotting Treatment Planes#

just.ctrl<-threeDVeg[!(threeDVeg$Trt=='GE'),]

fit1=lm(volume~BAPA.inst+SiteIndex_Value,data=just.ctrl)
summary(fit1)

coefs <- coef(fit1)
planes3d(a=coefs["SiteIndex_Value"], b=coefs["BAPA.inst"],-1, coefs["(Intercept)"], alpha=0.50, col="plum2")

just.ge<-threeDVeg[!(threeDVeg$Trt=='CTRL'),]

fit2=lm(volume~BAPA.inst+SiteIndex_Value,data=just.ge)
summary(fit1)

coefs <- coef(fit2)
planes3d(a=coefs["SiteIndex_Value"], b=coefs["BAPA.inst"],-1, coefs["(Intercept)"], alpha=0.50, col="red")


#making a movie#

# Create a movie

movie3d(spin3d(axis = c(0, 0, 1)), duration = 3,
        dir = getwd())


#Extracts coordinates from 3D plot for installation labels#
#s3D.coords <- s3D$xyz.convert(threeDVeg$BAPA.inst,threeDVeg$SiteIndex_Value,threeDVeg$volume)
#text(s3D.coords$x, s3D.coords$y,     # x and y coordinates
#     labels=(threeDVeg$Installation),       # text to plot
#    pos=4, cex=.5)    

dev.off()
#Just SI vs BA#
ggplot(threeDVeg, aes(x =BAPA.inst , y = SiteIndex_Value,color=threeDVeg$SiteIndex_Species))+
  geom_point(size=2)+
  ggtitle("Site Index vs Residual Basal Area")+
  xlab("Residual Basal Area (ft^2/acre)")+
  ylab("Site Index")+
  labs(color="SI Species")+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14,face="bold"))












