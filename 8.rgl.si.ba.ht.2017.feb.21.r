
library(rgl)

# color ramp
myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

annual.gr43<-annual.gr4[!annual.gr4$ht_annual<0,]
annual.gr43<-annual.gr43[!annual.gr43$ht_annual>4,]


plot3d(annual.gr43$SiteIndex_Value,annual.gr43$bapa.OS,annual.gr43$ht_annual, 
     size=3,
       col=myColorRamp(c("blue","green","yellow","red"),annual.gr43$ht_annual),
       xlab="", ylab="", zlab="")
# axes3d(c("x+", "y-", "z-"))
grid3d(side=c('x+', 'y-', 'z'), col="gray")
# title3d(
#   ylab = "Residual BAPA (ft. sq./acre)",
#   zlab = "Annual Height Growth (ft.)",
#   xlab = "",
#   col="black")

#Adding vertical droplines#
# plot3d(threeDVeg$SiteIndex_Value,threeDVeg$ave.BAPA,threeDVeg$volume,type='h',add=T,
#        col=myColorRamp(c("blue","green","yellow","red"),threeDVeg$volume))

#Fitting&Plotting Treatment Planes#

# just.ctrl<-annual.[!(threeDVeg$Trt=='GE'),]

fit1=rq(annual.gr43$ht_annual~bapa.OS+SiteIndex_Value,data=annual.gr43,tau=.9)
summary(fit1)

coefs <- coef(fit1)
planes3d(a=coefs["SiteIndex_Value"], b=coefs["bapa.OS"],-1, coefs["(Intercept)"], alpha=0.40, col="yellow")

# just.ge<-threeDVeg[!(threeDVeg$Trt=='CTRL'),]

fit2=rq(annual.gr43$ht_annual~bapa.OS+SiteIndex_Value,data=annual.gr43,tau=.5)
summary(fit2)

coefs <- coef(fit2)
planes3d(a=coefs["SiteIndex_Value"], b=coefs["bapa.OS"],-1, coefs["(Intercept)"], alpha=0.50, col="green")

# just.ge<-threeDVeg[!(threeDVeg$Trt=='CTRL'),]

fit3=rq(annual.gr43$ht_annual~bapa.OS+SiteIndex_Value,data=annual.gr43,tau=.1)
summary(fit3)

coefs <- coef(fit3)
planes3d(a=coefs["SiteIndex_Value"], b=coefs["bapa.OS"],-1, coefs["(Intercept)"], alpha=0.60, col="blue")


#making a movie#

# Create a movie

movie3d(spin3d(axis = c(0, 0, 1)), duration = 15,
        dir = getwd())

getwd()

#Extracts coordinates from 3D plot for installation labels#
#s3D.coords <- s3D$xyz.convert(threeDVeg$BAPA.inst,threeDVeg$SiteIndex_Value,threeDVeg$volume)
#text(s3D.coords$x, s3D.coords$y,     # x and y coordinates
#     labels=(threeDVeg$Installation),       # text to plot
#    pos=4, cex=.5)    

dev.off()


