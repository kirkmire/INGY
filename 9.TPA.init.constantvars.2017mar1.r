library(rgl)

# color ramp
#myColorRamp <- function(colors, values) {
#  v <- (values - min(values))/diff(range(values))
#  x <- colorRamp(colors)(v)
#  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
#}
#dev.off()


#Adding vertical droplines#
# plot3d(threeDVeg$SiteIndex_Value,threeDVeg$ave.BAPA,threeDVeg$volume,type='h',add=T,
#        col=myColorRamp(c("blue","green","yellow","red"),threeDVeg$volume))

#Fitting Planes#

res <- 30
hts <- seq(2,22,length=res)
tpa <- seq(0,100,length=res)

qfunction <- function(ht,tpa){
  newdf <- data.frame(srHeight_Total=sqrt(ht),
                      cratio=.5,
                      TPA.OS=tpa,
                      slopePercent=11.223331,
                      elevation=1003.537,
                      cos_rad_asp=0.0325605,
                      sin_rad_asp=0.1114712)
  predict(qr.SI.1,newdata=newdf)
}

htsurf1 <- matrix(qfunction(rep(hts,each=length(tpa)),
                            rep(tpa,length(hts))),
                  nrow=length(hts),byrow=T)

qfunction5 <- function(ht,tpa){
  newdf <- data.frame(srHeight_Total=sqrt(ht),
                      cratio=.5,
                      TPA.OS=tpa,
                      slopePercent=11.223331,
                      elevation=1003.537,
                      cos_rad_asp=0.0325605,
                      sin_rad_asp=0.1114712)
  predict(qr.SI.5,newdata=newdf)
}

htsurf5 <- matrix(qfunction5(rep(hts,each=length(tpa)),
                            rep(tpa,length(hts))),
                  nrow=length(hts),byrow=T)

qfunction9 <- function(ht,tpa){
  newdf <- data.frame(srHeight_Total=sqrt(ht),
                      cratio=.5,
                      TPA.OS=tpa,
                      slopePercent=11.223331,
                      elevation=1003.537,
                      cos_rad_asp=0.0325605,
                      sin_rad_asp=0.1114712)
  predict(qr.SI.9,newdata=newdf)
}

htsurf9 <- matrix(qfunction9(rep(hts,each=length(tpa)),
                             rep(tpa,length(hts))),
                  nrow=length(hts),byrow=T)


plot3d(hts,tpa,c(min(htsurf1),max(htsurf9)),
       size=3,
       col=myColorRamp(c("blue","green","yellow","red")),
       xlab="", ylab="",
       zlab="",
       type="n"
)
# axes3d(c("x+", "y-", "z-"))
grid3d(side=c('x+', 'y-', 'z'), col="gray")
title3d(
  #ylab = "Residual BAPA (ft/acre)",
  # zlab = "Vegetation Volume (ft^3/ft^2)",
  #xlab = "Site Index (ft)",
  col="red")

surface3d(hts,tpa,htsurf1, alpha=0.50, col="red")
surface3d(hts,tpa,htsurf5, alpha=0.40, col="yellow")
surface3d(hts,tpa,htsurf9, alpha=0.30, col="green")
