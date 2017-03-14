library(rgl)

#Fitting Planes#

res <- 30
hts <- seq(2,22,length=res)
cra <- seq(0,1,length=res)

qfunction <- function(ht,cra){
  newdf <- data.frame(srHeight_Total=sqrt(ht),
                      cratio=cra,
                      TPA.OS=35,
                      slopePercent=11.223331,
                      elevation=1003.537,
                      cos_rad_asp=0.0325605,
                      sin_rad_asp=0.1114712)
  predict(qr.SI.1,newdata=newdf)
}

htsurf1 <- matrix(qfunction(rep(hts,each=length(cra)),
                            rep(cra,length(hts))),
                  nrow=length(hts),byrow=T)

qfunction5 <- function(ht,cra){
  newdf <- data.frame(srHeight_Total=sqrt(ht),
                      cratio=cra,
                      TPA.OS=35,
                      slopePercent=11.223331,
                      elevation=1003.537,
                      cos_rad_asp=0.0325605,
                      sin_rad_asp=0.1114712)
  predict(qr.SI.5,newdata=newdf)
}

htsurf5 <- matrix(qfunction5(rep(hts,each=length(cra)),
                            rep(cra,length(hts))),
                  nrow=length(hts),byrow=T)

qfunction9 <- function(ht,cra){
  newdf <- data.frame(srHeight_Total=sqrt(ht),
                      cratio=cra,
                      TPA.OS=35,
                      slopePercent=11.223331,
                      elevation=1003.537,
                      cos_rad_asp=0.0325605,
                      sin_rad_asp=0.1114712)
  predict(qr.SI.9,newdata=newdf)
}

htsurf9 <- matrix(qfunction9(rep(hts,each=length(cra)),
                            rep(cra,length(hts))),
                  nrow=length(hts),byrow=T)


plot3d(hts,cra,c(min(htsurf1),max(htsurf9)),
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

surface3d(hts,cra,htsurf1, alpha=0.50, col="red")
surface3d(hts,cra,htsurf5, alpha=0.40, col="yellow")
surface3d(hts,cra,htsurf9, alpha=0.30, col="green")
