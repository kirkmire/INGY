#Rename annual.gr4 for this figure
df_figureBF1<-annual.gr4
df_figureBF1<-df_figureBF1[which(df_figureBF1$Installation=="BF"),]


#Make predictors constant
# df_figureBF1$elevation<-mean(df_figureBF1$elevation)
# df_figureBF1$TPA.OS<-mean(df_figureBF1$TPA.OS)
# df_figureBF1$slopePercent<-mean(df_figureBF1$slopePercent)
# df_figureBF1<-df_figureBF1[!is.na(df_figureBF1$cratio)==T,]
# df_figureBF1$cratio<-mean(df_figureBF1$cratio)
# df_figureBF1$cos_rad_asp<-mean(df_figureBF1$aspect)
# df_figureBF1$sin_rad_asp<-mean(df_figureBF1$sin_rad_asp)

#Remove annual.ht<0 
df_figureBF1<-df_figureBF1[!df_figureBF1$ht_annual<0,]

#Remove cratio<0 
df_figureBF1<-df_figureBF1[!df_figureBF1$cratio<0,]

#use predict.rq function 
df_figureBF1$qr.pred.one <- predict.rq(qr.SI.1, df_figureBF1)
df_figureBF1$qr.pred.five <- predict.rq(qr.SI.5, df_figureBF1)
df_figureBF1$qr.pred.nine <- predict.rq(qr.SI.9, df_figureBF1)

library(rgl)

# color ramp
myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}
dev.off()


library(rgl)

#Fitting Planes#

res <- 30
hts <- seq(2,27,length=res)
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


plot3d(df_figureBF1$Height_Total,df_figureBF1$cratio,df_figureBF1$ht_annual,
       size=5,col=myColorRamp(c("red","orange","yellow","green"),df_figureBF1$ht_annual),
       xlab="", ylab="", zlab="")
# axes3d(c("x+", "y-", "z-"))
grid3d(side=c('x+', 'y-', 'z-'), col="gray")
title3d(
  #ylab = "Residual BAPA (ft/acre)",
  # zlab = "Vegetation Volume (ft^3/ft^2)",
  #xlab = "Site Index (ft)",
  col="red")

surface3d(hts,cra,htsurf1, alpha=0.50, col="red")
surface3d(hts,cra,htsurf5, alpha=0.40, col="yellow")
surface3d(hts,cra,htsurf9, alpha=0.30, col="green")

