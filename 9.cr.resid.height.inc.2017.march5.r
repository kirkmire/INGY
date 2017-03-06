#Rename annual.gr4 for this figure
df_figure21<-annual.gr4

#Make predictors constant
df_figure21$elevation<-mean(df_figure21$elevation)
# df_figure21$TPA.OS<-mean(df_figure21$TPA.OS)
df_figure21$slopePercent<-mean(df_figure21$slopePercent)
df_figure21<-df_figure21[!is.na(df_figure21$cratio)==T,]
# df_figure21$cratio<-mean(df_figure21$cratio)
df_figure21$cos_rad_asp<-mean(df_figure21$cos_rad_asp)
df_figure21$sin_rad_asp<-mean(df_figure21$sin_rad_asp)

#Remove annual.ht<0 
df_figure21<-df_figure21[!df_figure21$ht_annual<0,]

#Remove cratio<0 
df_figure21<-df_figure21[!df_figure21$cratio<0,]

#use predict.rq function 
df_figure21$qr.pred.one <- predict.rq(qr.SI.1, df_figure21)
df_figure21$qr.pred.five <- predict.rq(qr.SI.5, df_figure21)
df_figure21$qr.pred.nine <- predict.rq(qr.SI.9, df_figure21)

library(rgl)

# color ramp
myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}
dev.off()


plot3d(df_figure21$cratio,df_figure21$TPA.OS,df_figure21$ht_annual, 
       size=3,
       col=myColorRamp(c("blue","green","yellow","red"),df_figure21$ht_annual),
       xlab="cratio", ylab="Residual TPA", zlab="Annualized Ht. Growth (ft)")
# axes3d(c("x+", "y-", "z-"))
grid3d(side=c('x+', 'y-', 'z'), col="gray")
title3d(
  #ylab = "Residual BAPA (ft/acre)",
  # zlab = "Vegetation Volume (ft^3/ft^2)",
  #xlab = "Site Index (ft)",
  col="red")

#Adding vertical droplines#
# plot3d(threeDVeg$SiteIndex_Value,threeDVeg$ave.BAPA,threeDVeg$volume,type='h',add=T,
#        col=myColorRamp(c("blue","green","yellow","red"),threeDVeg$volume))

#Fitting Planes#

fit1=lm(qr.pred.one~cratio+TPA.OS,data=df_figure21)
summary(fit1)

coefs <- coef(fit1)
planes3d(a=coefs["cratio"], b=coefs["TPA.OS"],
         -1, coefs["(Intercept)"], alpha=0.50, col="blue")


fit2=lm(qr.pred.five~cratio+TPA.OS,data=df_figure21)
summary(fit2)

coefs <- coef(fit2)
planes3d(a=coefs["cratio"], b=coefs["TPA.OS"],-1, coefs["(Intercept)"], alpha=0.50, col="green")

fit3=lm(qr.pred.nine~cratio+TPA.OS,data=df_figure21)
summary(fit3)

coefs <- coef(fit3)
planes3d(a=coefs["cratio"], b=coefs["TPA.OS"],-1, coefs["(Intercept)"], alpha=0.50, col="yellow")

