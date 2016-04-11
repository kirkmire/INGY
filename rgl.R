library(rgl)

fakedata <- data.frame(x=runif(100,0,10),
                       y=runif(100,0,10),
                       z=rnorm(100,20,5))

plot3d(fakedata$x,fakedata$y,fakedata$z, 
       axes=TRUE,
       xlab="easting", ylab="northing", zlab="height")

plot3d(fakedata$x,fakedata$y,fakedata$z, 
       axes=FALSE,
       xlab="easting", ylab="northing", zlab="height")



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
title3d(xlab = "Site Index", ylab = "Residual BAPA",zlab = "Volume Index", col="red")

#Adding vertical droplines#
plot3d(threeDVeg$SiteIndex_Value,threeDVeg$BAPA.inst,threeDVeg$volume,type='h',add=T,
       col=myColorRamp(c("blue","green","yellow","red"),threeDVeg$volume))

#trying something else#
threeDVeg<-threeDVeg[!(threeDVeg$Installation==c('DC','GR','GS','KC','EM','LL','LF')),]
#DC has no CTRL


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








###Good  OS.BA vs CTRL.plt#

library(scatterplot3d)
s3D<-scatterplot3d(threeDVeg$BAPA.inst,threeDVeg$SiteIndex_Value,threeDVeg$volume,
                   
                   
                   type="h", lty.hplot=2,       # lines to the horizontal plane
                   scale.y=.75,                 # scale y axis (reduce by 25%)
                   main="3-D Scatterplot",
                   xlab="Residual BAPA (ft^3/Ac)",
                   ylab="Site Index Value",
                   zlab="Volume Index",
                   pch=as.integer(threeDVeg$Trt)
)

#Extracts coordinates from 3D plot for installation labels#
s3D.coords <- s3D$xyz.convert(threeDVeg$BAPA.inst,threeDVeg$SiteIndex_Value,threeDVeg$volume)
text(s3D.coords$x, s3D.coords$y,     # x and y coordinates
     labels=(threeDVeg$Installation),       # text to plot
     pos=4, cex=.5)    

#Fancy $hit#
#Need to remove NA plots of GR and GS#
#No Site index recorded for both#

threeDVeg<-threeDVeg[!threeDVeg$Installation==c("GR","GS"),]

plotPlaneFancy=function(model=NULL,plotx1=NULL,plotx2=NULL,plotPoints=T,plotDroplines=T,npp=50,x1lab=NULL,x2lab=NULL,ylab=NULL,x1lim=NULL,x2lim=NULL,cex=0.5,col.palette=NULL,segcol="darkgrey",interval="none",confcol="lightgrey",confalpha=0.4,lit=T,outfile="graph.png",aspect=c(1,1,0.7),zoom=1,userMatrix=matrix(c(0.80,-0.60,0.022,0,0.23,0.34,0.91,0,-0.55,-0.72,0.41,0,0,0,0,1),ncol=4,byrow=T),windowRect=c(0,29,1920,1032)) { # or library(colorRamps);col.palette <- matlab.like(1000)
  library(rockchalk)
  library(rgl)
  library(colorRamps)
  mf=model.frame(model);emf=rockchalk::model.data(model)
  if (is.null(x1lab)) x1lab=plotx1
  if (is.null(x2lab)) x2lab=plotx2
  if (is.null(ylab)) ylab=names(mf)[[1]]
  if (is.null(col.palette)) col.palette=rev(colorRampPalette(rainbow(13,s=0.9,v=0.8),bias=0.6,interpolate ="spline")(1000))
  x1=emf[,plotx1]
  x2=emf[,plotx2]
  y=mf[,1]
  if (is.null(x1lim)) x1lim=c(min(x1),max(x1))
  if (is.null(x2lim)) x2lim=c(min(x2),max(x2))
  preds=predictOMatic(model,predVals=c(plotx1,plotx2),n=npp,divider="seq",interval=interval)
  ylim=c(min(c(preds$fit,y)),max(c(preds$fit,y)))
  open3d(zoom=zoom,userMatrix=userMatrix,windowRect=windowRect)
  if (plotPoints) plot3d(x=x1,y=x2,z=y,type="s",col=col.palette[(y-min(y))*999/diff(range(y))+1],radius=cex,aspect=aspect,xlab=x1lab,ylab=x2lab,zlab=ylab,lit=lit)
  if (!plotPoints) plot3d(x=x1,y=x2,z=y,type="n",col=col.palette[(y-min(y))*999/diff(range(y))+1],radius=cex,aspect=aspect,xlab=x1lab,ylab=x2lab,zlab=ylab)
  if ("lwr" %in% names(preds)) persp3d(x=unique(preds[,plotx1]),y=unique(preds[,plotx2]),z=matrix(preds[,"lwr"],npp,npp),color=confcol, alpha=confalpha, lit=lit, back="lines",add=TRUE)
  ypred=matrix(preds[,"fit"],npp,npp)
  cols=col.palette[(ypred-min(ypred))*999/diff(range(ypred))+1]
  persp3d(x=unique(preds[,plotx1]),y=unique(preds[,plotx2]),z=ypred,color=cols, alpha=0.7, lit=lit, back="lines",add=TRUE)
  if ("upr" %in% names(preds)) persp3d(x=unique(preds[,plotx1]),y=unique(preds[,plotx2]),z=matrix(preds[,"upr"],npp,npp),color=confcol, alpha=confalpha, lit=lit, back="lines",add=TRUE)
  if (plotDroplines) segments3d(x=rep(x1,each=2),y=rep(x2,each=2),z=matrix(t(cbind(y,fitted(model))),nc=1),col=segcol,lty=2)
  if (!is.null(outfile)) rgl.snapshot(outfile, fmt="png", top=TRUE)
}

BAPA.INST<-threeDVeg$BAPA.inst


plotPlaneFancy(fit1, plotx1 = "BAPA.INST", plotx2 = "threeDVeg$SiteIndex_Value",cex=0.6)
plotPlaneFancy(fit1, plotx1 = "BAPA.INST", plotx2 = "threeDVeg$SiteIndex_Value",cex=0.5,interval="confidence")
plotPlaneFancy(fit1, plotx1 = "BAPA.INST", plotx2 = "threeDVeg$SiteIndex_Value",cex=0.5,interval="prediction")







