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

plot3d(fakedata$x,fakedata$y,fakedata$z, 
       axes=FALSE,
       col=myColorRamp(c("blue","green","yellow","red"),fakedata$z),
       xlab="", ylab="", zlab="")
axes3d(c("x+", "y-", "z-"))
grid3d(side=c('x+', 'y-', 'z'), col="gray")
title3d(xlab = "UTM.Easting", ylab = "UTM.Northing",zlab = "Height", col="red")
planes3d(0, 0, -1, 20, col="gray", alpha=0.7)   # plane

