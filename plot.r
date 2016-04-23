
#Circle of Plot#
dev.off()
library(plotrix)






plot(0:170,0:170,type="n",xlab="",ylab="",main="Plot Design",asp = 1)
#Large tree plot#
draw.circle(80,80,80,border="black",nv=1000,
            col=NA,lty=1,lwd=2)
#Medium Tree plot#
draw.circle(80,80,60,border="red",lty=3,lwd=3)

#Small tree Plots#
draw.circle(80,111,10,border="green",lty=1,lwd=1)
draw.circle(80,49,10,border="green",lty=1,lwd=1)

#side small trees#
draw.circle(106.8,95.5,10,border="green",lty=1,lwd=1)
draw.circle(106.8,64.5,10,border="green",lty=1,lwd=1)
draw.circle(53.2,64.5,10,border="green",lty=1,lwd=1)
draw.circle(53.2,95.5,10,border="green",lty=1,lwd=1)

#1ft radius plot center#
draw.circle(80,80,1,border="green",lty=1,lwd=1)


#Draw radial lines#
draw.radial.line(0,31, center=c(80,80),deg=30)
draw.radial.line(0,31, center=c(80,80),deg=90)
draw.radial.line(0,31, center=c(80,80),deg=150)
draw.radial.line(0,31, center=c(80,80),deg=210)
draw.radial.line(0,31, center=c(80,80),deg=270)
draw.radial.line(0,31, center=c(80,80),deg=330)

#Drawing 4x4 Veg Quad Squares#
rect(80,111,col=NA,border="black")



draw.circle(3.5,7,0.8,border="blue",lty=2,lwd=2)


plot(1:5,seq(1,10,length=5),type="n",xlab="",ylab="",main="Test draw.circle",asp = 1)
draw.circle(2,4,c(1,0.66,0.33),border="purple",
            col=c("#ff00ff","#ff77ff","#ffccff"),lty=1,lwd=1)
draw.circle(2.5,8,0.6,border="red",lty=3,lwd=3)
draw.circle(4,3,0.7,border="green",lty=1,lwd=1)
draw.circle(3.5,7,0.8,border="blue",lty=2,lwd=2)




## an example showing colouring and shading
plot(c(100, 200), c(300, 450), type= "n", xlab = "", ylab = "")
rect(100, 300, 125, 350) # transparent
rect(100, 400, 125, 450, col = "green", border = "blue") # coloured
rect(115, 375, 150, 425, col = par("bg"), border = "transparent")
rect(150, 300, 175, 350, density = 10, border = "red")
rect(150, 400, 175, 450, density = 30, col = "blue",
     angle = -30, border = "transparent")