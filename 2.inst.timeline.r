
#! 2. installations to drop
# RL was abandoned w/o measurement; others are check plots
drp <- c("TCCheck","LRCheck","BCCheck","BBCheck","RL")
# DC gives only 1 yr of growth data, CT only 2 years, before destroyed
drp <- c(drp, "DC","CT")


library(lattice)
#! 1. plot treatment history
#pdf(file="plothist.pdf",height=11,width=8.5)
xyplot(Plot ~ Year_Measurement | Installation,
       data=splothist[!(splothist$Installation %in% drp),],
       trt=splothist[!(splothist$Installation %in% drp),"Action"],
       xlab="Year",ylab="Plot",
       main=paste("STCV Plot History (",filenm,")",sep=""),
       panel=function(x,y,subscripts,trt,...){
         insttrt <- trt[subscripts]
         #colr <- ifelse(insttrt %in% c("IT","RT"),"red","blue")
         sym <- ifelse(insttrt %in% c("IT","RT"),19,21)
         for (i in unique(y)){
           panel.xyplot(x[y==i],i,type="l")
           #panel.xyplot(x[y==i],i,type="p",pch=19,col=colr[y==i])
           panel.xyplot(x[y==i],i,type="p",pch=sym[y==i],fill="white")
          
         }
       })
#dev.off()

#! 2. summarize installation
head(sinst[!(sinst$Installation %in% drp),c(2,4,5,6,7,8,9,10,11)])

#! 2. get installation history
trt <- unique(splothist[splothist$Action %in% c("IT","RT"),c(2,6)])
trt <- droplevels(trt[!(trt$Installation %in% drp),])
first.trt <- aggregate(trt$Year_Measurement,list(Installation=trt$Installation),min)
sinsthist2 <- droplevels(sinsthist[!(sinsthist$Installation %in% drp),])
sinsthist3 <- merge(sinsthist2,first.trt,all.x=T)

xyplot(Installation ~ Year_Measurement,
       xlim=c(1997,2016),
       data=sinsthist3,
       htyr=sinsthist3[,"Height_Year"],
       firstyr=sinsthist3[,"x"],  
       xlab="Year in which measurements were taken",ylab="Installation",
       scales=list(x=list(at=1998:2015)),
       main=paste("STCV Installation History (",filenm,")",sep=""),
       panel=function(x,y,htyr,firstyr,...){
         for (i in as.numeric(unique(y))){
           subx <- x[as.numeric(y)==i]
           panel.xyplot(subx,i,type="l")
           panel.xyplot(subx,i,type="p",pch=21,fill="white",cex=1.45)
           subht <- htyr[as.numeric(y)==i]
           special <- !is.na(subht) #& (subht %in% c(0,1,2,3,4,8,12))
           panel.xyplot(subx[special],i,type="p",pch=19,cex=2)
           panel.text(subx[special],i,subht[special],col="white",cex=.7,
                      adj=c(.45,.35))
           first <- firstyr[as.numeric(y)==i] - .15
           panel.segments(first,i-.3,first,i+.3,col="red",lwd=2)
         }
       })

trt2 <- merge(trt,first.trt)
trt2$num <- 1
for (i in 2:nrow(trt2)){
  trt2$num[i] <- ifelse(trt2$Year_Measurement[i]==trt2$x[i],1,trt2$num[i-1]+1)
}
trt3 <- reshape(trt2,v.names="Year_Measurement",idvar="Installation",timevar="num",direction="wide",drop="x")
sinsthist3 <- merge(sinsthist2,trt3,all.x=T)

xyplot(Installation ~ Year_Measurement,
       xlim=c(1997,2016),
       data=sinsthist3,
       htyr=sinsthist3[,"Height_Year"],
       trtyr=sinsthist3[,13:17],   #sinsthist3[,14:18],  
       xlab="Year in which measurements were taken",ylab="Installation",
       scales=list(x=list(at=1998:2015)),
       main=paste("STCV Installation History (",filenm,")",sep=""),
           panel=function(x,y,htyr,trtyr,...){
         for (i in as.numeric(unique(y))){
           subx <- x[as.numeric(y)==i]
           panel.xyplot(subx,i,type="l")
           panel.xyplot(subx,i,type="p",pch=21,fill="white",cex=1.45)
           subht <- htyr[as.numeric(y)==i]
           special <- !is.na(subht) #& (subht %in% c(0,1,2,3,4,8,12))
           panel.xyplot(subx[special],i,type="p",pch=15,cex=2)
           panel.text(subx[special],i,subht[special],col="white",cex=.7,
                      adj=c(.45,.35))
           first <- trtyr[as.numeric(y)==i,1] - .15
           panel.segments(first,i-.3,first,i+.3,col="red",lwd=2)
           for (j in 2:5){
             othr <- trtyr[as.numeric(y)==i,j] - .15
             panel.segments(othr,i-.3,othr,i+.3,col="green",lwd=2)
           }
         }
       })


#Sleeker Timeline Figures with ggplot2#
library(ggplot2)
library(ggthemes)

mteq <- sinsthist3[sinsthist3$Year_Measurement.1==sinsthist3$Year_Measurement,]

ggplot(sinsthist3, aes(x=Year_Measurement, y=Installation,colour=factor(Entered=="0"),
                       shape=factor(Entered=="0"),label=Year_Measurement.1 ))+
  geom_point(alpha=1,size=5)+theme(
    panel.grid.minor.x=element_line(size = 1),
    text = element_text(size=20),
    axis.text.x = element_text(angle=0, vjust=.5),
    axis.ticks = element_line(size = 2),
    legend.position = "none",
    legend.background = element_rect(colour = "black"),
    legend.key = element_rect(colour = "black"),
    legend.title = element_text(face = "italic"),
    
    plot.title = element_text(size = rel(1.2), colour = "black"),
    panel.border = element_rect(colour = "black", fill=NA, size=1))+
  ggtitle("STCV Installation History")+xlab("Year Measured")+
  scale_colour_grey(name = "Measured")+
  scale_shape_manual(values = c(16,0))+
  scale_x_continuous(breaks=seq(1995,2015,2))



  


