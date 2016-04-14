#Using John's STCV INstallation Summary

locdata <- sinstloc[sinstloc$Coordinate_Type=="DD"& sinstloc$Plot=='0',]
locdataX <- locdata[locdata$Coordinate_Axis=='X',]
#locdataY <- locdata[locdata$Coordinate_Axis=='Y',]

#colnames(stcv_installations1)[c(8,11)] <-c("AET","BA")

#total <- merge(locdataX,locdataY,by="Installation")
#master<-merge(total,sinst,by="Installation")


#cc=is.na(master$AET)
#m=which(cc==c("TRUE"))
#final=master[-m,]


#col.rainbow <- cm.colors(19)
#palette(col.rainbow)


#Find Overstory BAPA
soverhist$BA<-(soverhist$DBH^2)*.005454


#Find minumim year of measurement at each installation#

firstOSyear<-aggregate(soverhist$Year_Measurement,list(soverhist$Installation),min)
colnames(firstOSyear) <- c("Installation","Year_Measurement")

#Exclude all non first year measurements#
just.first.year<- subset(soverhist, soverhist$Installation==firstOSyear$Installation & soverhist$Year_Measurement ==firstOSyear$first_year)

soverhist.first <- merge(soverhist,firstOSyear,by=c("Installation","Year_Measurement"))
  

#Find per acre basal area
ba<-aggregate(soverhist.first$BA ~ soverhist.first$Installation, FUN=sum,digits=5)
names(ba)[2] <- "basal"
names(ba)[1] <- "Installation"
ba$BAPA<-(ba$basal)/3.22

options(digits=4)

ba.final<-merge(ba,sinst,by="Installation")
ba.final

#Plot SI vs Overstory BA

plot(ba.final$SiteIndex_Value~ba.final$BAPA, xlab="Basal Area Per Acre",ylab="Site Index", main="Site Index vs. Initital Overstory BAPA",pch=19,cex.main=2,cex.lab=1.5,cex.axis=1.5,col=ba.final$SiteIndex_Species,cex=2)
identify(ba.final$SiteIndex_Value~ba.final$BAPA)
palette()
grid()
legend(55, 95,legend=c("LAOC-Black","PIPO-Red","PSME-Green"), fill=c("Black","Red","Green"))

summary(ba.final$SiteIndex_Species)



library(ggplot2)

ba.final<-merge(locdataX,ba.final,by="Installation")

ggplot(ba.final, aes(x =BAPA , y = SiteIndex_Value,color=ba.final$SiteIndex_Species))+
  geom_point(size=2)+
   ggtitle("Log(Height) vs Log (Force) by Species")+
  ggtitle("Log(Height) vs Log (Force) by Factor")+
  xlab("Log(Force)")+
  ylab("Log(Height)")


