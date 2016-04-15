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





