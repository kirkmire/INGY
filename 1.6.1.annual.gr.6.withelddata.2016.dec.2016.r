######Overstory Tree Variables######
#TPA, BAPA, Crown Competition Factor#


annual.gr6$cratio<-annual.gr6$CrownLength/annual.gr6$Height_Total

#Makes OS dbh that are NA = zero, NA typically corresponds to 
#cut or dead trees, subsequent code wont (sum) aggregate NAs

soverhist$DBH[is.na(soverhist$DBH)] <- 0

#Removes dead OS trees record
dead.words<-c("DEAD","Dead","DEAD-CUT","DEAD-CUT DOWN")


soverhist<-soverhist[! soverhist$Damage %in% dead.words,]


#Example on one OS tree record
OS.plot("BB",4,175)

annual.gr6$bapa.OS<-0
annual.gr6$Installation<-as.character(annual.gr6$Installation)

for(i in 1:nrow(annual.gr6)){
  annual.gr6$bapa.OS[i]<-bapa.OS.lm(
    annual.gr6$Installation[i], 
    annual.gr6$Plot[i],
    annual.gr6$Year_Measurement[i])
}


annual.gr6$CCF.OS<-0

for(i in 1:nrow(annual.gr6)){
  annual.gr6$CCF.OS[i]<-CCF.OS.lm(
    annual.gr6$Installation[i], 
    annual.gr6$Plot[i],
    annual.gr6$Year_Measurement[i])
}

annual.gr6$TPA.OS<-0

for(i in 1:nrow(annual.gr6)){
  annual.gr6$TPA.OS[i]<-TPA.OS.lm(
    annual.gr6$Installation[i], 
    annual.gr6$Plot[i],
    annual.gr6$Year_Measurement[i])
}

annual.gr6<-merge(annual.gr6,sinst.pipo, by="Installation")


annual.gr6$InstPlot<-paste(annual.gr6$Installation,annual.gr6$Plot,sep="")
annual.gr6$InstPlot<-as.character(annual.gr6$InstPlot)
ge_sea$InstPlot<-as.character(ge_sea$InstPlot)
annual.gr6<-merge(annual.gr6, ge_sea, by="InstPlot")



