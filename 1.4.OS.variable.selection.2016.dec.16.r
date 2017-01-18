######Overstory Tree Variables######
#TPA, BAPA, Crown Competition Factor#


#Makes OS dbh that are NA = zero, NA typically corresponds to 
#cut or dead trees, subsequent code wont (sum) aggregate NAs

soverhist$DBH[is.na(soverhist$DBH)] <- 0

#Removes dead OS trees record
dead.words<-c("DEAD","Dead","DEAD-CUT","DEAD-CUT DOWN")

soverhist<-soverhist[! soverhist$Damage %in% dead.words,]

#Aggregates OS ba data to the plot level 
#trees may grow out of <10.5 in class that are in the quarter acre plot,
#need to assign avariable that stays with OS tree regadless of growth (ie .26 or .46)
OS.plot<-function(installation,plot,tree){
  #creates dataframe of an individual OS tree in all meas years
  treeinfo<-soverhist[soverhist$Installation==installation&soverhist$Plot==plot&
          soverhist$Tree==tree,]
  #selects the min year
  years <- min(treeinfo$Year_Measurement)
  treeinfo <- treeinfo[treeinfo$Year_Measurement==years,]
if(treeinfo$DBH<10.5){
  plot.size<-.26
    }else{plot.size<-.46}
  plot.size
}

#Example on one OS tree record
OS.plot("BB",4,175)

#Apply to all overhist records

#Assign column for recent bapa
soverhist$plot.size<-0

#Apply function to every row
for(i in 1:nrow(soverhist)){
  soverhist$plot.size[i]<-OS.plot(soverhist$Installation[i], 
                                soverhist$Plot[i],
                                soverhist$Tree[i])
}


soverhist$BAPA<-((soverhist$DBH^2)*.005454)/soverhist$plot.size


agg.over.data <-aggregate(soverhist$BAPA,
                          by=list("Installation"=soverhist$Installation,
                                  "Plot"=soverhist$Plot,
                                  "Year_MeasurementOS"=soverhist$Year_Measurement)
                                  ,FUN=sum)

names(agg.over.data)[4]<-c("over.sum.bapa")

inst.year<-unique(agg.over.data[,c(1,3)])
#every inst year combo
inst.yr.1<-inst.year;inst.yr.1$Plot<-1
inst.yr.2<-inst.year;inst.yr.2$Plot<-2
inst.yr.3<-inst.year;inst.yr.3$Plot<-3
inst.yr.4<-inst.year;inst.yr.4$Plot<-4
inst.yr.5<-inst.year;inst.yr.5$Plot<-5
inst.yr.6<-inst.year;inst.yr.6$Plot<-6
inst.yr.7<-inst.year;inst.yr.7$Plot<-7

inst.combos<-rbind(inst.yr.1,inst.yr.2,inst.yr.3,
                   inst.yr.4,inst.yr.5,inst.yr.6
                  ,inst.yr.7)

inst.combos$conc<-paste(inst.combos$Installation,inst.combos$Plot,inst.combos$Year_MeasurementOS,sep=",")

agg.over.data$conc<-paste(agg.over.data$Installation,agg.over.data$Plot,agg.over.data$Year_MeasurementO,sep=",")

agg.over.check<-merge(agg.over.data,inst.combos,by=("conc"))

missing.OS<-subset(inst.combos,!( inst.combos$conc %in% agg.over.check$conc))
missing.OS.that.matter<-missing.OS[! missing.OS$Installation %in% drp60,]

#GC plot 5 OS measurements missing
#TC plots 2,6 and 7 missing as well
#No OS record for PC

#Not really missing...confirmed the lack of OS trees in hard copies of data

#Makes "zero" rows for GC Plot#5 and TC Plots#2,6,7 and PC Installation

newRow <- data.frame(Installation="GC",Plot=5,Year_MeasurementOS=1999,over.sum.bapa=0,conc="GC,5,1999")
agg.over.data<-rbind(newRow,agg.over.data)

newRow1 <- data.frame(Installation="GC",Plot=5,Year_MeasurementOS=2008,over.sum.bapa=0,conc="GC,5,2008")
agg.over.data<-rbind(newRow1,agg.over.data)

newRow2 <- data.frame(Installation="GC",Plot=5,Year_MeasurementOS=2012,over.sum.bapa=0,conc="GC,5,2012")
agg.over.data<-rbind(newRow2,agg.over.data)

newRow3 <- data.frame(Installation="TC",Plot=2,Year_MeasurementOS=2000,over.sum.bapa=0,conc="TC,2,2000")
agg.over.data<-rbind(newRow3,agg.over.data)

newRow4 <- data.frame(Installation="TC",Plot=2,Year_MeasurementOS=2010,over.sum.bapa=0,conc="TC,2,2010")
agg.over.data<-rbind(newRow4,agg.over.data)

newRow5 <- data.frame(Installation="TC",Plot=6,Year_MeasurementOS=2000,over.sum.bapa=0,conc="TC,6,2000")
agg.over.data<-rbind(newRow5,agg.over.data)

newRow6 <- data.frame(Installation="TC",Plot=6,Year_MeasurementOS=2010,over.sum.bapa=0,conc="TC,6,2010")
agg.over.data<-rbind(newRow6,agg.over.data)

newRow7 <- data.frame(Installation="TC",Plot=7,Year_MeasurementOS=2000,over.sum.bapa=0,conc="TC,7,2000")
agg.over.data<-rbind(newRow7,agg.over.data)

newRow8 <- data.frame(Installation="TC",Plot=7,Year_MeasurementOS=2010,over.sum.bapa=0,conc="TC,7,2010")
agg.over.data<-rbind(newRow8,agg.over.data)

newRow9<- data.frame(Installation="PC",Plot=1,Year_MeasurementOS=1999,over.sum.bapa=0,conc="PC,1,1999")
agg.over.data<-rbind(newRow9,agg.over.data)

newRow10<- data.frame(Installation="PC",Plot=1,Year_MeasurementOS=2008,over.sum.bapa=0,conc="PC,1,2008")
agg.over.data<-rbind(newRow10,agg.over.data)

newRow11<- data.frame(Installation="PC",Plot=2,Year_MeasurementOS=1999,over.sum.bapa=0,conc="PC,2,1999")
agg.over.data<-rbind(newRow11,agg.over.data)

newRow12<- data.frame(Installation="PC",Plot=2,Year_MeasurementOS=2008,over.sum.bapa=0,conc="PC,2,2008")
agg.over.data<-rbind(newRow12,agg.over.data)

newRow13<- data.frame(Installation="PC",Plot=3,Year_MeasurementOS=1999,over.sum.bapa=0,conc="PC,3,1999")
agg.over.data<-rbind(newRow13,agg.over.data)

newRow14<- data.frame(Installation="PC",Plot=3,Year_MeasurementOS=2008,over.sum.bapa=0,conc="PC,3,2008")
agg.over.data<-rbind(newRow14,agg.over.data)

newRow15<- data.frame(Installation="PC",Plot=4,Year_MeasurementOS=1999,over.sum.bapa=0,conc="PC,4,1999")
agg.over.data<-rbind(newRow15,agg.over.data)

newRow16<- data.frame(Installation="PC",Plot=4,Year_MeasurementOS=2008,over.sum.bapa=0,conc="PC,4,2008")
agg.over.data<-rbind(newRow16,agg.over.data)

newRow17<- data.frame(Installation="PC",Plot=5,Year_MeasurementOS=1999,over.sum.bapa=0,conc="PC,5,1999")
agg.over.data<-rbind(newRow17,agg.over.data)

newRow18<- data.frame(Installation="PC",Plot=5,Year_MeasurementOS=2008,over.sum.bapa=0,conc="PC,5,2008")
agg.over.data<-rbind(newRow18,agg.over.data)

newRow19<- data.frame(Installation="PC",Plot=6,Year_MeasurementOS=1999,over.sum.bapa=0,conc="PC,6,1999")
agg.over.data<-rbind(newRow19,agg.over.data)

newRow20<- data.frame(Installation="PC",Plot=6,Year_MeasurementOS=2008,over.sum.bapa=0,conc="PC,6,2008")
agg.over.data<-rbind(newRow20,agg.over.data)

newRow21<- data.frame(Installation="PC",Plot=7,Year_MeasurementOS=1999,over.sum.bapa=0,conc="PC,7,1999")
agg.over.data<-rbind(newRow21,agg.over.data)

newRow22<- data.frame(Installation="PC",Plot=7,Year_MeasurementOS=2008,over.sum.bapa=0,conc="PC,7,2008")
agg.over.data<-rbind(newRow22,agg.over.data)






#plot basal areas to check for missing inst/years
library(lattice)
xyplot(agg.over.data$over.sum.bapa~agg.over.data$Year_MeasurementOS|agg.over.data$Installation,groups=agg.over.data$Plot)

#Upper Metcalf OSBA decreased bt 1999 and 2010 across all installations
#possible reasons are mortality, harvest or measurement error

soverhistUM<-soverhist[soverhist$Installation=="UM",]
xyplot(soverhistUM$Height_Total~soverhistUM$Year_Measurement,groups=soverhistUM$Tree,type="b")

#tallest UM trees seems to be missing from 2010 remeasurement
#unable to locate hard copies of UM OS measurements


#Obtains an estimate of bapa at the plot level 

bapa.OS.lm<-function(installation, plot, year){
  # installation<-"BB"
  # plot<-1
  # year<-2008
  plotinfo<-agg.over.data[agg.over.data$Installation==installation&agg.over.data$Plot==plot,]
  if(min(plotinfo$Year_MeasurementOS)>=year){
    est.bapa.OS<-plotinfo$over.sum.bapa[plotinfo$Year_MeasurementOS==min(plotinfo$Year_MeasurementOS)]
  } else  if(max(plotinfo$Year_MeasurementOS)<=year){
    est.bapa.OS<-plotinfo$over.sum.bapa[plotinfo$Year_MeasurementOS==max(plotinfo$Year_MeasurementOS)]
  }else {
    newplotinfo<-plotinfo[plotinfo$Year_MeasurementOS>year,][1,]
    first.year<-plotinfo[plotinfo$Year_MeasurementOS<=year,]
    second.year<-newplotinfo[nrow(newplotinfo),]
    lm.years<-rbind(first.year,second.year)
    bapa.model<-lm(lm.years$over.sum.bapa~lm.years$Year_MeasurementOS)
    est.bapa.OS<-bapa.model$coefficients[1]+((year)*bapa.model$coefficients[2])
  }
  est.bapa.OS
}
#Example:
#plot.info<-agg.over.data[agg.over.data$Installation=="BB"&agg.over.data$Plot==1,]
#bapa.model<-lm(plotinfo$over.sum.bapa~plotinfo$Year_MeasurementOS)
#bapa.model.coef<-c(bapa.model$coefficients[2])
#summary(bapa.model)
#plot(bapa.model)

bapa.OS.lm("GC",5,1999)


annual.gr4$bapa.OS<-0

for(i in 1:nrow(annual.gr4)){
  annual.gr4$bapa.OS[i]<-bapa.OS.lm(
    annual.gr4$Installation[i], 
    annual.gr4$Plot[i],
    annual.gr4$Year_Measurement[i])
}

#Installation PC missing from OS, check for it

#OLS appropriate considering that 
#CCF and BAPA are plot level aggregates 





###Crown Competition Factor
#Crown width variable found in "CrownWidth1" and "CorwnWidth2"
#Need to create "if/else" statement that creates a new variable that either
#takes "CrownWidth1" (assumes this is the averageCW for treetaken in field)
#or takes both metrics and computes an average (assumes no average was taken in field)

soverhist$CrownWidth_1[is.na(soverhist$CrownWidth_1)] <- 0

soverhist$CrownWidth_3<-0

for(i in 1:nrow(soverhist)){
if (is.na(soverhist$CrownWidth_2[i])==T) {
      soverhist$CrownWidth_3[i]<-soverhist$CrownWidth_1[i]
} else {soverhist$CrownWidth_3[i]<-(sum(soverhist$CrownWidth_1[i],soverhist$CrownWidth_2[i])/2)
}
}

#Computes Crown Area in square feet
soverhist$CrownWidth_4<-((soverhist$CrownWidth_3/2)^2)*3.14

#Computes Crown Area in terms of percent of an acre
soverhist$Crown_MCA<-(soverhist$CrownWidth_3/(43560*soverhist$plot.size))

#Aggregates Crown Width data to the plot level as CCF 

agg.over.data.CCF <-aggregate(soverhist$Crown_MCA,
                          by=list("Installation"=soverhist$Installation,
                                  "Plot"=soverhist$Plot,
                                  "Year_MeasurementOS"=soverhist$Year_Measurement)
                          ,FUN=sum)

names(agg.over.data.CCF)[4]<-c("CCF")

agg.over.data.CCF$conc<-paste(agg.over.data.CCF$Installation,agg.over.data.CCF$Plot,agg.over.data.CCF$Year_MeasurementO,sep=",")

agg.over.check.CCF<-merge(agg.over.data.CCF,inst.combos,by=("conc"))

missing.OS<-subset(inst.combos,!( inst.combos$conc %in% agg.over.check.CCF$conc))
missing.OS.that.matter<-missing.OS[! missing.OS$Installation %in% drp60,]

newRow <- data.frame(Installation="GC",Plot=5,Year_MeasurementOS=1999,CCF=0,conc="GC,5,1999")
agg.over.data.CCF<-rbind(newRow,agg.over.data.CCF)

newRow1 <- data.frame(Installation="GC",Plot=5,Year_MeasurementOS=2008,CCF=0,conc="GC,5,2008")
agg.over.data.CCF<-rbind(newRow1,agg.over.data.CCF)

newRow2 <- data.frame(Installation="GC",Plot=5,Year_MeasurementOS=2012,CCF=0,conc="GC,5,2012")
agg.over.data.CCF<-rbind(newRow2,agg.over.data.CCF)

newRow3 <- data.frame(Installation="TC",Plot=2,Year_MeasurementOS=2000,CCF=0,conc="TC,2,2000")
agg.over.data.CCF<-rbind(newRow3,agg.over.data.CCF)

newRow4 <- data.frame(Installation="TC",Plot=2,Year_MeasurementOS=2010,CCF=0,conc="TC,2,2010")
agg.over.data.CCF<-rbind(newRow4,agg.over.data.CCF)

newRow5 <- data.frame(Installation="TC",Plot=6,Year_MeasurementOS=2000,CCF=0,conc="TC,6,2000")
agg.over.data.CCF<-rbind(newRow5,agg.over.data.CCF)

newRow6 <- data.frame(Installation="TC",Plot=6,Year_MeasurementOS=2010,CCF=0,conc="TC,6,2010")
agg.over.data.CCF<-rbind(newRow6,agg.over.data.CCF)

newRow7 <- data.frame(Installation="TC",Plot=7,Year_MeasurementOS=2000,CCF=0,conc="TC,7,2000")
agg.over.data.CCF<-rbind(newRow7,agg.over.data.CCF)

newRow8 <- data.frame(Installation="TC",Plot=7,Year_MeasurementOS=2010,CCF=0,conc="TC,7,2010")
agg.over.data.CCF<-rbind(newRow8,agg.over.data.CCF)

newRow9<- data.frame(Installation="PC",Plot=1,Year_MeasurementOS=1999,CCF=0,conc="PC,1,1999")
agg.over.data.CCF<-rbind(newRow9,agg.over.data.CCF)

newRow10<- data.frame(Installation="PC",Plot=1,Year_MeasurementOS=2008,CCF=0,conc="PC,1,2008")
agg.over.data.CCF<-rbind(newRow10,agg.over.data.CCF)

newRow11<- data.frame(Installation="PC",Plot=2,Year_MeasurementOS=1999,CCF=0,conc="PC,2,1999")
agg.over.data.CCF<-rbind(newRow11,agg.over.data.CCF)

newRow12<- data.frame(Installation="PC",Plot=2,Year_MeasurementOS=2008,CCF=0,conc="PC,2,2008")
agg.over.data.CCF<-rbind(newRow12,agg.over.data.CCF)

newRow13<- data.frame(Installation="PC",Plot=3,Year_MeasurementOS=1999,CCF=0,conc="PC,3,1999")
agg.over.data.CCF<-rbind(newRow13,agg.over.data.CCF)

newRow14<- data.frame(Installation="PC",Plot=3,Year_MeasurementOS=2008,CCF=0,conc="PC,3,2008")
agg.over.data.CCF<-rbind(newRow14,agg.over.data.CCF)

newRow15<- data.frame(Installation="PC",Plot=4,Year_MeasurementOS=1999,CCF=0,conc="PC,4,1999")
agg.over.data.CCF<-rbind(newRow15,agg.over.data.CCF)

newRow16<- data.frame(Installation="PC",Plot=4,Year_MeasurementOS=2008,CCF=0,conc="PC,4,2008")
agg.over.data.CCF<-rbind(newRow16,agg.over.data.CCF)

newRow17<- data.frame(Installation="PC",Plot=5,Year_MeasurementOS=1999,CCF=0,conc="PC,5,1999")
agg.over.data.CCF<-rbind(newRow17,agg.over.data.CCF)

newRow18<- data.frame(Installation="PC",Plot=5,Year_MeasurementOS=2008,CCF=0,conc="PC,5,2008")
agg.over.data.CCF<-rbind(newRow18,agg.over.data.CCF)

newRow19<- data.frame(Installation="PC",Plot=6,Year_MeasurementOS=1999,CCF=0,conc="PC,6,1999")
agg.over.data.CCF<-rbind(newRow19,agg.over.data.CCF)

newRow20<- data.frame(Installation="PC",Plot=6,Year_MeasurementOS=2008,CCF=0,conc="PC,6,2008")
agg.over.data.CCF<-rbind(newRow20,agg.over.data.CCF)

newRow21<- data.frame(Installation="PC",Plot=7,Year_MeasurementOS=1999,CCF=0,conc="PC,7,1999")
agg.over.data.CCF<-rbind(newRow21,agg.over.data.CCF)

newRow22<- data.frame(Installation="PC",Plot=7,Year_MeasurementOS=2008,CCF=0,conc="PC,7,2008")
agg.over.data.CCF<-rbind(newRow22,agg.over.data.CCF)

library(lattice)
xyplot(agg.over.data.CCF$CCF~agg.over.data.CCF$Year_MeasurementOS|factor(agg.over.data.CCF$Installation))



#Obtains an estimate of CCF at the plot level

#Obtains an estimate of bapa at the plot level 

CCF.OS.lm<-function(installation, plot, year){
  # installation<-"BB"
  # plot<-1
  # year<-2008
  plotinfo<-agg.over.data.CCF[agg.over.data.CCF$Installation==installation&agg.over.data.CCF$Plot==plot,]
  if(min(plotinfo$Year_MeasurementOS)>=year){
    est.CCF.OS<-plotinfo$CCF[plotinfo$Year_MeasurementOS==min(plotinfo$Year_MeasurementOS)]
  } else  if(max(plotinfo$Year_MeasurementOS)<=year){
    est.CCF.OS<-plotinfo$CCF[plotinfo$Year_MeasurementOS==max(plotinfo$Year_MeasurementOS)]
  }else {
    newplotinfo<-plotinfo[plotinfo$Year_MeasurementOS>year,][1,]
    first.year<-plotinfo[plotinfo$Year_MeasurementOS<=year,]
    second.year<-newplotinfo[nrow(newplotinfo),]
    lm.years<-rbind(first.year,second.year)
    CCF.model<-lm(lm.years$CCF~lm.years$Year_MeasurementOS)
    est.CCF.OS<-CCF.model$coefficients[1]+((year)*CCF.model$coefficients[2])
  }
  est.CCF.OS
}

#Example:
#plot.info<-agg.over.data.CCF[agg.over.data.CCF$Installation=="BB"&agg.over.data.CCF$Plot==1,]
#CCF.model<-lm(plotinfo$CCF~plotinfo$Year_MeasurementOS)
#CCF.model.coef<-c(CCF.model$coefficients[2])
#summary(CCF.model)
#plot(CCF.model)

CCF.OS.lm("BM",5,1999)


annual.gr4$CCF.OS<-0

for(i in 1:nrow(annual.gr4)){
  annual.gr4$CCF.OS[i]<-CCF.OS.lm(
    annual.gr4$Installation[i], 
    annual.gr4$Plot[i],
    annual.gr4$Year_Measurement[i])
}




###TPA Factor
#assigns how much each OS tree is "worth" at a per acre basis
#inverse of assigned plot size

soverhist$TPA<-1/soverhist$plot.size

agg.over.data.TPA <-aggregate(soverhist$TPA,
                              by=list("Installation"=soverhist$Installation,
                                      "Plot"=soverhist$Plot,
                                      "Year_MeasurementOS"=soverhist$Year_Measurement)
                              ,FUN=sum)

names(agg.over.data.TPA)[4]<-c("TPA")

agg.over.data.TPA$conc<-paste(agg.over.data.TPA$Installation,agg.over.data.TPA$Plot,agg.over.data.TPA$Year_MeasurementO,sep=",")

agg.over.check.TPA<-merge(agg.over.data.TPA,inst.combos,by=("conc"))

missing.OS<-subset(inst.combos,!( inst.combos$conc %in% agg.over.check.TPA$conc))
missing.OS.that.matter<-missing.OS[! missing.OS$Installation %in% drp60,]

newRow <- data.frame(Installation="GC",Plot=5,Year_MeasurementOS=1999,TPA=0,conc="GC,5,1999")
agg.over.data.TPA<-rbind(newRow,agg.over.data.TPA)

newRow1 <- data.frame(Installation="GC",Plot=5,Year_MeasurementOS=2008,TPA=0,conc="GC,5,2008")
agg.over.data.TPA<-rbind(newRow1,agg.over.data.TPA)

newRow2 <- data.frame(Installation="GC",Plot=5,Year_MeasurementOS=2012,TPA=0,conc="GC,5,2012")
agg.over.data.TPA<-rbind(newRow2,agg.over.data.TPA)

newRow3 <- data.frame(Installation="TC",Plot=2,Year_MeasurementOS=2000,TPA=0,conc="TC,2,2000")
agg.over.data.TPA<-rbind(newRow3,agg.over.data.TPA)

newRow4 <- data.frame(Installation="TC",Plot=2,Year_MeasurementOS=2010,TPA=0,conc="TC,2,2010")
agg.over.data.TPA<-rbind(newRow4,agg.over.data.TPA)

newRow5 <- data.frame(Installation="TC",Plot=6,Year_MeasurementOS=2000,TPA=0,conc="TC,6,2000")
agg.over.data.TPA<-rbind(newRow5,agg.over.data.TPA)

newRow6 <- data.frame(Installation="TC",Plot=6,Year_MeasurementOS=2010,TPA=0,conc="TC,6,2010")
agg.over.data.TPA<-rbind(newRow6,agg.over.data.TPA)

newRow7 <- data.frame(Installation="TC",Plot=7,Year_MeasurementOS=2000,TPA=0,conc="TC,7,2000")
agg.over.data.TPA<-rbind(newRow7,agg.over.data.TPA)

newRow8 <- data.frame(Installation="TC",Plot=7,Year_MeasurementOS=2010,TPA=0,conc="TC,7,2010")
agg.over.data.TPA<-rbind(newRow8,agg.over.data.TPA)

newRow9<- data.frame(Installation="PC",Plot=1,Year_MeasurementOS=1999,TPA=0,conc="PC,1,1999")
agg.over.data.TPA<-rbind(newRow9,agg.over.data.TPA)

newRow10<- data.frame(Installation="PC",Plot=1,Year_MeasurementOS=2008,TPA=0,conc="PC,1,2008")
agg.over.data.TPA<-rbind(newRow10,agg.over.data.TPA)

newRow11<- data.frame(Installation="PC",Plot=2,Year_MeasurementOS=1999,TPA=0,conc="PC,2,1999")
agg.over.data.TPA<-rbind(newRow11,agg.over.data.TPA)

newRow12<- data.frame(Installation="PC",Plot=2,Year_MeasurementOS=2008,TPA=0,conc="PC,2,2008")
agg.over.data.TPA<-rbind(newRow12,agg.over.data.TPA)

newRow13<- data.frame(Installation="PC",Plot=3,Year_MeasurementOS=1999,TPA=0,conc="PC,3,1999")
agg.over.data.TPA<-rbind(newRow13,agg.over.data.TPA)

newRow14<- data.frame(Installation="PC",Plot=3,Year_MeasurementOS=2008,TPA=0,conc="PC,3,2008")
agg.over.data.TPA<-rbind(newRow14,agg.over.data.TPA)

newRow15<- data.frame(Installation="PC",Plot=4,Year_MeasurementOS=1999,TPA=0,conc="PC,4,1999")
agg.over.data.TPA<-rbind(newRow15,agg.over.data.TPA)

newRow16<- data.frame(Installation="PC",Plot=4,Year_MeasurementOS=2008,TPA=0,conc="PC,4,2008")
agg.over.data.TPA<-rbind(newRow16,agg.over.data.TPA)

newRow17<- data.frame(Installation="PC",Plot=5,Year_MeasurementOS=1999,TPA=0,conc="PC,5,1999")
agg.over.data.TPA<-rbind(newRow17,agg.over.data.TPA)

newRow18<- data.frame(Installation="PC",Plot=5,Year_MeasurementOS=2008,TPA=0,conc="PC,5,2008")
agg.over.data.TPA<-rbind(newRow18,agg.over.data.TPA)

newRow19<- data.frame(Installation="PC",Plot=6,Year_MeasurementOS=1999,TPA=0,conc="PC,6,1999")
agg.over.data.TPA<-rbind(newRow19,agg.over.data.TPA)

newRow20<- data.frame(Installation="PC",Plot=6,Year_MeasurementOS=2008,TPA=0,conc="PC,6,2008")
agg.over.data.TPA<-rbind(newRow20,agg.over.data.TPA)

newRow21<- data.frame(Installation="PC",Plot=7,Year_MeasurementOS=1999,TPA=0,conc="PC,7,1999")
agg.over.data.TPA<-rbind(newRow21,agg.over.data.TPA)

newRow22<- data.frame(Installation="PC",Plot=7,Year_MeasurementOS=2008,TPA=0,conc="PC,7,2008")
agg.over.data.TPA<-rbind(newRow22,agg.over.data.TPA)

library(lattice)
xyplot(agg.over.data.TPA$TPA~agg.over.data.TPA$Year_MeasurementOS|factor(agg.over.data.TPA$Installation))

#Obtains an estimate of TPA at the plot level 
#Is it proper to linearly model the TPA...it doesnt change 
#also need to remember to address mortality

TPA.OS.lm<-function(installation, plot, year){
  # installation<-"BB"
  # plot<-1
  # year<-2008
  plotinfo<-agg.over.data.TPA[agg.over.data.TPA$Installation==installation&agg.over.data.TPA$Plot==plot,]
  if(min(plotinfo$Year_MeasurementOS)>=year){
    est.TPA.OS<-plotinfo$TPA[plotinfo$Year_MeasurementOS==min(plotinfo$Year_MeasurementOS)]
  } else  if(max(plotinfo$Year_MeasurementOS)<=year){
    est.TPA.OS<-plotinfo$TPA[plotinfo$Year_MeasurementOS==max(plotinfo$Year_MeasurementOS)]
  }else {
    newplotinfo<-plotinfo[plotinfo$Year_MeasurementOS>year,][1,]
    first.year<-plotinfo[plotinfo$Year_MeasurementOS<=year,]
    second.year<-newplotinfo[nrow(newplotinfo),]
    lm.years<-rbind(first.year,second.year)
    TPA.model<-lm(lm.years$TPA~lm.years$Year_MeasurementOS)
    est.TPA.OS<-TPA.model$coefficients[1]+((year)*TPA.model$coefficients[2])
  }
  est.TPA.OS
}

#Example:
#plot.info<-agg.over.data.TPA[agg.over.data.TPA$Installation=="BB"&agg.over.data.TPA$Plot==1,]
#TPA.model<-lm(plotinfo$TPA~plotinfo$Year_MeasurementOS)
#TPA.model.coef<-c(TPA.model$coefficients[2])
#summary(TPA.model)
#plot(TPA.model)

TPA.OS.lm("BM",5,2000)


annual.gr4$TPA.OS<-0

for(i in 1:nrow(annual.gr4)){
  annual.gr4$TPA.OS[i]<-TPA.OS.lm(
    annual.gr4$Installation[i], 
    annual.gr4$Plot[i],
    annual.gr4$Year_Measurement[i])
}





#GAM for BAPA
gam.BAPA<-gam(ht_annual~s(srHeight_Total)+s(bapa.OS),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.BAPA)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.BAPA,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

#GAM for CCF
gam.CCF<-gam(ht_annual~s(srHeight_Total)+s(CCF.OS),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.CCF)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.CCF,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


#GAM for TPA
gam.TPA<-gam(ht_annual~s(srHeight_Total)+s(TPA.OS),data=annual.gr4, family=gaussian(link="identity"))
summary(gam.CCF)

par(mfrow=c(1,2),mar=c(4,4,1,2))
plot(gam.TPA,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)




#OS Quantreg (Carrying forward CW/sTPA and shrub transect)
library(quantreg)

#QR for BAPA
qr.BAPA<-rq(ht_annual~srHeight_Total+STP+diff.S+bapa.OS,tau=c(.5),data=annual.gr4)
summary(qr.BAPA)
aic.list.OS<-AIC(qr.BAPA)[1]

#QR for CCF
qr.CCF<-rq(ht_annual~srHeight_Total+STP+diff.S+CCF.OS,tau=c(.5),data=annual.gr4)
summary(qr.CCF)
aic.list.OS<-c(aic.list.OS,AIC(qr.CCF)[1])

#QR for TPA
qr.TPA<-rq(ht_annual~srHeight_Total+STP+diff.S+TPA.OS,tau=c(.5),data=annual.gr4)
summary(qr.TPA)
aic.list.OS<-c(aic.list.OS,AIC(qr.TPA)[1])


OS.variable<-c("BAPA","CCF","TPA")

aic.list.OS<-t(as.data.frame(aic.list.OS))

colnames(aic.list.OS)<-(OS.variable)


#CW QR#####################################################

#QR for BAPA CW
qr.BAPA.CW<-rq(ht_annual~srHeight_Total+CrownWidth+diff.S+bapa.OS,tau=c(.5),data=annual.gr4)
summary(qr.BAPA.CW)
aic.list.OS.CW<-AIC(qr.BAPA.CW)[1]
nlist.OS<-length(qr.BAPA.CW$y)

#QR for CCF CW
qr.CCF.CW<-rq(ht_annual~srHeight_Total+CrownWidth+diff.S+CCF.OS,tau=c(.5),data=annual.gr4)
summary(qr.CCF.CW)
aic.list.OS.CW<-c(aic.list.OS.CW,AIC(qr.CCF.CW)[1])
nlist.OS<-c(nlist.OS,length(qr.CCF.CW$y))


#QR for TPA CW
qr.TPA.CW<-rq(ht_annual~srHeight_Total+CrownWidth+diff.S+TPA.OS,tau=c(.5),data=annual.gr4)
summary(qr.TPA.CW)
aic.list.OS.CW<-c(aic.list.OS.CW,AIC(qr.TPA.CW)[1])
nlist.OS<-c(nlist.OS,length(qr.TPA.CW$y))


OS.variable<-c("BAPA","CCF","TPA")

OS.variable<-as.data.frame(OS.variable)



OS.aic<-as.data.frame(cbind(nlist.OS,aic.list.OS.CW))

OS.aic<-cbind(OS.variable,OS.aic)


#CW has clearly seperated from stpa 
#BAPA OS var only has a marginally lower
#aic than the model that included the 
#addition of shrub transect var




