


#Makes OS dbh that are NA = zero, NA typically corresponds to 
#cut or dead trees, subsequent code wont (sum) aggregate NAs

soverhist$DBH[is.na(soverhist$DBH)] <- 0


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

#Makes "zero" rows for GC Plot#5 and TC Plots#2,6,7

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

newRow7 <- data.frame(Installation="TC",Plot=7,Year_MeasurementOS=2010,over.sum.bapa=0,conc="TC,7,2000")
agg.over.data<-rbind(newRow7,agg.over.data)

newRow8 <- data.frame(Installation="TC",Plot=7,Year_MeasurementOS=2010,over.sum.bapa=0,conc="TC,7,2010")
agg.over.data<-rbind(newRow8,agg.over.data)

#plot basal areas to check for missing inst/years
library(lattice)
xyplot(agg.over.data$over.sum.bapa~agg.over.data$Year_MeasurementOS|agg.over.data$Installation,groups=agg.over.data$Plot)

#Upper Metcalf OSBA decreased bt 1999 and 2010 across all installations
#possible reasons are mortality, harvest or measurement error

soverhistUM<-soverhist[soverhist$Installation=="UM",]
xyplot(soverhistUM$Height_Total~soverhistUM$Year_Measurement,groups=soverhistUM$Tree,type="b")

#tallest UM trees seems to be missing from 2010 remeasurement
#unable to locate hard copies of UM OS measurements


#Function that assigns an OSBA variable based on Year_Measurement of 
#OS trees nearest that of small trees
#replaced in favor of interpolative BA function
#recent.OS<-function(installation,plot,year){
    #creates dataframe of an individual OS records in all meas years
 # treeinfo<-agg.over.data[agg.over.data$Installation==installation&agg.over.data$Plot==plot,]
  #years <- treeinfo$Year_MeasurementOS
  #selects OS records that are less than specified year
 # relevant.years <- years[years<=year]
  #selects the maximum year OS record from those remaining
 # yearOS <- ifelse(length(relevant.years)==0,0,max(relevant.years))

 # treeinfo <- treeinfo[treeinfo$Year_Measurement==yearOS,]
 # bapa<-treeinfo$over.sum.bapa
 # bapa
#}

#example on one plot
#recent.OS("BB","7",2008)

#Assign column for recent bapa
#annual.gr4$bapa1<-0

#Apply function to every row
#for(i in 1:nrow(annual.gr4)){
# annual.gr4$bapa[i]<-recent.OS(annual.gr4$Installation[i], 
                              # annual.gr4$Plot[i],
                              # annual.gr4$Year_Measurement[i])
#}


#Crown Competition Factor
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



library(lattice)
xyplot(agg.over.data.CCF$CCF~agg.over.data.CCF$Year_MeasurementOS|factor(agg.over.data.CCF$Installation))


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
plot.info<-agg.over.data[agg.over.data$Installation=="BB"&agg.over.data$Plot==1,]
bapa.model<-lm(plotinfo$over.sum.bapa~plotinfo$Year_MeasurementOS)
bapa.model.coef<-c(bapa.model$coefficients[2])
summary(bapa.model)
plot(bapa.model)

bapa.OS.lm("GC",5,1999)
#issue seems to be the 

  
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


#Obtains an estimate of CCF at the plot level


CCF.OS.lm<-function(installation, plot, year){
  # installation<-"BB"
  # plot<-1
  # year<-2008
  plotinfo<-agg.over.data.CCF[agg.over.data.CCF$Installation==installation&agg.over.data.CCF$Plot==plot,]
  if(min(plotinfo$Year_MeasurementOS)>=year){
    est.CCF.OS<-plotinfo$CCF[plotinfo$Year_MeasurementOS==min(plotinfo$Year_MeasurementOS)]
  } else  if(max(plotinfo$Year_MeasurementOS)<=year){
    est.CCF.OS<-plotinfo$over.sum.CCF[plotinfo$Year_MeasurementOS==max(plotinfo$Year_MeasurementOS)]
  }else {
    newplotinfo<-plotinfo[plotinfo$Year_MeasurementOS>year,][1,]
    first.year<-plotinfo[plotinfo$Year_MeasurementOS<=year,]
    second.year<-newplotinfo[nrow(newplotinfo),]
    lm.years<-rbind(first.year,second.year)
    CCF.model<-lm(lm.years$over.sum.CCF~lm.years$Year_MeasurementOS)
    est.CCF.OS<-CCF.model$coefficients[1]+((year)*bapa.model$coefficients[2])
  }
  est.CCF.OS
}
#Example:
plot.info<-agg.over.data.CCF[agg.over.data.CCF$Installation=="BB"&agg.over.data.CCF$Plot==1,]
CCF.model<-lm(plotinfo$over.sum.CCF~plotinfo$Year_MeasurementOS)
CCF.model.coef<-c(CCF.model$coefficients[2])
summary(CCF.model)
plot(CCF.model)

CCF.OS.lm("BB",1,2008)


annual.gr4$CCF.OS<-0

for(i in 1:nrow(annual.gr4)){
  annual.gr4$CCF.OS[i]<-bapa.OS.lm(
    annual.gr4$Installation[i], 
    annual.gr4$Plot[i],
    annual.gr4$Year_Measurement[i])
}


