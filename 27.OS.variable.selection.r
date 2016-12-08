
#code to remove all .y variables from df 
#may need to move this

for(i in 3:18){
  y.names<-c(y.names,names(annual.gr4[,substring(names(annual.gr4),i-1,i)==".y"]))
}

annual.gr4<-annual.gr4[,! names(annual.gr4) %in% y.names]


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


#need to add a column to annual growth that assigns a OS reference year
#reference year should be before 

#Function that assigns an OSBA variable based on Year_Measurement of 
#tree record and equal to or most recent year measurement

recent.OS<-function(installation,plot,year){
  
  #creates dataframe of an individual OS records in all meas years
  treeinfo<-agg.over.data[agg.over.data$Installation==installation&agg.over.data$Plot==plot,]
  years <- treeinfo$Year_MeasurementOS
  #selects OS records that are less than specified year
  relevant.years <- years[years<=year]
  #selects the maximum year OS record from those remaining
  yearOS <- ifelse(length(relevant.years)==0,0,max(relevant.years))

  treeinfo <- treeinfo[treeinfo$Year_Measurement==yearOS,]
  bapa<-treeinfo$over.sum.bapa
  bapa
}

#example on one plot
recent.OS("BB","7",2008)

#Assign column for recent bapa
annual.gr4$bapa<-0

#Apply function to every row
for(i in 1:nrow(annual.gr4)){
 annual.gr4$bapa[i]<-recent.OS(annual.gr4$Installation[i], 
                               annual.gr4$Plot[i],
                               annual.gr4$Year_Measurement[i])
}


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

#CCF appears to be highly correlated with year measurement
#Need to adjust for this

plot(agg.over.data.CCF$CCF~agg.over.data.CCF$Year_MeasurementOS)


#Function that assigns an CCF variable based on Year_Measurement of 
#tree record and equal to or most recent year measurement

recent.CCF<-function(installation,plot,year){
  
  #creates dataframe of OS records in all meas years
  treeinfo<-agg.over.data.CCF[agg.over.data.CCF$Installation==installation&agg.over.data.CCF$Plot==plot,]
  years <- treeinfo$Year_MeasurementOS
  #selects OS records that are less than specified year
  relevant.years <- years[years<=year]
  #selects the maximum year OS record from those remaining
  yearOS <- ifelse(length(relevant.years)==0,0,max(relevant.years))
  
  treeinfo <- treeinfo[treeinfo$Year_Measurement==yearOS,]
  CCF<-treeinfo$CCF
  CCF
}

#example on one plot
recent.CCF("BB","7",2008)

#Assign column for recent bapa
annual.gr4$CCF<-0

#Apply function to every row
for(i in 1:nrow(annual.gr4)){
  annual.gr4$CCF[i]<-recent.CCF(
    annual.gr4$Installation[i], 
    annual.gr4$Plot[i],
    annual.gr4$Year_Measurement[i])
}



  
