
#Makes OS dbh that are NA = zero, NA typically corresponds to 
#cut or dead trees, subsequent code wont (sum) aggregate NAs

soverhist$DBH[is.na(soverhist$DBH)] <- 0




#Aggregates OS ba data to the plot level 
#trees may grow out of <10.5 in class that are in the quarter acre plot,
#need to assign avariable that stays with OS tree regadless of growth (ie .26 or .46)
OS.plot<-function(installation,plot,tree){
  
  #creates dataframe of an individual small tree in all meas years
  treeinfo<-soverhist[soverhist$Installation==installation&soverhist$Plot==plot&
                        soverhist$Tree=tree,]
  years <- treeinfo$Year_MeasurementOS
  #selects tree records that are less than specified year
  relevant.years <- years[years<=year]
  #selects the maximum tree record from those remaining
  yearOS <- ifelse(length(relevant.years)==0,0,min(relevant.years))
  
  treeinfo <- treeinfo[treeinfo$Year_Measurement==yearOS,]
  bapa<-treeinfo$over.sum.bapa
  bapa
}
soverhist$OSplot<-ifelse(soverhist$DBH<10.5,.26,.46) 


soverhist$BAPA<-ifelse(soverhist$DBH<10.5,((soverhist$DBH^2)*.005454)/.26,((soverhist$DBH^2)*.005454)/.46) 



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
  
  #creates dataframe of an individual small tree in all meas years
  treeinfo<-agg.over.data[agg.over.data$Installation==installation&agg.over.data$Plot==plot,]
  years <- treeinfo$Year_MeasurementOS
  #selects tree records that are less than specified year
  relevant.years <- years[years<=year]
  #selects the maximum tree record from those remaining
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
 annual.gr4$bapa[i]<-recent.OS(annual.gr4$Installation[1], 
                               annual.gr4$Plot[1],
                               annual.gr4$Year_Measurement[1])
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
soverhist$CrownWidth_3<-((soverhist$CrownWidth_3/2)^2)*3.14

#Computes Crown Area in terms of percent of an acre
soverhist$Crown_MCA<-(soverhist$CrownWidth_3/43560)*100

soverhist$BAPA<-ifelse(soverhist$DBH<10.5,((soverhist$DBH^2)*.005454)/.26,((soverhist$DBH^2)*.005454)/.46) 

agg.over.data <-aggregate(soverhist$BAPA,
                          by=list("Installation"=soverhist$Installation,
                                  "Plot"=soverhist$Plot,
                                  "Year_MeasurementOS"=soverhist$Year_Measurement)
                          ,FUN=sum)

names(agg.over.data)[4]<-c("over.sum.bapa")


#Aggregates Crown Width data to the plot level 

soverhist$BAPA<-ifelse(soverhist$DBH<10.5,((soverhist$DBH^2)*.005454)/.26,((soverhist$DBH^2)*.005454)/.46) 

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
  
  #creates dataframe of an individual small tree in all meas years
  treeinfo<-agg.over.data[agg.over.data$Installation==installation&agg.over.data$Plot==plot,]
  years <- treeinfo$Year_MeasurementOS
  #selects tree records that are less than specified year
  relevant.years <- years[years<=year]
  #selects the maximum tree record from those remaining
  yearOS <- ifelse(length(relevant.years)==0,0,max(relevant.years))
  
  treeinfo <- treeinfo[treeinfo$Year_Measurement==yearOS,]
  bapa<-treeinfo$over.sum.bapa
  bapa
}








  
