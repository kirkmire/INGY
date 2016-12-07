
#Makes OS dbh that are NA = zero, NA typically corresponds to 
#cut or dead trees, subsequent code wont (sum) aggregate NAs

soverhist$DBH[is.na(soverhist$DBH)] <- 0




#Aggregates OS ba data to the plot level 

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




  
