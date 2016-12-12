

#Makes "Installation" a character vector for both timeline and tree_records
merged_stagm_stag$Installation<-as.character(merged_stagm_stag$Installation)
timeline$Installation<-as.character(timeline$Installation)

#Removes tree records that are from dropped installations
merged_stagm_stag<-merged_stagm_stag[! merged_stagm_stag$Installation %in% drp,]

#Removes tree records where year measurement=year growth
merged_stagm_stag<-merged_stagm_stag[!merged_stagm_stag$Year_Measurement==merged_stagm_stag$Year_Growth,]

#Removes all non-PIPO tree records
merged_stagm_stag<-merged_stagm_stag[merged_stagm_stag$Species=="PIPO",]

#Concatenates Inst,Plot,STP,Tree for unique individual tree identification
merged_stagm_stag$conc<-paste(merged_stagm_stag$Installation,merged_stagm_stag$Plot,merged_stagm_stag$STP,
                                merged_stagm_stag$Tree,sep=",")


#This function checks the year of measurement in the tree record against
#the measurement timeline, finds the previous measurement year's height,
#then subtracts the height in the first year of measurement to provide a 
#height increment that is assigned to the 

annual.ht<-function(conca,year){
  #creates dataframe of an individual small tree in all meas years
  treeinfo<-merged_stagm_stag[merged_stagm_stag$conc==conca,]
  #selects all treeinfo records in years that are not found in timeline
  next.year <- timeline[timeline$Installation==treeinfo$Installation[1],"Year_Measurement"]
  #removes NA from treerecords
  next.year <- next.year[!is.na(next.year)]
  #selects tree records that are greater than specified year
  next.year <- next.year[next.year>year]
  #selects the minimum tree record from those remaining
  next.year <- ifelse(length(next.year)==0,0,min(next.year))
  #selects treeinfo records that correspond to specified year and "next year"
  #produces error of -999 if nrow of treeinfo =1
  #otherwise, subtracts spec year height from next year height and 
  #divides by the range in the measurement years
  #produces output "htinc" which is in units ft/year
  treeinfo <- treeinfo[treeinfo$Year_Measurement %in% c(year,next.year),]
    if (nrow(treeinfo)==1){
    htinc <- -999
  } else {
    htinc <- treeinfo$Height_Total[treeinfo$Year_Measurement==next.year]-
           treeinfo$Height_Total[treeinfo$Year_Measurement==year] 
              
        htinc <- htinc/diff(range(treeinfo$Year_Measurement[1],treeinfo$Year_Measurement[2]))
  }
  htinc
}
  
###Example on a single tree record
annual.ht("LR,2,5,138",2002)

#Assign column for height annualized height growth inc
merged_stagm_stag$ht_annual<-0

#Apply function to every row
for(i in 1:nrow(merged_stagm_stag)){
    merged_stagm_stag$ht_annual[i]<-annual.ht(merged_stagm_stag$conc[i], merged_stagm_stag$Year_Measurement[i])
}

#Remove all records with ht_annual=NA,-999, 
#these represent end-of-timeline-interval tree records
merged_stagm_stag$ht_annual[is.na(merged_stagm_stag$ht_annual)]<--999     
merged_stagm_stag<-merged_stagm_stag[!(merged_stagm_stag$ht_annual==-999),]

#Rename dataframe something reasonable
annual.gr<-merged_stagm_stag

#Removes unneeded columns in df
f.names<-names(annual.gr[,substring(names(annual.gr),1,1)=="F"])

other.names<-c("Height_Growth3Year","ID.x")

annual.gr<-annual.gr[,!(names(annual.gr) %in% f.names)]
annual.gr<-annual.gr[,!(names(annual.gr) %in% other.names)]

#Remove tree EM 1,5,636, lean resulted in measurement inconsistancies and a -10 ft annual ht
#maybe remove from annual.gr get it out of model earlier

annual.gr<-annual.gr[!annual.gr$conc=="EM,1,5,636",]


            