
merged_stagm_stag$Installation<-as.character(merged_stagm_stag$Installation)
timeline$Installation<-as.character(timeline$Installation)


#concate Inst,Plot,STP,Tree
merged_stagm_stag$conc<-paste(merged_stagm_stag$Installation,merged_stagm_stag$Plot,merged_stagm_stag$STP,
                                merged_stagm_stag$Tree,sep=",")

#(height-prev.years height)/interval=annualized height growth

merged_stagm_stag<-merged_stagm_stag[!merged_stagm_stag$Year_Measurement==merged_stagm_stag$Year_Growth,]
merged_stagm_stag<-merged_stagm_stag[! merged_stagm_stag$Installation %in% drp,]


#This function checks the year of measurement in the tree record against
#the measurement timeline and finds the previous measurement year's height,
#then subtracts it from the year of measurement to provide a height increment

annual.ht<-function(conca,year){
  treeinfo<-merged_stagm_stag[merged_stagm_stag$conc==conca,]
  prev.year <- timeline[timeline$Installation==treeinfo$Installation[1],"Year_Measurement"]
  prev.year <- prev.year[!is.na(prev.year) ]
  prev.year <- prev.year[prev.year>year]
  prev.year <- ifelse(length(prev.year)==0,0,min(prev.year))
  treeinfo <- treeinfo[treeinfo$Year_Measurement %in% c(year,prev.year),]
    if (nrow(treeinfo)==1){
    htinc <- -999
  } else {
    htinc <- treeinfo$Height_Total[treeinfo$Year_Measurement==year] -
              treeinfo$Height_Total[treeinfo$Year_Measurement==prev.year]
    htinc <- -htinc/diff(range(treeinfo$Year_Measurement))
  }
  htinc
}
  

###Example on a single tree record
annual.ht("LR,2,1,123",2010)

#Remove records of DF plot 4 stp 1 w/ year growth=2001 due to duplicate records of year meas=2002, bees nest
merged_stagm_stag<-merged_stagm_stag[!(merged_stagm_stag$Installation=='DF'&merged_stagm_stag$Plot==4&
                    merged_stagm_stag$STP==1&merged_stagm_stag$Year_Growth==2001),]

#change year measurement of DF plot 4 stp1 from 2002 to 2001, was measured later due to bees nest
merged_stagm_stag[(merged_stagm_stag$Installation=='DF'&merged_stagm_stag$Plot==4&
merged_stagm_stag$STP==1&merged_stagm_stag$Year_Growth==2001),]<-2001

merged_stagm_stag$Year_Measurement[merged_stagm_stag=='DF'&merged_stagm_stag$Plot==4&
                                     merged_stagm_stag$STP==1&merged_stagm_stag$Year_Measurement==2002]<-2001

#Assign column for height annualized height growth inc
merged_stagm_stag$ht_annual<-0

#Apply function to every row
for(i in 1:nrow(merged_stagm_stag)){
    merged_stagm_stag$ht_annual[i]<-annual.ht(merged_stagm_stag$conc[i], merged_stagm_stag$Year_Measurement[i])
}

#Remove all records with ht_annual=NA,-999
merged_stagm_stag$ht_annual[is.na(merged_stagm_stag$ht_annual)]<--999     
merged_stagm_stag<-merged_stagm_stag[!(merged_stagm_stag$ht_annual==-999),]

#Rename dataframe something reasonable
annual.gr<-merged_stagm_stag
              
                                     







