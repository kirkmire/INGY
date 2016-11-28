
merged_stagm_stag$Installation<-as.character(merged_stagm_stag$Installation)
timeline$Installation<-as.character(timeline$Installation)


#concate Inst,Plot,STP,Tree
merged_stagm_stag$conc<-paste(merged_stagm_stag$Installation,merged_stagm_stag$Plot,merged_stagm_stag$STP,
                                merged_stagm_stag$Tree,sep=",")

#(height-prev.years height)/interval=annualized height growth

merged_stagm_stag<-merged_stagm_stag[!merged_stagm_stag$Year_Measurement==merged_stagm_stag$Year_Growth,]
merged_stagm_stag<-merged_stagm_stag[! merged_stagm_stag$Installation %in% drp,]

#Select only PIPO trees
merged_stagm_stag<-merged_stagm_stag[merged_stagm_stag$Species=="PIPO",]

#This function checks the year of measurement in the tree record against
#the measurement timeline, finds the next measurement year's height,
#then subtracts the height of the first year of measurement to provide a height increment

annual.ht<-function(conca,year){
  
  treeinfo<-merged_stagm_stag[merged_stagm_stag$conc==conca,]
  next.year <- timeline[timeline$Installation==treeinfo$Installation[1],"Year_Measurement"]
  next.year <- next.year[!is.na(next.year)]
  next.year <- next.year[next.year>year]
  next.year <- ifelse(length(next.year)==0,0,min(next.year))
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
annual.ht("LR,2,1,123",2010)

#remove DF plot 4 stp1 2001 measurements
#merged_stagm_stag<-merged_stagm_stag[!(merged_stagm_stag=='DF'&merged_stagm_stag$Plot==4&
  #                    merged_stagm_stag$STP==1&merged_stagm_stag$Year_Measurement==2001),]

#change year measurement of DF plot 4 stp1 from 2002 to 2001, was measured later due to bees nest
#merged_stagm_stag$Year_Measurement[which(merged_stagm_stag=='DF'&merged_stagm_stag$Plot==4&
 #   merged_stagm_stag$STP==1&merged_stagm_stag$Year_Measurement==2002)]<-2001

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
              
                                     







