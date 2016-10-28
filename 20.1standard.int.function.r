#This function checks the year of measurement in the tree record against
#the measurement timeline df to assign an interval length that can later 
#be used to standardize the growth intervals to one year 

int.function<-function(inst,year){
  inst.df<-timeline[timeline$Installation==inst,]
  inst.df<-as.data.frame(inst.df)
  inst.df <- inst.df[!is.na(inst.df$Year_Measurement),]
  if (year==min(inst.df$Year_Measurement)) {
    interval<-0
  } else {
    next.small<-max(inst.df$Year_Measurement[which(inst.df$Year_Measurement<year)])
    interval<-year-next.small
  }
    return(interval)
}

merged_stagm_stag$Installation<-as.character(merged_stagm_stag$Installation)
timeline$Installation<-as.character(timeline$Installation)


int.function(merged_stagm_stag$Installation[3],merged_stagm_stag$Year_Measurement[3])

#example
int.function(inst="BB",2012)

#apply function across all rows

interval<-mapply(int.function, merged_stagm_stag$Installation, merged_stagm_stag$Year_Measurement)     
      
#assign interval values to tree record column
merged_stagm_stag$interval<-interval 

#concate Inst,Plot,Tree
merged_stagm_stag$conc<-paste(merged_stagm_stag$Installation,merged_stagm_stag$Plot,merged_stagm_stag$Tree,sep=",")

#(height-prev.years height)/interval=annualized height growth

merged_stagm_stag<-merged_stagm_stag[!merged_stagm_stag$Year_Measurement==merged_stagm_stag$Year_Growth,]

annual.ht<-function(conca,year){
  treeinfo<-merged_stagm_stag[merged_stagm_stag$conc==conca,]
  prev.year <- timeline[timeline$Installation==treeinfo$Installation[1],"Year_Measurement"]
  prev.year <- prev.year[!is.na(prev.year) ]
  prev.year <- prev.year[prev.year<year]
  prev.year <- ifelse(length(prev.year)==0,0,max(prev.year))
  treeinfo <- treeinfo[treeinfo$Year_Measurement %in% c(year,prev.year),]
    if (nrow(treeinfo)==1){
    htinc <- -999
  } else {
    htinc <- treeinfo$Height_Total[treeinfo$Year_Measurement==year] -
              treeinfo$Height_Total[treeinfo$Year_Measurement==prev.year]
    htinc <- htinc/diff(range(treeinfo$Year_Measurement))
  }
  htinc
}
  


#Example on a single tree record
annual.ht("LR,2,123",2010)


merged_stagm_stag$ht_annual<-0

for(i in 1:nrow(merged_stagm_stag)){
    merged_stagm_stag$ht_annual[i]<-annual.ht(merged_stagm_stag$conc[i], merged_stagm_stag$Year_Measurement[i])
}


