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

int.function(merged_stagm_stag$Installation[3],merged_stagm_stag$Year_Measurement[3])

#example
int.function(inst="BB",2012)

#apply function across all rows

intervals<-mapply(int.function, merged_stagm_stag$Installation, merged_stagm_stag$Year_Measurement)     
      
#assign interval values to tree record column
merged_stagm_stag$intervals<-intervals      
      



