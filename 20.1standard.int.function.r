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

interval<-mapply(int.function, merged_stagm_stag$Installation, merged_stagm_stag$Year_Measurement)     
      
#assign interval values to tree record column
merged_stagm_stag$interval<-interval 

#concate Inst,Plot,Tree
conc<-paste(merged_stagm_stag$Installation,merged_stagm_stag$Plot,merged_stagm_stag$Tree,sep=",")
merged_stagm_stag$conc<-conc

#(height-prev.years height)/interval=annualized height growth
annual.ht<-function(conca,year){
  treeinfo<-merged_stagm_stag[merged_stagm_stag$conc==conca,]
  treeinfo<-as.data.frame(treeinfo)
  x<-as.data.frame(cbind(treeinfo$Year_Measurement,treeinfo$Height_Total,treeinfo$interval))
  toBeRemoved<-which(x$V3=="Inf"|x$V1>year)
  x<-x[-toBeRemoved,]
  last.meas<-x[x$V1==max(x$V1),]
  pen.meas<-x[x$V1==max(x$V1[x$V1!=max(x$V1)]),]
  an.height<-((last.meas$V2-pen.meas$V2)/last.meas$V3)
  
 return(an.height)
}

#Example on a single tree
annual.ht("UW,7,92",2008)

ht_annual<-annual.ht(merged_stagm_stag$conc, merged_stagm_stag$Year_Measurement)

unique(c(merged_stagm_stag$conc,merged_stagm_stag$Year_Measurement))

last.meas-pen.meas
tert.meas<-sort(x,partial=n-2)[n-2]
quart.meas<-sort(x,partial=n-3)[n-3]

x <- as.data.frame(c(12.45,34,4,0,-234,45.6,4))
y <- as.data.frame(c(23,43,43,34,34,34,34))
z <- as.data.frame(c(x,y))
last.meas<-z[z$x==max(z$x),]
pen.meas<-x[x$Year_Measurement==max(x[x!=max(x)]),]
an.height<-((last.meas$Height_Total[1]-pen.meas$Height_Total[1])/last.meas$interval[1])

max(x)



treeinfo<-merged_stagm_stag[merged_stagm_stag$conc=="BB,1,350",]
treeinfo<-as.data.frame(treeinfo)
x<-as.data.frame(cbind(treeinfo$Year_Measurement,treeinfo$Height_Total,treeinfo$interval))
last.meas<-2008 #year
toBeRemoved<-which(x$V3=="Inf"|x$V1>2008)
x<-x[-toBeRemoved,]
last.meas<-x[x$V1==max(x$V1),]
pen.meas<-x[x$V1==max(x$V1[x$V1!=max(x$V1)]),]
an.height<-((last.meas$V2-pen.meas$V2)/last.meas$V3)




