

#Makes "Installation" a character vector for both timeline and tree_records
merged_stagm_stag$Installation<-as.character(merged_stagm_stag$Installation)
timeline$Installation<-as.character(timeline$Installation)

#Removes tree records that are from dropped installations
merged_stagm_stag<-merged_stagm_stag[! merged_stagm_stag$Installation %in% drp60,]

#Removes tree records where year measurement=year growth
merged_stagm_stag<-merged_stagm_stag[!merged_stagm_stag$Year_Measurement==merged_stagm_stag$Year_Growth,]

#Removes all non-PIPO tree records
merged_stagm_stag<-merged_stagm_stag[merged_stagm_stag$Species=="PIPO",]

#Removes tree record that experienced specified damage
damageRemoved<-c("DEAD","D","DT","DT AD")
merged_stagm_stag<-merged_stagm_stag[!merged_stagm_stag$Damage %in% damageRemoved,]

merged_stagm_stag<-merged_stagm_stag[(merged_stagm_stag$Height_Total!=0 & !is.na(merged_stagm_stag$Height_Total)),]




#remove any for height total missing/0

#Removes all PIPO with DBH> 3.5 (no longer a small tree by definition)
# merged_stagm_stag<-merged_stagm_stag[which(merged_stagm_stag$DBH<3.5),]


#Concatenates Inst and Plot for unique STP identification
merged_stagm_stag$InstPlot<-paste(merged_stagm_stag$Installation,
                                     merged_stagm_stag$Plot,sep=",")

#Assigns a random plot 1:6 to each STP

uniqueinst<-unique(splothist[,c(2,3,4)])

if (Sys.info()['sysname']=="Linux"){
  library(gdata)
  randstp <- read.xls("rand_stp.xlsx", sheet = 1, header = TRUE)
} else {
  library(xlsx)
  #write.xlsx(uniqueinst, "rand_stp.xlsx")
  randstp<-read.xlsx("rand_stp.xlsx",sheetName = "Sheet1")
}
colnames(randstp)[4]<-"STP"


merged_stagm_stag<-merge(randstp,merged_stagm_stag,by=c("Installation","Plot","STP"))


#now just need to assign this to mergeed_stag_stagmdataframe, pair up by Inst, Plot and STP

#Concatenates Inst,Plot,STP,Tree for unique individual tree identification
merged_stagm_stag$conc<-paste(merged_stagm_stag$Installation,merged_stagm_stag$Plot,merged_stagm_stag$STP,
                                merged_stagm_stag$Tree,sep=",")


#Removes DF measurements from year measurement ==2002,
#no measurements from this year due to bees nest
#not sure where these heights came from, inconsistant with following years

merged_stagm_stag<-merged_stagm_stag[!(merged_stagm_stag$Installation=="DF"&
                                       merged_stagm_stag$Plot==4&
                                       merged_stagm_stag$STP==1&
                                       merged_stagm_stag$Year_Measurement==2002),]

#This function checks the year of measurement in the tree record against
#the measurement timeline, finds the previous measurement year's height,
#then subtracts the height in the first year of measurement to provide a 
#height increment that is assigned to the "ht_annual" var

annual.ht<-function(conca,year){
  #conca="EM,1,1,608"
  #year=2001
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
merged_stagm_stag$ht_annual[is.na(merged_stagm_stag$ht_annual)]<- -999     
merged_stagm_stag<-merged_stagm_stag[!(merged_stagm_stag$ht_annual==-999),]


# keep the right growth periods
keep <- unique(timeline[,c("Installation","Year_Measurement")])
keep <- keep[!is.na(keep$Year_Measurement),]
merged_stagm_stag2 <- merge(merged_stagm_stag,keep)

#Rename dataframe something reasonable
annual.gr<-merged_stagm_stag2

#Removes unneeded columns in df
f.names<-names(annual.gr[,substring(names(annual.gr),1,1)=="F"])

other.names<-c("Height_Growth3Year","ID.x")

annual.gr<-annual.gr[,!(names(annual.gr) %in% f.names)]
annual.gr<-annual.gr[,!(names(annual.gr) %in% other.names)]

#Remove tree EM 1,5,636, lean resulted in measurement inconsistancies and a -10 ft annual ht
#maybe remove from annual.gr get it out of model earlier

annual.gr<-annual.gr[!annual.gr$conc=="EM,1,5,636",]

#Removes trees with negative height increments
annual.gr<-annual.gr[!annual.gr$ht_annual<0,]


# Look at damage codes
table(droplevels(annual.gr$Damage)) # how many of these codes can you decipher?
#SW=sweep
#FT=forked top
#FUT= damaged (f**ed up) top catchall
#RT=regenerated top
#DT=dead top
#BT=broken top
#ID= missing ID (we're pretty sure this is the right one though based on azimuth&dist)
#AD= animal damage
#MT= mistletoe
#SP= species error?
#X=? (160 have this code), no damage?


# 
# annual.gr[annual.gr$Damage=="D",] # dead? remove
# #will look up
tempdf<-annual.gr[grep("RT",annual.gr$Damage,invert=F),] # dead top?
# 
# 
table(droplevels(tempdf$Damage)) # how many of these codes can you decipher?
# 
# 
# hist(annual.gr$ht_annual)
# hist(tempdf$ht_annual)




            