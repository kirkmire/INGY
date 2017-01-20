## This script reads the STCV databased into R, creating separate data frames
## for each of the Access DB tables.
##
## Change lines 7 and 8 as needed.

#! 1. set directory and filename of current database
data.dir <- "C:/Users/Colin/Desktop/R-Projects/INGY"  # note the backward slashes, check to see if it matches Rprojects folder
filenm <- "stcv_database_Aug2016.accdb"


#! 2. access libraries
library(RODBC)
library(lattice)
library(MASS)
library(lme4)

#! 3. read the database
path <- odbcConnectAccess2007(paste(data.dir,filenm,sep="/"))
sqlTables(path)[,3]

# plot data
splot <- sqlFetch(path,"Plots")
splothist <- sqlFetch(path,"Plots_History")

# installation data
sinst <- sqlFetch(path,"Installations")
sinsthist <- sqlFetch(path,"Installations_History")
sinstloc <- sqlFetch(path,"Installations_Locations_GIS")

# overstory data
sover <- sqlFetch(path,"Plots_Overstory_Trees")
soverhist <- sqlFetch(path,"Plots_Overstory_Measures")

#transect dta
strani <- sqlFetch(path,"Transects")
stran <- sqlFetch(path,"Transects_PointVeg")
stranco <- sqlFetch(path,"Transects_Cover")
strangr <- sqlFetch(path,"Transects_GrassHeight")

# small tree plot data
sstp <- sqlFetch(path,"STPs")
sstp1 <- sqlFetch(path,"STPs_1meter")
sstp4 <- sqlFetch(path,"STPs_4meter")
sstpr <- sqlFetch(path,"STPs_Regeneration")
sstpt <- sqlFetch(path,"STPs_SaplingTallies")

# tagged small tree growth data
stag <- sqlFetch(path,"STPs_TaggedTrees")
stagm <- sqlFetch(path,"STPs_TaggedTrees_Measures")

# lookup data
spptr <- sqlFetch(path,"Lookup_Species_Tree")
sppnontr <- sqlFetch(path,"Lookup_Species_NonTree")

#timeline data
timeline <- sqlFetch(path,"Timeline")


#Forb/Shrub reference data
forbshrub <- sqlFetch(path,"Lookup_Vegetation_Form")

#Function for assigning "lifeform" to transect point data
#Results posted to updated accdb

length((forbshrub$Species))

lookup<-function(species){
   row<-forbshrub[forbshrub$Species==species,]
  ifelse(length(row$Lifeform)==1,
         "good",
         ###issue: some species (SYAL, AMAL) listed as F and S
         ifelse(row$Lifeform=="F",
                "bad",
                "good"))
  
}

forbshrub$Lifeform2<-0

for(i in 1:nrow(forbshrub)){
  forbshrub$Lifeform2[i]<-lookup(
    forbshrub$Species[i])
}

unsure<-forbshrub[forbshrub$Lifeform2=="bad",]
library(xlsx)
write.xlsx(unsure, "unsure.xlsx")

print(unsure)

lf.lookup<-function(species,top){
  species<-"SYAL"
  top<-4
  row<-forbshrub[forbshrub$Species==species,]
  ifelse(length(row$Lifeform)==1,
    lf<-row$Lifeform,
    ###issue: some species (SYAL, AMAL) listed as F and S
    ifelse(top>3,
          lf<-"HS",
          lf<-"LS"))
  lf
}

###For Site Quality#
latlong.STCV <- sqlFetch(path,"Installations_Locations_GIS")
latlong.STCV<-latlong.STCV[latlong.STCV$Coordinate_Type=="DD",]

latlong.STCV.data<-reshape(latlong.STCV, direction="wide",idvar=
                        c("Installation","Plot"),
                      timevar="Coordinate_Axis")

library(reshape2)
latlong.STCV.data<-melt(latlong.STCV, id.vars=c("Installation","Plot","Year_Measurement"))

stran$Lifeform1<-0

for(i in 1:nrow(stran)){
  stran$Lifeform1[i]<-lf.lookup(
    stran$Species_Primary[i],
    stran$Top[i])
}





odbcCloseAll()

#merged stagm and stag#
merged_stagm_stag <- merge(stagm, stag,by=c("Installation","Plot","STP","Tree"))

#merge with plot data for treatment etc
merged_stagm_stag<-merge(merged_stagm_stag,splot,by=c("Installation","Plot"))

#! 4. installations to drop
# RL was abandoned w/o measurement; others are check plots
drp <- c("TCCheck","LRCheck","BCCheck","BBCheck","RL")
# DC gives only 1 yr of growth data, CT only 2 years, before destroyed
drp <- c(drp, "DC","CT")
# Add installations with <60 PIPO at initiation to drp
stag$count<-1
spec.freq.table<-xtabs(count~Installation+Species, data=stag)
spec.freq.table<-as.data.frame(spec.freq.table)
spec.freq.table<-spec.freq.table[! spec.freq.table$Installation %in% drp,]
pi<-spec.freq.table[spec.freq.table$Species=="PIPO",]
lt60<-pi[pi$Freq<60,]
lt60<-lt60$Installation
lt60<-as.vector(lt60)
drp60<-c(drp,lt60)
