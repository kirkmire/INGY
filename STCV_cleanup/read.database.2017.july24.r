## This script reads the STCV databased into R, creating separate data frames
## for each of the Access DB tables.
##
## Change lines 7 and 8 as needed.

#! 1. set directory and filename of current database
data.dir <- "C:/Users/Colin/Desktop/R-Projects/INGY"  # note the backward slashes, check to see if it matches Rprojects folder
filenm <- "stcv_database_2017july24.accdb"


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

#! The "Plots_History" table is
#missing many of the slope, aspect and
#elevation measurements
#could retrieve these based on lat long coords

# #For extracting plot lat long data
# Plots.lat.long <- sqlFetch(path,"Plots")
# Plots.lat.long1 <- Plots.lat.long[!Plots.lat.long$Installation %in% c("BCCheck","LRCheck"),
#                                   c("Installation","Plot","Latitude","Longitude")]
# Plots.lat.long1 <- Plots.lat.long1[!is.na(Plots.lat.long1$Latitude)==T,]

# installation data
sinst <- sqlFetch(path,"Installations")
sinsthist <- sqlFetch(path,"Installations_History")
sinstloc <- sqlFetch(path,"Installations_Locations_GIS")

#!the "Installations_Location_GIS" table provides 
#plot coordinates for the installation reference tree (plot 0)
#as well as all plots in a variety of formats


#timeline data for 2017 research
#contains the associated overstory meas 
#and understory treatments for
#a subset of installations
timeline <- sqlFetch(path,"Timeline")


# installations to drop
# RL was abandoned w/o measurement; others are check plots
drp <- c("TCCheck","LRCheck","BCCheck","BBCheck","RL")
# DC gives only 1 yr of growth data, CT only 2 years, before destroyed
drp <- c(drp, "DC","CT")


# overstory data
sover <- sqlFetch(path,"Plots_Overstory_Trees")
soverhist <- sqlFetch(path,"Plots_Overstory_Measures")

#!Note that the clearcut installations do not have
#any overstory measurement record

#transect dta
strani <- sqlFetch(path,"Transects")
stran <- sqlFetch(path,"Transects_PointVeg")
stranco <- sqlFetch(path,"Transects_Cover")
strangr <- sqlFetch(path,"Transects_GrassHeight")

#!An additional ten ft of transect was added to the study
#starting in 2007. This means that transect points 16-20 are
#only available post-2007

# small tree plot data
sstp <- sqlFetch(path,"STPs")
sstp1 <- sqlFetch(path,"STPs_1meter")
sstp4 <- sqlFetch(path,"STPs_4meter")
sstpr <- sqlFetch(path,"STPs_Regeneration")
sstpt <- sqlFetch(path,"STPs_SaplingTallies")

#!Prior to 2007, only 1 m-squared quadrat data available
#4m-squared quadrat added to study while continueing
#1m-squared measurements


# tagged small tree growth data
stag <- sqlFetch(path,"STPs_TaggedTrees")
stagm <- sqlFetch(path,"STPs_TaggedTrees_Measures")

#Merges tagged tree measurements with other characteristics of tree
merged_stagm_stag <- merge(stagm, stag,by=c("Installation","Plot","STP","Tree"))

#Merges with plot data for plot characteristics such as treatment, lat/long, slope, elev
merged_stagm_stag<-merge(merged_stagm_stag,splot,by=c("Installation","Plot"))

# lookup data
spptr <- sqlFetch(path,"Lookup_Species_Tree")
sppnontr <- sqlFetch(path,"Lookup_Species_NonTree")

#!The "Lookup_Species_Veg_comp table" has a 
#complete list of all non-tree vegetation obtained 
#over the course of the study from all sampling 

#270 of the 4 letter codes have not been identified



# 
# 
# un.sstp1<-as.character(unique(sstp1$Species_Primary))
# un.sstp4<-as.character(unique(sstp4$Species_Primary))
# un.tran<-as.character(unique(stran$Species_Primary))
# 
# non.tr<-as.character(unique(sppnontr$Abbr))
# 
# tran.sp<-c(un.sstp1,un.sstp4,un.tran,non.tr)
# un.sp<-unique(tran.sp)
# un.sp<-as.data.frame(un.sp)
# 
# un.sp$Abbr<-un.sp
# 
# #575 unique non-tree plant species total
# 
# 
# merged_nontr<-merge(un.sp,sppnontr,by=c("Abbr"),all=T)
# write.csv(merged_nontr,"all_nontr_spec.csv")






odbcCloseAll()










