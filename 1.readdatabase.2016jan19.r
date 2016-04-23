## This script reads the STCV databased into R, creating separate data frames
## for each of the Access DB tables.
##
## Change lines 7 and 8 as needed.

#! 1. set directory and filename of current database
data.dir <- "G:/INGY/INGY DATA/data"  # note the backward slashes
filenm <- "stcv_database_19jan2016.accdb"


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
meast <- sqlFetch(path,"Meas_Timeline")

odbcCloseAll()




#! 4. installations to drop
# RL was abandoned w/o measurement; others are check plots
drp <- c("TCCheck","LRCheck","BCCheck","BBCheck","RL")
# DC gives only 1 yr of growth data, CT only 2 years, before destroyed
drp <- c(drp, "DC","CT")



