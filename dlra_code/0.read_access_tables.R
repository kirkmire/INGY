## This script reads the STCV MS Access database
## and saves the tables as Rdata

#! 1. set filename of current database
filenm <- "stcv_database_Aug2016.accdb"

#! 2. access libraries
library(RODBC)

#! 3. read the database
path <- odbcConnectAccess2007(filenm)

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
forbshrub <- sqlFetch(path,"Lookup_Vegetation_Form_ckedits")

#timeline data
timeline <- sqlFetch(path,"Timeline")

timelineJan<-timeline

###For Site Quality#
Plots.lat.long <- sqlFetch(path,"Plots")

odbcCloseAll()

save(splot,splothist,
     sinst,sinsthist,sinstloc,
     sover,soverhist,
     strani,stran,stranco,strangr,
     sstp,sstp1,sstp4,sstpr,sstpt,
     stag,stagm,
     spptr,sppnontr,forbshrub,
     meast,timeline,timelineJan,
     Plots.lat.long,
     file=file.path("./dlra_code",paste(filenm,".Rdata",sep="")))





