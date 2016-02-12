library(RODBC)
library(RgoogleMaps)

#! 1. read in some tables
setwd("F:/INGY DATA/STCVRcode/STCV")
filenm <- "stcv_database_19jan2016.accdb"

path <- odbcConnectAccess2007(filenm)
sqlTables(path)[,3]

sinstloc <- sqlFetch(path,"Installations_Locations_GIS")

odbcCloseAll()


#! 2. subset of installations
locs0 <- sinstloc[sinstloc$Coordinate_Type=="DD" & 
                    !(sinstloc$Installation %in% c("BCCheck","LRCheck")),]
locs0v2 <- reshape(locs0,direction="wide",
                   v.names="Coordinate_Value",
                   timevar="Coordinate_Axis",
                   idvar=c("Installation","Plot"))
locs0v2 <- locs0v2[locs0v2$Plot==0,c(1,3,5,6)]
names(locs0v2)[3:4] <- c("Long","Lat")


#! 3. map it
ratio <- 6/10 #height to width ratio of map

png("inst_map.png",width=300*10,height=300*10*ratio,pointsize=12,res=300)
boxit <- qbbox(locs0v2$Lat,locs0v2$Long, TYPE="all")
basemap <- GetMap.bbox(boxit$lonR,boxit$latR, size=c(640,640*ratio),
                       destfile="temp.png", maptype="terra-in") #"mapmaker-hybrid",
tmp <- PlotOnStaticMap(basemap)
tmp <- PlotOnStaticMap(basemap,cex=2,pch=21,col="red",
                       lat=locs0v2$Lat,
                       lon=locs0v2$Long)
#tmp <- PlotOnStaticMap(basemap,cex=1.1,col="red",add=T,
#  lat=locs0v2$Lat,
#  lon=locs0v2$Long,
#  FUN=text,labels=locs0v2$Installation)#



dev.off()

