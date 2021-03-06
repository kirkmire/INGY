# set the directory to file where the 21 downloaded R functions are#
sdir = "C:/open-fvs/rFVS/R"
for (rf in dir (sdir)) source (paste(sdir,rf,sep="/"))


# load the FVS libraryin rstudio (where you have built a copy of your FVS variant)#

fvsLoad("FVSiec", bin="C:/open-fvs/trunk/bin")


#set the directory for writing the .key file#
key.dir <- getwd()
key.filename <- "trial.key"
keyfile <- file.path(key.dir,key.filename)


##make data##

kc.trees<-annual.gr4[which(annual.gr4$Installation=="KC"),]



FVS.Tree.Data<- data.frame(plot=paste(kc.trees$Plot,kc.trees$Year_Measurement,sep=""),
                           tree=paste(
                             #kc.trees$Plot,
                             kc.trees$STP,
                             #kc.trees$Year_Measurement,
                             kc.trees$Tree,
                                      sep=""),
                          count=27.74,
                          species="PP",
                          dbh=round(kc.trees$DBH,digits=1),
                          hist=1,
                          height=kc.trees$Height_Total,
                          crown.ratio=100*(kc.trees$Height_Total-kc.trees$Height_CrownBase)/kc.trees$Height_Total)

#Need to bring in Overstory Trees#
OS.KC.Trees<-soverhist[which(soverhist$Installation=="KC"&soverhist$Year_Measurement==2001),]

OS.KC.Trees.2002<-soverhist[which(soverhist$Installation=="KC"&soverhist$Year_Measurement==2001),]
OS.KC.Trees.2002$Year_Measurement[OS.KC.Trees.2002$Year_Measurement== 2001] <- 2002

OS.KC.Trees.2003<-soverhist[which(soverhist$Installation=="KC"&soverhist$Year_Measurement==2001),]
OS.KC.Trees.2003$Year_Measurement[OS.KC.Trees.2003$Year_Measurement== 2001] <- 2003

OS.KC.Trees.2006<-soverhist[which(soverhist$Installation=="KC"&soverhist$Year_Measurement==2001),]
OS.KC.Trees.2006$Year_Measurement[OS.KC.Trees.2006$Year_Measurement== 2001] <- 2006

OS.KC.Trees.2010<-soverhist[which(soverhist$Installation=="KC"&soverhist$Year_Measurement==2001),]
OS.KC.Trees.2010$Year_Measurement[OS.KC.Trees.2010$Year_Measurement== 2001] <- 2010

OS.KC.Trees<-rbind(OS.KC.Trees,
      OS.KC.Trees.2002,
      OS.KC.Trees.2003,
      OS.KC.Trees.2006,
      OS.KC.Trees.2010)
      
      
     
FVS.Tree.Data.OS<- data.frame(plot=paste(OS.KC.Trees$Plot,OS.KC.Trees$Year_Measurement,sep=""),
                           tree=paste(
                             #OS.KC.Trees$Plot,
                             OS.KC.Trees$STP,
                             #OS.KC.Trees$Year_Measurement,
                             OS.KC.Trees$Tree,
                             sep=""),
                           count=1,
                           species="PP",
                           dbh=round(OS.KC.Trees$DBH,digits=1),
                           hist=1,
                           height=OS.KC.Trees$Height_Total,
                           crown.ratio=OS.KC.Trees$CrownRatio)

for(i in 1:nrow(FVS.Tree.Data.OS)){
  FVS.Tree.Data.OS$count[i]<-if(OS.KC.Trees$DBH[i]>10){2}
  else{4}
}



#Rbind small and OS trees
FVS.Tree.Data<-rbind(FVS.Tree.Data,FVS.Tree.Data.OS)

#FVS.Tree.Data1<-FVS.Tree.Data[!is.na(FVS.Tree.Data$dbh==TRUE),]

FVS.Tree.Data$dbh[is.na(FVS.Tree.Data$dbh)] <- 0.1


#unfortunately rFVS doesnt seem to be able to handle NA or 0.0 DBHS
#many small trees are not tall enough to have a DBH!

##From essential FVS:##
#Trees smaller than 4.5 feet in height should be assigned
#a small, but nonzero, diameter (for example, an estimated bud width, or 0.1 inch).
#This diameter will not be incremented until projected
#height becomes greater than 4.5 feet. DBH must be recorded if the tree is to be projected;
#records with blank or zero DBH values are ignored. 

#Have to remove negative crown ratios
FVS.Tree.Data<-FVS.Tree.Data[which(FVS.Tree.Data$crown.ratio>0),]

#Have to abbreviate tree and plot info
#FVS.Tree.Data <- transform(FVS.Tree.Data,tree=as.numeric(factor(tree)))
FVS.Tree.Data <- transform(FVS.Tree.Data,plot=as.numeric(factor(plot)))

#data.frame(plot=rep(1:4,each=5),tree=1:20,
#     count=1,species="DF",dbh=rnorm(20,12,3),
#                      hist=1,height=NA,crown.ratio=c(NA,runif(19,0,1)*100))

##OR Load Your Own Dataframe##

#FVS.Tree.Data<- dens200[dens200$year<1967,]

#Make sure that your crown ratios are out of 100 (ie not .30)

#FVS.Tree.Data$crown.ratio<-FVS_TreeInit$crown.ratio*100#


#Make sure that the column headers of your data frame match this:#
  #note that you may have to change headers from what you use in suppose#
  #plot:      plot number corresponding to tree record#
  #tree:     for tree identifer number#
  #count:    for how many trees are represented by the record
  #species:  see FVS IE variant for species codes#
  #dbh:      diameter at breast height#
  #hist:     1=alive, 6 =tree died during obs period, 9=died prior to obs#
  
  ##Entries for the following are optional, although NA if absent##
  #height:       in feet
  #crown.ratio:  as a percentage of 100

## set options##

#Suppress simulated natural Ingrowth? (TRUE/FALSE)#
NOAUTOES <- TRUE

#Request output table that shows a statistical desription of input data#
STATS <- TRUE

#Set seed to 0 for stochastic output, to some other
#integer for repeatable stochastic output#
RANNSEED <- 55329

#Prevent tripling of tree records?#
NOTRIPLE <- TRUE 

#Name of Stand (STDIDENT)#
stdname<-("Low")

##Cruise Design##
#BAF- Negative value is interpreted as the inverse of a large fixed area plot#
BAF<-(-1)

FRP<-10


#Break point- the diameter cuttoff between large and small tree plots, default is 5in, put "999"#
#for cruise designs that only use one plot size#
DiamCO<-(999)
#Plot Count#
Plotcount<-(length(unique(FVS.Tree.Data$plot)))
#each half acre large tree plot considered as a plot

#Non-Stockable Plots#
nonstock<-(0)
#Prop of stand considered stockable#  
propstockstand<-(1)


##Stand Info (STDINFO)##
#National Forest Near Where Stand is Located (Refer to variant guide)#
NFcode<-(106)
#Cour de'Alene

#Stand habitat code
habtype <- 620
#Stand age in years#
age<-(0)
#Stand Aspect in degrees #
aspect<-(315)
#Stand Slope in percent#
slope<-(10)
#Stand Elevation in feet#
elev <- 2000
elev <- round(elev/100)

##Inventory Year (INVYEAR)##
year <- (2000)

##Number of Cycles to be projected (NUMCYCLE)##
numcycle<-(10)

## Cycle length in years ##
cyclelen <- (5)


# make a keyword file (set to where your make.keyword.r file is located)
source('C:/open-fvs/rFVS/R/make.keyword.R')







