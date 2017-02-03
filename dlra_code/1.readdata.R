## This script reads the STCV data into R


#! 1. set directory and filename of current database
filenm <- "stcv_database_Aug2016.accdb"

#! 2. access libraries
library(lattice)
library(MASS)
library(lme4)

#! 3. read the database
load(file.path("./dlra_code",paste(filenm,".Rdata",sep="")))



# 4 code from readdatabase.2016jun2
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

print(unsure)

##Code for assigning new lifeform to stran data#

stran$Lifeform1<-0
forbshrub$Lifeform<-as.character(forbshrub$Lifeform)
stran$Species_Primary<-as.character(stran$Species_Primary)
stran<-stran[!is.na(stran$Top)==T,]


lf.lookup<-function(species,top){
  # species<-"FRVE"
  # top<-.3
  row<-forbshrub[forbshrub$Species==species,]
  if(length(row$Lifeform)==1){row$Lifeform
  } else {
    if(top>3){"HS"
    } else {
      "LS"
    }}
  
}


for(i in 1:nrow(stran)){
  stran$Lifeform1[i]<-lf.lookup(
    stran$Species_Primary[i],
    stran$Top[i])
}

###For Site Quality#
Plots.lat.long1 <- Plots.lat.long[!Plots.lat.long$Installation %in% c("BCCheck","LRCheck"),
                                  c("Installation","Plot","Latitude","Longitude")]


Plots.lat.long1 <- Plots.lat.long1[!is.na(Plots.lat.long1$Latitude)==T,]

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
