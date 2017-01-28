## This script reads the STCV data into R


#! 1. set directory and filename of current database
filenm <- "stcv_database_Jun2016.accdb"

#! 2. access libraries
library(lattice)
library(MASS)
library(lme4)

#! 3. read the database
load(file.path("./dlra_code_for_linux",paste(filenm,".Rdata",sep="")))

#merged stagm and stag#
merged_stagm_stag <- merge(stagm, 
                           stag,
                           by=c("Installation","Plot","STP","Tree"))


#! 4. installations to drop
# RL was abandoned w/o measurement; others are check plots
drp <- c("TCCheck","LRCheck","BCCheck","BBCheck","RL")
# DC gives only 1 yr of growth data, CT only 2 years, before destroyed
drp <- c(drp, "DC","CT")



