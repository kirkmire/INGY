###GLM Take1###

library(gam)

#Selecting installations of similar overstory basal area and SI
sim<-c("EM","BC","TJ","RM","CM","TC")

annual.gr<-annual.gr[annual.gr$Installation %in% sim, ]

#Exclude the 6th plots of each installation
annual.gr<-annual.gr[! annual.gr$Plot==6,]



gam(annual.gr$ht_annual~sqrt(annual.gr$Height_Total)+smooth(annual.gr$#TPA))