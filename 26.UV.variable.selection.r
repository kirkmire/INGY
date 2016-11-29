
######Understory Non-Tree Variables######
#Percentage cover 1m, upper and lower extent of height




#Selecting installations 
veg<-c("EM","BC","TJ","RM","CM","TC")

annual.gr3<-annual.gr[annual.gr$Installation %in% sim, ]


#Reshape 1 meter small tree plot veg measurements so that each stp record year has its
#own associated Forb, Grass, Low Shrub, High Shrub and Polyveg measurements

stp<-reshape(sstp1, direction="wide",idvar=
                     c("Installation","Plot","STP","Year_Measurement"),
                   timevar="Lifeform",v.names=c("Coverage","Base","Top"))

#Merge plot history and stp 1m veg

veg_record<- merge(splot, stp,by=c("Installation","Plot"))


#Merge annual small tree growth with veg_record for each year

veg_record1<- merge(annual.gr3, veg_record,by=c("Installation","Plot","STP","Year_Measurement"))



