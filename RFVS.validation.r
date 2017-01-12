#Subset growth increment dataframe to only include the relevant KC trees


KC.FVS.Tree.Data<-annual.gr4[which(annual.gr4$Installation=="KC"),]


KC.FVS.Tree.Data<-KC.FVS.Tree.Data[which(FVS.Tree.Data$crown.ratio>0),]

#make id column in KC.FVS.Data
KC.FVS.Tree.Data$Plot<-paste(KC.FVS.Tree.Data$Installation,
                             KC.FVS.Tree.Data$Plot,
                             KC.FVS.Tree.Data$STP,
                             KC.FVS.Tree.Data$Year_Measurement,
                             KC.FVS.Tree.Data$Tree,
                             sep="")



KC.FVS.Tree.Data <- transform(KC.FVS.Tree.Data,Plot=as.numeric(factor(Plot)))


#Merge with FVS tree list final now that the same lengths

merge(KC.FVS.Tree.Data,tree.lists.final)
