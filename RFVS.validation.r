#Subset growth increment dataframe to only include the relevant KC trees


KC.FVS.Tree.Data<-annual.gr4[which(annual.gr4$Installation=="KC"),]


KC.FVS.Tree.Data<-KC.FVS.Tree.Data[
  which((100*(KC.FVS.Tree.Data$Height_Total-KC.FVS.Tree.Data$Height_CrownBase)/KC.FVS.Tree.Data$Height_Total)>0),]

#make id column in KC.FVS.Data
KC.FVS.Tree.Data$Plot<-paste(KC.FVS.Tree.Data$Plot,KC.FVS.Tree.Data$Year_Measurement,sep="")

KC.FVS.Tree.Data$Tree<-paste(
           #KC.FVS.Tree.Data$Plot,
           KC.FVS.Tree.Data$STP,
           #KC.FVS.Tree.Data$Year_Measurement,
           KC.FVS.Tree.Data$Tree,sep="")


#Have to abbreviate tree and plot info
#KC.FVS.Tree.Data <- transform(KC.FVS.Tree.Data,Tree=as.numeric(factor(Tree)))
KC.FVS.Tree.Data <- transform(KC.FVS.Tree.Data,Plot=as.numeric(factor(Plot)))


KC.FVS.Tree.Data$Tree<-paste(KC.FVS.Tree.Data$Plot,KC.FVS.Tree.Data$Tree,sep="")

tree.lists.final$Tree<-paste(tree.lists.final$plot,tree.lists.final$id,sep="")


#Merge with FVS tree list final now that the same lengths

FVS.Final<-merge(KC.FVS.Tree.Data,tree.lists.final,by="Tree")

