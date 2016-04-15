#! 2. installations to drop
# RL was abandoned w/o measurement; others are check plots
drp <- c("TCCheck","LRCheck","BCCheck","BBCheck","RL")
# DC gives only 1 yr of growth data, CT only 2 years, before destroyed
drp <- c(drp, "DC","CT")

stag<-stag[!(stag$Installation %in% drp),]
#Colorstuff#
library(RColorBrewer)

#Lookat all colors#
display.brewer.all(n=NULL, type="all", select=NULL, exact.n=TRUE,
                   colorblindFriendly=T)
# The brewing#
cbPalette <- brewer.pal(11,"RdYlBu")

#Frequeny Plot by Species#

library(plyr)
library(ggplot2)

speciescount<-data.frame(count(stag$Species))

spec.freq <- ggplot(speciescount, aes(x=factor(x),y=freq))+geom_bar(stat="identity",fill=cbPalette)
spec.freq

#Stacked Bar Chart by Installation and Species#

stag$numb<-1
stag.sp.inst<-aggregate(numb~Installation+Species, data=stag,sum, na.rm=TRUE)

#qplot(factor(Installation), data=stag[!(stag$Installation %in% drp),], geom="bar", fill=(factor(Species),values=cbPalette))#

#For ordering installations by xcoord#

locdata <- sinstloc[sinstloc$Coordinate_Type=="DD"& sinstloc$Plot=='0',]
locdataX <- locdata[locdata$Coordinate_Axis=='X',]


#Merge w location info#
stag.sp.inst<-merge(x=locdataX, y=stag.sp.inst, by.x="Installation", by.y="Installation")

#order by location info#
stag.sp.inst<-stag.sp.inst[order(stag.sp.inst$Coordinate_Value),]


theme(axis.line=element_blank(),
      axis.text.x=element_blank(),
      axis.text.y=element_blank(),
      axis.ticks=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank(),
      legend.position="none",
      panel.background=element_blank(),
      panel.border=element_blank(),
      panel.grid.major=element_blank(),
      panel.grid.minor=element_blank(),
      plot.background=element_blank())


ggplot(data=stag.sp.inst, aes(x=factor(Coordinate_Value), y=numb)) + 
                   geom_bar(aes(fill=Species),stat="identity")+ylab("Number")+xlab("Installation")+
  scale_fill_manual(values=cbPalette)+theme(text = element_text(size=20),axis.text.x=element_blank())
           
           
#Overstory BA/Ac by Installation (Year of First OS Measurement)#

soverhist2 <- data.table(soverhist, key=c("Installation","Plot","Tree"))
test<-soverhist2[, list(value=min("Year_Measurement")), by=c("Installation","Plot","Tree")]

library(plyr)
library(data.table)
library(lattice)

soverhist5 = data.table(soverhist)

min.OS<-soverhist5[,min(Year_Measurement),by=c("Installation","Plot","Tree")]

setnames(min.OS, "V1", "Year_Measurement")
min.OS

OverS <- merge(min.OS, soverhist5,by=c("Installation","Plot","Tree","Year_Measurement"))

OverS$BAPA<-ifelse(OverS$DBH<10.5,((OverS$DBH^2)*.005454)/.26,((OverS$DBH^2)*.005454)/.46) 




BAPA.by.plot<-aggregate(OverS$BAPA, by=list(Installation=OverS$Installation,Plot=OverS$Plot), FUN=sum)
setnames(BAPA.by.plot, "x", "BAPA")



sum(OverS$BAPA[OverS$Installation=="KC"])

#Bar Chart of Initial BAPA by Installation, grouped by plot#

barchart(BAPA~Installation,data=BAPA.by.plot[!(BAPA.by.plot$Installation %in% drp),],groups=Plot)

#ggplot(data=BAPA.by.plot[!(BAPA.by.plot$Installation %in% drp),], aes(x=Installation, y=BAPA)) + 
#  geom_bar(aes(fill=BAPA),   
 #          stat="identity",
 #          colour="black",    # Black outline for all
 #          position="dodge"+ylab("BAPA")+xlab("Installation")+
#theme(text = element_text(size=20))#


plotcolor<-brewer.pal(22,"RdYlBu")

bapa.drp<-BAPA.by.plot[!(BAPA.by.plot$Installation %in% drp),]


ggplot(data=bapa.drp, aes(x=Installation,y=BAPA,fill=factor(Plot)))
+ geom_bar(stat="identity",position="dodge",colour="black")
+
  scale_fill_discrete(name="Plot",breaks=c(1, 2),values=plotcolor)+xlab("Installation")+ylab("Retained Basal Area Per Acre")+
  theme(text = element_text(size=20))

#Bar chart of Initial Average BAPA by Installation#
BAPA.by.inst<-ddply(OverS, "Installation", summarise, BAPA.inst= sum(BAPA))

BAPA.by.inst$ave.BAPA<-BAPA.by.inst$BAPA.inst/7

barchart(ave.BAPA~Installation, data=BAPA.by.inst[!(BAPA.by.inst$Installation %in% drp),])

bapa.ave.inst<-BAPA.by.inst[!(BAPA.by.inst$Installation %in% drp),]
                            
ggplot(data=bapa.ave.inst, aes(x=factor(Installation), y=ave.BAPA))+
  geom_bar(stat="identity",position="dodge")+ylab("Average BAPA ")+xlab("Installation")+
  theme(text = element_text(size=20))



  geom_bar(aes(fill=cond2),   # fill depends on cond2
           stat="identity",
           colour="black",    # Black outline for all
           position=position_dodge()) 

#Aggregate small trees by inst and year meas#

stc = aggregate(sstpt$Count,list(sstpt$Installation,sstpt$Year_Measurement), sum)



#Include plot , then average accros#
st.count<- setNames(stc, c("Installation","Year","Count"))

#Split the df by Installation and Min Year Measurement#

minyr.st.count<-do.call(rbind,lapply(split(st.count,st.count$Installation),function(chunk) chunk[which.min(chunk$Year),]))

#Per Acre Count of Small Trees by Installation in First Year Measurement#

barchart((Count/.294)~Installation,data=minyr.st.count[!(minyr.st.count$Installation %in% drp),])
