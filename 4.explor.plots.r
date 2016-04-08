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
stag.sp.inst<-aggregate(stag$numb~stag$Installation+stag$Species, data=stag,sum, na.rm=TRUE)

#qplot(factor(Installation), data=stag[!(stag$Installation %in% drp),], geom="bar", fill=(factor(Species),values=cbPalette))#



ggplot(data=stag.sp.inst, aes(x=stag$Installation, y=stag$numb)) + 
                   geom_bar(aes(fill=stag$Species),stat="identity")
           
           
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


BAPA.by.inst<-ddply(OverS, "Installation", summarise, BAPA.inst= sum(BAPA))

#Bar Chart of Initial BAPA by Installation, grouped by plot#

barchart(BAPA~Installation,data=BAPA.by.plot[!(BAPA.by.plot$Installation %in% drp),],groups=Plot)

#Bar chart of Initial Average BAPA by Installation#

BAPA.by.inst$ave.BAPA<-BAPA.by.inst$BAPA.inst/7

barchart(ave.BAPA~Installation, data=BAPA.by.inst[!(BAPA.by.inst$Installation %in% drp),])

#Aggregate small trees by inst and year meas#

stc = aggregate(sstpt$Count,list(sstpt$Installation,sstpt$Year_Measurement), sum)

#Include plot , then average accros#
st.count<- setNames(stc, c("Installation","Year","Count"))

#Split the df by Installation and Min Year Measurement#

minyr.st.count<-do.call(rbind,lapply(split(st.count,st.count$Installation),function(chunk) chunk[which.min(chunk$Year),]))

#Per Acre Count of Small Trees by Installation in First Year Measurement#

barchart((Count/.294)~Installation,data=minyr.st.count[!(minyr.st.count$Installation %in% drp),])
