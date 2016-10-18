
histogram(~Species|Installation,data=stag)

barchart(Species~Freq|Installation,data=stag)

stag$count<-1

barchart(xtabs(count~Installation+Species, data=stag),stack=T,auto.key=T,
         par.settings=list(superpose.polygon=list(col=rainbow(12))))
         

spec.freq.table<-xtabs(count~Installation+Species, data=stag)
spec.freq.table<-as.data.frame(spec.freq.table)

pi<-spec.freq.table[spec.freq.table$Species=="PIPO",]
gt100<-pi[pi$Freq>100,]
gt60<-pi[pi$Freq>60,]

#There are only 9 installations with more than 100 small tree subjects 
#at initiation

#There are 15 installations with more than 60 small tree subjects 
#at initiation (10 avg per plot)