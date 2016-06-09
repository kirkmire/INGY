###
source('C:/Users/Colin/Desktop/R-Projects/INGY/1.readdatabase.2016jun2.r', echo=F)

merged_stagm_stag <- merge(stagm, stag,by=c("Installation","Plot","STP","Tree"))


LL<-merged_stagm_stag[merged_stagm_stag$Installation=="LL"&merged_stagm_stag$Species=="PIPO"&merged_stagm_stag$Plot%in%c(1:7)&merged_stagm_stag$Year_Measurement%in%c(1999,2001,2004,2008,2012),]


#removing certain trees
#Appears to be errors in data entry
#Tree 663 goes from 8.5ft to 1.6ft, should read 9.6ft
LL$Height_Total[LL$Year_Measurement==2008&LL$Plot==1&LL$Tree==663]<-9.6
#Tree 53 goes from 8.6ft to 1.5ft, should read 9.5ft
LL$Height_Total[LL$Plot==3&LL$Tree==53&LL$Year_Measurement==2008]<-9.5
#Tree 19, Plot 3 has Height total of 0 for 2008 measurement, removed#
LL<-LL[!(LL$Plot=="3"&LL$Tree=="19"),]
#Tree 242, Plot 6 has Height total of 0 for 2008 measurement, removed#
LL<-LL[!(LL$Plot=="6"&LL$Tree=="242"),]
#tree 683 plot 4 stp 5#
LL<-LL[!(LL$Plot=="4"&LL$Tree=="683"),]
#tree 111 plot 1 stp 6#
LL<-LL[!(LL$Plot=="1"&LL$Tree=="111"),]



LL1_init<-(LL[LL$Year_Measurement=="1999",])



LL1_both<-merge(LL1_init,LL, by=c("Plot","STP","Tree"))

LL1_both$inc<-LL1_both$Height_Total.y-LL1_both$Height_Total.x




#Merge with splot for treatment column#
LL1_both<-merge(LL1_both,splot,by=c("Installation","Plot"))
LL1_both<- LL1_both[!(is.na(LL1_both$BasalDiameter.x)) ,]
LL1_both<- LL1_both[!(is.na(LL1_both$BasalDiameter.y)) ,]

boxplot(LL1_both$inc)


###Plotting
library(ggplot2)


tbp<-theme(                              
  axis.title.x = element_text(face="bold", color="black", size=16),
  axis.title.y = element_text( face="bold",color="black", size=16),
  plot.title = element_text(face="bold", color = "black", size=18), 
  legend.position="bottom",axis.title.y=element_text(vjust=0.8))

lllp<-ggplot(data=LL1_both, aes(x=Year_Measurement.y, y=inc,color=LL1_both$Height_Total.x))+geom_point()+
  labs(title="LL Tree H-H Inc over Meas Years",y="Height-Init Height (ft)", x = "Measurement Year")+ 
  scale_fill_brewer(palette="Dark2") +tbp+ geom_line(aes(group=LL1_both$Tree))+ylim(0,20)
lllp+scale_colour_gradient(limits=c(0, 15), low="red", high="green")


#For coloring by quantiles of height growth difference from initial#

qfun <- function(x, q = 4) {
  quantile <- cut(x, breaks = quantile(x, probs = 0:q/q), 
                  include.lowest = TRUE, labels = 1:q)
  quantile
}




LL2012<-LL1_both[LL1_both$Year_Measurement.y=="2012",]

LL2012$quant <- qfun(LL2012$Height_Total.y)

mk4 <- within(LL1_both, quanti <- totalPr - shipPr)

merge(LL2012,LL1_both,by=c("Plot","Installation","STP","Tree"))

lllp<-ggplot(data=LL1_both, aes(x=Year_Measurement.y, y=inc,color= factor(LL1_both$Quant)))+geom_point()+
  labs(title="LL Tree H-H Inc over Meas Years",y="Height-Init Height (ft)", x = "Measurement Year")+ 
  scale_fill_brewer(palette="Dark2") +tbp+ geom_line(aes(group=LL1_both$Tree))+ylim(0,20)

lllp+scale_colour_manual(values = c("black", "blue", "red","green"), 
                    labels = c("0-25", "25-50", "50-75","75-100")) 








                 



