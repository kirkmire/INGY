###

LL1<-merged_stagm_stag[merged_stagm_stag$Installation=="LL"&merged_stagm_stag$Species=="PIPO"&merged_stagm_stag$Plot%in%c(1:7)&merged_stagm_stag$Year_Measurement%in%c(1999,2001,2004,2008,2012),]


LL1_init<-aggregate(LL1$Height_Total ~ LL1$Tree+LL1$Plot+LL1$STP, LL1, function(x) min(x))

names(LL1_init)[1:4]<-c("Tree","Plot","STP","Height_Total")

LL1_both<-merge(LL1_init,LL1, by=c("Plot","STP","Tree"))

LL1_both$inc<-LL1_both$Height_Total.y-LL1_both$Height_Total.x

#Merge with splot for treatment column#
LL1_both<-merge(LL1_both,splot,by=c("Installation","Plot"))

#Create Column with growth increment#
LL1_both$inc<-EM_both$Height_Total.y-EM_both$Height_Total.x

boxplot(LL1_both$inc)

###Plotting

tbp<-theme(                              
  axis.title.x = element_text(face="bold", color="black", size=16),
  axis.title.y = element_text( face="bold",color="black", size=16),
  plot.title = element_text(face="bold", color = "black", size=18), 
  legend.position="bottom",axis.title.y=element_text(vjust=0.8))

lllp<-ggplot(data=LL1_both, aes(x=Year_Measurement, y=inc,color=Treatment))+geom_point()+
  labs(title="LL Tree Height Inc over Meas Years",y="Height (ft)", x = "Measurement Year")+ 
  scale_fill_brewer(palette="Dark2") +tbp+ geom_line(aes(group=LL1_both$Tree))+ylim(0,10)
lllp



                 



