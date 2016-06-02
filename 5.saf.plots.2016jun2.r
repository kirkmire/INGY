###BB Plot###########################################################################

BB1 <- stagm[stagm$Installation=='BB'& stagm$Plot=="1",]
BB1$prevyear <- 0 + 1999*(BB1$Year_Measurement==2000)+ 
  2000*(BB1$Year_Measurement==2002)+ 
  2002*(BB1$Year_Measurement==2004)+
  2004*(BB1$Year_Measurement==2008)+
 2008*(BB1$Year_Measurement==2012)
inc.dataBB1 <- merge(BB1[,c(2,3,4,5,6,17,10)],
                  BB1[,c(2,3,4,5,6,10)],
                  by.x=c("Installation","Plot","STP","Tree","prevyear"),
                  by.y=c("Installation","Plot","STP","Tree","Year_Measurement"))

BB4 <- stagm[stagm$Installation=='BB'& stagm$Plot=="4",]
BB4$prevyear <- 0 + 1999*(BB4$Year_Measurement==2000)+ 
  2000*(BB4$Year_Measurement==2002)+ 
  2002*(BB4$Year_Measurement==2004)+
  2004*(BB4$Year_Measurement==2008)+
  2008*(BB4$Year_Measurement==2012) 

inc.dataBB4 <- merge(BB4[,c(2,3,4,5,6,17,10)],
                  BB4[,c(2,3,4,5,6,10)],
                  by.x=c("Installation","Plot","STP","Tree","prevyear"),
                  by.y=c("Installation","Plot","STP","Tree","Year_Measurement"))

BB1tags<- stag[stag$Installation=='BB'& stag$Plot=="1",]

BB4tags<- stag[stag$Installation=='BB'& stag$Plot=="4",]


mBB1<- merge(inc.dataBB1, BB1tags, by.x = "Tree", by.y = "Tree",all=F)
mBB4<-merge(inc.dataBB4, BB4tags, by.x = "Tree", by.y = "Tree",all=F)

BB1x<-rbind.data.frame(mBB1,mBB4)
####################################################################################
BB5 <- stagm[stagm$Installation=='BB'& stagm$Plot=="5",]
BB5$prevyear <- 0 + 1999*(BB5$Year_Measurement==2000)+
  2000*(BB5$Year_Measurement==2002)+ 
  2002*(BB5$Year_Measurement==2004)+
  2004*(BB5$Year_Measurement==2008)+
  2008*(BB5$Year_Measurement==2012)

inc.dataBB5 <- merge(BB5[,c(2,3,4,5,6,17,10)],
                   BB5[,c(2,3,4,5,6,10)],
                   by.x=c("Installation","Plot","STP","Tree","prevyear"),
                   by.y=c("Installation","Plot","STP","Tree","Year_Measurement"))

BB5tags<- stag[stag$Installation=='BB'& stag$Plot=="5",]

mBB5 <- merge(inc.dataBB5, BB5tags, by.x = "Tree", by.y = "Tree",all=F)

BB7 <- stagm[stagm$Installation=='BB'& stagm$Plot=="7",]
BB7$prevyear <- 0 + 1999*(BB7$Year_Measurement==2000)+
  2000*(BB7$Year_Measurement==2002)+ 
  2002*(BB7$Year_Measurement==2004)+
  2004*(BB7$Year_Measurement==2008)+
  2008*(BB7$Year_Measurement==2012)

inc.dataBB7 <- merge(BB7[,c(2,3,4,5,6,17,10)],
                   BB7[,c(2,3,4,5,6,10)],
                   by.x=c("Installation","Plot","STP","Tree","prevyear"),
                   by.y=c("Installation","Plot","STP","Tree","Year_Measurement"))

BB7tags<- stag[stag$Installation=='BB'& stag$Plot=="7",]

mBB7 <- merge(inc.dataBB7, BB7tags, by.x = "Tree", by.y = "Tree",all=F)


BBCTRL<-rbind.data.frame(mBB7,mBB5)

             
plot1<-rbind.data.frame(BB1x,BBCTRL)
plot1$dif<-(plot1$Height_Total.x- plot1$Height_Total.y)

finalData<-subset(plot1,!(is.na(plot1$dif)))
finalData$Plot.x[finalData$Plot.x=="1"] <- "One Time"
finalData$Plot.x[finalData$Plot.x=="4"] <- "One Time"
finalData$Plot.x[finalData$Plot.x=="5"] <- "Control"
finalData$Plot.x[finalData$Plot.x=="7"] <- "Control"

finalData$Year <- as.factor(finalData$Year_Measurement)
finalData$plo <- as.factor(finalData$Plot.x)

BBfinalyear<-finalData
[finalData$Year_Measurement=='2008',]

psmedata<-BBfinalyear[BBfinalyear$Species=='PSME',]


xyplot(I(dif) ~ Height_Total.x, data=psmedata,colour=psmedata$Plot.x,
       type=c("p","smooth"),groups=psmedata$Plot.x, ylab="Measurement Year Height-Previous Year Height (ft.)",
       xlab="Measurement Year Height (ft)", main="Height Growth From Previous Year vs. Measurement Year Height (ft)",
       pch=4,auto.key=T)

tbp<-theme(                              
  axis.title.x = element_text(face="bold", color="black", size=16),
  axis.title.y = element_text( face="bold",color="black", size=16),
  plot.title = element_text(face="bold", color = "black", size=18), 
  legend.position="bottom",axis.title.y=element_text(vjust=0.8))

library(ggplot2)

bblp<-ggplot(data=psmedata, aes(x=Year_Measurement, y=(Height_Total.x*.3048),col=plo))+geom_point() +
  scale_fill_brewer(palette="Dark2") +tbp+labs(title="High Productivity/Low Overstory Site (BB)",y="Height (m)", x = "Measurement Year")+ 
  scale_fill_brewer(palette="Dark2") +tbp+ geom_line(aes(group = Tree))+ylim(0,10)
bblp

bblp<-ggplot(data=BBfinalyear, aes(y=((Height_Total.x-Height_Total.y)*.3048), x=(Height_Total.x*.3048),col=plo))+geom_smooth(method=lm) +
  scale_fill_brewer(palette="Dark2") +tbp+labs(title="Height vs. Year of Measurement",x="Height (m)", y = "Height Growth Increment")+ 
  scale_fill_brewer(palette="Dark2") +tbp+geom_point()+ylim(0,4)+xlim(0,12)
bblp



bbbp<-ggplot(data=BBfinalyear, aes(x=Year, y=(Height_Total.x*.3048),fill=plo))+
  geom_boxplot(position=position_dodge())+
  labs(title="Height Growth for High Productivity/Low Overstory Site (BB)",x="Measurement Year", y = "Height (m)")+ scale_fill_brewer(palette="Dark2") +tbp+ylim(0,10)
bbbp


#####################KEIFFER CABIN################################
KC3 <- stagm[stagm$Installation=='KC'& stagm$Plot=='3',]
KC3$prevyear <- 0 + 2001*(KC3$Year_Measurement==2002) +
  2002*(KC3$Year_Measurement==2003) + 
  2003*(KC3$Year_Measurement==2006) +
  2006*(KC3$Year_Measurement==2010) 


inc.dataKC3 <- merge(KC3[,c(2,3,4,5,6,17,10)],
                    KC3[,c(2,3,4,5,6,10)],
                    by.x=c("Installation","Plot","STP","Tree","prevyear"),
                    by.y=c("Installation","Plot","STP","Tree","Year_Measurement"))

KC6 <- stagm[stagm$Installation=='KC'& stagm$Plot=='6',]
KC6$prevyear <- 0 + 2001*(KC6$Year_Measurement==2002) +
  2002*(KC6$Year_Measurement==2003) + 
  2003*(KC6$Year_Measurement==2006) +
  2006*(KC6$Year_Measurement==2010) 

inc.dataKC6 <- merge(KC6[,c(2,3,4,5,6,17,10)],
                     KC6[,c(2,3,4,5,6,10)],
                     by.x=c("Installation","Plot","STP","Tree","prevyear"),
                     by.y=c("Installation","Plot","STP","Tree","Year_Measurement"))

KC3tags<- stag[stag$Installation=='KC'& stag$Plot=='3',]

KC6tags<- stag[stag$Installation=='KC'& stag$Plot=='6',]



mKC3 <- merge(inc.dataKC3, KC3tags, by.x = "Tree", by.y = "Tree",all=F)
mKC6 <- merge(inc.dataKC6, KC6tags, by.x = "Tree", by.y = "Tree",all=F)

KC1x<-rbind.data.frame(mKC3,mKC6)

##############################################################################
KC7 <- stagm[stagm$Installation=='KC'& stagm$Plot=='7',]
KC7$prevyear <- 0 + 2001*(KC7$Year_Measurement==2002) +
  2002*(KC7$Year_Measurement==2003) + 
  2003*(KC7$Year_Measurement==2006) +
  2006*(KC7$Year_Measurement==2010) 


inc.dataKC7 <- merge(KC7[,c(2,3,4,5,6,17,10)],
                    KC7[,c(2,3,4,5,6,10)],
                    by.x=c("Installation","Plot","STP","Tree","prevyear"),
                    by.y=c("Installation","Plot","STP","Tree","Year_Measurement"))


KC1 <- stagm[stagm$Installation=='KC'& stagm$Plot=='1',]
KC1$prevyear <- 0 + 2001*(KC1$Year_Measurement==2002) +
  2002*(KC1$Year_Measurement==2003) + 
  2003*(KC1$Year_Measurement==2006) +
  2006*(KC1$Year_Measurement==2010) 


inc.dataKC1 <- merge(KC1[,c(2,3,4,5,6,17,10)],
                    KC1[,c(2,3,4,5,6,10)],
                    by.x=c("Installation","Plot","STP","Tree","prevyear"),
                    by.y=c("Installation","Plot","STP","Tree","Year_Measurement"))

KC7tags<- stag[stag$Installation=='KC'& stag$Plot=='7',]

KC1tags<- stag[stag$Installation=='KC'& stag$Plot=='1',]


mKC7 <- merge(inc.dataKC7, KC7tags, by.x = "Tree", by.y = "Tree",all=F)
mKC1 <- merge(inc.dataKC1, KC1tags, by.x = "Tree", by.y = "Tree",all=F)
KCCTRL<-rbind.data.frame(mKC1,mKC7)

###

plot2<-rbind.data.frame(KC1x,KCCTRL)
plot2$dif<-(plot2$Height_Total.x- plot2$Height_Total.y)

finalData2<-subset(plot2,!(is.na(plot2$dif)))
finalData2$Plot.x[finalData2$Plot.x=="3"] <- "One Time"
finalData2$Plot.x[finalData2$Plot.x=="6"] <- "One Time"
finalData2$Plot.x[finalData2$Plot.x=="1"] <- "Control"
finalData2$Plot.x[finalData2$Plot.x=="7"] <- "Control"

finalData2$Year <- as.factor(finalData2$Year_Measurement)
finalData2$plo <- as.factor(finalData2$Plot.x)


KCfinalyear<-finalData2

[finalData2$Year_Measurement=='2010',]

psmedataKC<-KCfinalyear[KCfinalyear$Species=='PSME',]


tbp<-theme(                              
  axis.title.x = element_text(face="bold", color="black", size=16),
  axis.title.y = element_text( face="bold",color="black", size=16),
  plot.title = element_text(face="bold", color = "black", size=18), 
  legend.position="bottom",axis.title.y=element_text(vjust=0.8))

library(ggplot2)

kclp<-ggplot(data=psmedataKC, aes(x=Year, y=(Height_Total.x*.3048),col=plo))+geom_point()+stat_smooth(method=lm)+
  scale_fill_brewer(palette="Dark2") +tbp+
  labs(title="Low Productivity/High Overstory Site (KC)",x="Year", y = "Height")+ 
  scale_fill_brewer(palette="Dark2") +tbp+ylim(0,10)+ geom_line(aes(group = Tree))
kclp

kclp<-ggplot(data=psmedataKC, aes(y=((Height_Total.x-Height_Total.y)*.3048), x=(Height_Total.x*.3048),col=plo))+geom_smooth(method=lm) +
  scale_fill_brewer(palette="Dark2") +tbp+labs(title="Low Productivity/High Overstory Site (KC)",x="Height (m)", y = "Height Growth Increment")+ 
  scale_fill_brewer(palette="Dark2") +tbp+geom_point()+ylim(0,4)+xlim(0,12)
kclp

kcbp<-ggplot(data=KCfinalyear, aes(x=Year, y=(Height_Total.x*.3048),fill=plo))+
  geom_boxplot(position=position_dodge())+
  labs(title="Height Growth for Low Productivity/High Overstory Site (KC)",x="Measurement Year", y = "Height (m)")+ scale_fill_brewer(palette="Dark2") +tbp+ylim(0,10 )
kcbp




###########################################FOR EM#############################################

EM3 <- stagm[stagm$Installation=='EM'& stagm$Plot=='3',]
EM3$prevyear <- 0 + 2000*(EM3$Year_Measurement==2001) +
  2001*(EM3$Year_Measurement==2002) +
  2002*(EM3$Year_Measurement==2003) + 
  2003*(EM3$Year_Measurement==2006) +
  2006*(EM3$Year_Measurement==2010) 


inc.dataEM3 <- merge(EM3[,c(2,3,4,5,6,17,10)],
                     EM3[,c(2,3,4,5,6,10)],
                     by.x=c("Installation","Plot","STP","Tree","prevyear"),
                     by.y=c("Installation","Plot","STP","Tree","Year_Measurement"))


EM7 <- stagm[stagm$Installation=='EM'& stagm$Plot=='7',]
EM7$prevyear <- 0 + 2000*(EM7$Year_Measurement==2001) +
  2001*(EM7$Year_Measurement==2002) +
  2002*(EM7$Year_Measurement==2003) + 
  2003*(EM7$Year_Measurement==2006) +
  2006*(EM7$Year_Measurement==2010) 


inc.dataEM7 <- merge(EM7[,c(2,3,4,5,6,17,10)],
                     EM7[,c(2,3,4,5,6,10)],
                     by.x=c("Installation","Plot","STP","Tree","prevyear"),
                     by.y=c("Installation","Plot","STP","Tree","Year_Measurement"))

EM3tags<- stag[stag$Installation=='EM'& stag$Plot=='3',]

EM7tags<- stag[stag$Installation=='EM'& stag$Plot=='7',]


mEM3 <- merge(inc.dataEM3, EM3tags, by.x = "Tree", by.y = "Tree",all=F)
mEM7 <- merge(inc.dataEM7, EM7tags, by.x = "Tree", by.y = "Tree",all=F)
EM1x<-rbind.data.frame(mEM3,mEM7)

##################

EM2 <- stagm[stagm$Installation=='EM'& stagm$Plot=='2',]
EM2$prevyear <- 0 + 2000*(EM2$Year_Measurement==2001) +
  2001*(EM2$Year_Measurement==2002) +
  2002*(EM2$Year_Measurement==2003) + 
  2003*(EM2$Year_Measurement==2006) +
  2006*(EM2$Year_Measurement==2010) 


inc.dataEM2 <- merge(EM2[,c(2,3,4,5,6,17,10)],
                     EM2[,c(2,3,4,5,6,10)],
                     by.x=c("Installation","Plot","STP","Tree","prevyear"),
                     by.y=c("Installation","Plot","STP","Tree","Year_Measurement"))


EM6 <- stagm[stagm$Installation=='EM'& stagm$Plot=='6',]
EM6$prevyear <- 0 + 2000*(EM6$Year_Measurement==2001) +
  2001*(EM6$Year_Measurement==2002) +
  2002*(EM6$Year_Measurement==2003) + 
  2003*(EM6$Year_Measurement==2006) +
  2006*(EM6$Year_Measurement==2010) 

inc.dataEM6 <- merge(EM6[,c(2,3,4,5,6,17,10)],
                     EM6[,c(2,3,4,5,6,10)],
                     by.x=c("Installation","Plot","STP","Tree","prevyear"),
                     by.y=c("Installation","Plot","STP","Tree","Year_Measurement"))

EM2tags<- stag[stag$Installation=='EM'& stag$Plot=='2',]

EM6tags<- stag[stag$Installation=='EM'& stag$Plot=='6',]


mEM2 <- merge(inc.dataEM2, EM2tags, by.x = "Tree", by.y = "Tree",all=F)
mEM6 <- merge(inc.dataEM6, EM6tags, by.x = "Tree", by.y = "Tree",all=F)
EMCTRL<-rbind.data.frame(mEM2,mEM6)
finalData3<-rbind.data.frame(EM1x,EMCTRL)
###

finalData3<-subset(plot3,!(is.na(plot3$dif)))
finalData3$Plot.x[finalData3$Plot.x=="3"] <- "One Time"
finalData3$Plot.x[finalData3$Plot.x=="7"] <- "One Time"
finalData3$Plot.x[finalData3$Plot.x=="2"] <- "Control"
finalData3$Plot.x[finalData3$Plot.x=="6"] <- "Control"

finalData3$Year <- as.factor(finalData3$Year_Measurement)
finalData3$plo <- as.factor(finalData3$Plot.x)

EMfinalyear<-finalData3

[finalData3$Year_Measurement=='2006',]

psmedataEM<-EMfinalyear[EMfinalyear$Species=='PSME',]


tbp<-theme(                              
  axis.title.x = element_text(face="bold", color="black", size=16),
  axis.title.y = element_text( face="bold",color="black", size=16),
  plot.title = element_text(face="bold", color = "black", size=18), 
  legend.position="bottom",axis.title.y=element_text(vjust=0.8))

library(ggplot2)

emlp<-ggplot(data=psmedataEM, aes(x=(dif*.3048), y=(Height_Total.x*.3048),col=plo))+geom_point()+stat_smooth(method=lm)+
  scale_fill_brewer(palette="Dark2") +tbp+labs(title="Height vs. Height Growth Increment",y="Height at 2006 (m)", x = "Height Growth Increment (M)")+ scale_fill_brewer(palette="Dark2") +tbp
emlp

emlp<-ggplot(data=psmedataEM, aes(x=Year, y=(Height_Total.x*.3048),col=plo))+geom_point()+stat_smooth(method=lm)+
  scale_fill_brewer(palette="Dark2") +tbp+
  labs(title="Height vs. Year of Measurement (EM)",x="Year", y = "Height")+ 
  scale_fill_brewer(palette="Dark2") +tbp+ylim(0,10)+ geom_line(aes(group = Tree))
emlp



embp<-ggplot(data=EMfinalyear, aes(x=Year, y=(Height_Total.x*.3048),fill=plo))+
  geom_boxplot(position=position_dodge())+
  labs(title="Height Growth for Low Productivity/Low Overstory Site (EM)",x="Measurement Year", y = "Height (m)")+ scale_fill_brewer(palette="Dark2") +tbp+ylim(0,10)
embp

############################Plot SG#############################
SG1 <- stagm[stagm$Installation=='SG'& stagm$Plot=='1',]
SG1$prevyear <- 0 + 1998*(SG1$Year_Measurement==2000) +
  2000*(SG1$Year_Measurement==2003) + 
  2003*(SG1$Year_Measurement==2006) +
  2006*(SG1$Year_Measurement==2010)


inc.dataSG1 <- merge(SG1[,c(2,3,4,5,6,17,10)],
                     SG1[,c(2,3,4,5,6,10)],
                     by.x=c("Installation","Plot","STP","Tree","prevyear"),
                     by.y=c("Installation","Plot","STP","Tree","Year_Measurement"))


SG7 <- stagm[stagm$Installation=='SG'& stagm$Plot=='7',]
SG7$prevyear <- 0 + 1998*(SG7$Year_Measurement==2000) +
  2000*(SG7$Year_Measurement==2003) + 
  2003*(SG7$Year_Measurement==2006) +
  2006*(SG7$Year_Measurement==2010)


inc.dataSG7 <- merge(SG7[,c(2,3,4,5,6,17,10)],
                     SG7[,c(2,3,4,5,6,10)],
                     by.x=c("Installation","Plot","STP","Tree","prevyear"),
                     by.y=c("Installation","Plot","STP","Tree","Year_Measurement"))

SG1tags<- stag[stag$Installation=='SG'& stag$Plot=='1',]

SG7tags<- stag[stag$Installation=='SG'& stag$Plot=='7',]


mSG1 <- merge(inc.dataSG1, SG1tags, by.x = "Tree", by.y = "Tree",all=F)
mSG7 <- merge(inc.dataSG7, SG7tags, by.x = "Tree", by.y = "Tree",all=F)
SG1x<-rbind.data.frame(mSG1,mSG7)

##################

SG5 <- stagm[stagm$Installation=='SG'& stagm$Plot=='5',]
SG5$prevyear <- 0 + 1998*(SG5$Year_Measurement==2000) +
  2000*(SG5$Year_Measurement==2003) + 
  2003*(SG5$Year_Measurement==2006) +
  2006*(SG5$Year_Measurement==2010) 


inc.dataSG5 <- merge(SG5[,c(2,3,4,5,6,17,10)],
                     SG5[,c(2,3,4,5,6,10)],
                     by.x=c("Installation","Plot","STP","Tree","prevyear"),
                     by.y=c("Installation","Plot","STP","Tree","Year_Measurement"))


SG3 <- stagm[stagm$Installation=='SG'& stagm$Plot=='3',]
SG3$prevyear <- 0 + 1998*(SG3$Year_Measurement==2000) +
  2000*(SG3$Year_Measurement==2003) + 
  2003*(SG3$Year_Measurement==2006) +
  2006*(SG3$Year_Measurement==2010) 

inc.dataSG3 <- merge(SG3[,c(2,3,4,5,6,17,10)],
                     SG3[,c(2,3,4,5,6,10)],
                     by.x=c("Installation","Plot","STP","Tree","prevyear"),
                     by.y=c("Installation","Plot","STP","Tree","Year_Measurement"))

SG5tags<- stag[stag$Installation=='SG'& stag$Plot=='5',]

SG3tags<- stag[stag$Installation=='SG'& stag$Plot=='3',]


mSG5 <- merge(inc.dataSG5, SG5tags, by.x = "Tree", by.y = "Tree",all=F)
mSG3 <- merge(inc.dataSG3, SG3tags, by.x = "Tree", by.y = "Tree",all=F)
SGCTRL<-rbind.data.frame(mSG5,mSG3)

###

plot4<-rbind.data.frame(SG1x,SGCTRL)
plot4$dif<-(plot4$Height_Total.x- plot4$Height_Total.y)

finalData4<-subset(plot4,!(is.na(plot4$dif)))
finalData4$Plot.x[finalData4$Plot.x=="1"] <- "One Time"
finalData4$Plot.x[finalData4$Plot.x=="7"] <- "One Time"
finalData4$Plot.x[finalData4$Plot.x=="3"] <- "Control"
finalData4$Plot.x[finalData4$Plot.x=="5"] <- "Control"

finalData4$Year <- as.factor(finalData4$Year_Measurement)
finalData4$plo <- as.factor(finalData4$Plot.x)

SGfinalyear<-finalData4
[finalData4$Year_Measurement=='2006',]

psmedataSG<-SGfinalyear[SGfinalyear$Species=='PSME',]


tbp<-theme(                              
  axis.title.x = element_text(face="bold", color="black", size=16),
  axis.title.y = element_text( face="bold",color="black", size=16),
  plot.title = element_text(face="bold", color = "black", size=18), 
  legend.position="bottom",axis.title.y=element_text(vjust=0.8))

library(ggplot2)

sglp<-ggplot(data=psmedataSG, aes(x=(dif*.3048), y=(Height_Total.x*.3048),col=plo))+geom_point()+stat_smooth(method=lm)+
  scale_fill_brewer(palette="Dark2") +tbp+labs(title="Height vs. Height Growth Increment",y="Height at 2006 (m)", x = "Height Growth Increment (M)")+ scale_fill_brewer(palette="Dark2") +tbp
sglp

sglp<-ggplot(data=psmedataSG, aes(x=Year, y=(Height_Total.x*.3048),col=plo))+geom_point()+stat_smooth(method=lm)+
  scale_fill_brewer(palette="Dark2") +tbp+
  labs(title="High Productivity/High Overstory Site (SG)",x="Year", y = "Height")+ 
  scale_fill_brewer(palette="Dark2") +tbp+ylim(0,10)+ geom_line(aes(group = Tree))
sglp



sgbp<-ggplot(data=SGfinalyear, aes(x=Year, y=(Height_Total.x*.3048),fill=plo))+
  geom_boxplot(position=position_dodge())+
  labs(title="Height Growth for High Productivity/High Overstory Site (SG)",x="Measurement Year", y = "Height (m)")+ scale_fill_brewer(palette="Dark2") +tbp+ylim(0,10)
sgbp




