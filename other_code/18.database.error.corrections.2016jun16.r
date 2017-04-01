#Appears to be errors in data entry
#Tree 663 goes from 8.5ft to 1.6ft, should read 9.6ft
stagm$Height_Total[stagm$Year_Measurement==2008&stagm$Plot==1&stagm$Tree==663]<-9.6

#Tree 53 goes from 8.6ft to 1.5ft, should read 9.5ft
stagm$Height_Total[stagm$Plot==3&stagm$Tree==53&stagm$Year_Measurement==2008]<-9.5

#Tree 19, Plot 3 has Height total of 0 for 2008 measurement, removed#
stagm<-stagm[!(stagm$Plot=="3"&stagm$Tree=="19"),]

#Tree 242, Plot 6 has Height total of 0 for 2008 measurement, removed#
stagm<-stagm[!(stagm$Plot=="6"&stagm$Tree=="242"),]

#tree 683 plot 4 stp 5#
stagm<-stagm[!(stagm$Plot=="4"&stagm$Tree=="683"),]

#tree 111 plot 1 stp 6#
stagm<-stagm[!(stagm$Plot=="1"&stagm$Tree=="111"),]

#tree 53 plot 3#
stagm<-stagm[!(stagm$Plot=="3"&stagm$Tree=="53"),]

#tree 284 plot 7#
stagm<-stagm[!(stagm$Plot=="7"&stagm$Tree=="284"),]

#tree 273 plot 1#
stagm<-stagm[!(stagm$Plot=="1"&stagm$Tree=="673"),]

#LF
#Tree 782 goes from 13.2ft to 7.8ft, should read 17.8ft
stagm$Height_Total[stagm$Installation=="LF"&stagm$Plot==3&stagm$Tree==782&stagm$Year_Measurement==2014]<-17.80




