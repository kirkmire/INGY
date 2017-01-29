#Subset growth increment dataframe to only include the relevant KC trees


KC.FVS.Tree.Data<-annual.gr4[which(annual.gr4$Installation=="KC"),]

#Removes negative crown ratio records
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

#tree.lists.final$Tree<-paste(tree.lists.final$plot,tree.lists.final$Tree,sep="")
names(tree.lists.final)[1]<-"Tree"

#Need to remove OS trees#
tree.lists.final<-tree.lists.final[which(tree.lists.final$Tree>1000),]

tree.lists.final$Tree<-paste(tree.lists.final$plot,tree.lists.final$Tree,sep="")


hist(tree.lists.final$ht)



#Merge with FVS tree list final now that the same lengths

FVS.Final<-merge(KC.FVS.Tree.Data,tree.lists.final,by="Tree") 


#Calculate 1 year FVS predicted height growth from 5 year prediction
FVS.Final$FVS.pred<-((FVS.Final$ht-FVS.Final$Height_Total)/5)

hist(FVS.Final$FVS.pred)

library(quantreg)

qr.SI.5<-  rq(ht_annual~srHeight_Total+CrownLength+diff.G.1m+TPA.OS+SiteIndex_Value,tau=c(.5),data=annual.gr4)
qr.SI.1 <- rq(ht_annual~srHeight_Total+CrownLength+diff.G.1m+TPA.OS+SiteIndex_Value,tau=c(.1),data=annual.gr4)
qr.SI.9 <- rq(ht_annual~srHeight_Total+CrownLength+diff.G.1m+TPA.OS+SiteIndex_Value,tau=c(.9),data=annual.gr4)

anova(qr.SI.1,qr.SI.5,qr.SI.9)

#strong evidence that the predictors are not the same between at least two of the models


valid.func<-function(sqht, stcw, stran, tpaos, si, annualht){
  #sqht<-1
  #stcw<-4
  #s.tran<-40
  #tpa.os<-30
  #si<-60
  # annual.ht<-15
  
  qr.pred.nine <-qr.SI.9$coefficients[1]+qr.SI.9$coefficients[2]*sqht+
    qr.SI.9$coefficients[3]*stcw+qr.SI.9$coefficients[4]*stran+
    qr.SI.9$coefficients[5]*tpaos+qr.SI.9$coefficients[6]*si
  
  qr.pred.five <-qr.SI.5$coefficients[1]+qr.SI.5$coefficients[2]*sqht+
    qr.SI.5$coefficients[3]*stcw+qr.SI.5$coefficients[4]*stran+
    qr.SI.5$coefficients[5]*tpaos+qr.SI.5$coefficients[6]*si
  
  qr.pred.one <-qr.SI.1$coefficients[1]+qr.SI.1$coefficients[2]*sqht+
    qr.SI.1$coefficients[3]*stcw+qr.SI.1$coefficients[4]*stran+
    qr.SI.1$coefficients[5]*tpaos+qr.SI.1$coefficients[6]*si
  
  ifelse(annualht>qr.pred.nine, 
         #yes
         cat<-"top10", 
         #no
         ifelse(qr.pred.five<annualht&&annualht<qr.pred.nine,
                #yes
                cat<- "fiftytoninety", 
                #no
                ifelse(qr.pred.one<annualht&&annualht<qr.pred.five,
                       cat<-"bottom10tofifty",
                       cat<-"bottom10")))
  
  cat
}

FVS.Final$response.cat<-0

FVS.Final$CrownWidth[is.na(FVS.Final$CrownLength)] <- 0

for(i in 1:nrow(FVS.Final)){
  FVS.Final$response.cat[i]<-valid.func(
    FVS.Final$srHeight_Total[i], 
    FVS.Final$CrownLength[i],
    FVS.Final$diff.G.1m[i],
    FVS.Final$TPA.OS[i],
    FVS.Final$SiteIndex_Value[i],
    FVS.Final$FVS.pred[i])
}


FVS.Final$count<-1

sorted.totals<-as.data.frame(xtabs(FVS.Final$count~FVS.Final$response.cat)/nrow(FVS.Final))

sum(sorted.totals$Freq)

library(lattice)

#Find the average annual height by response category
aggregate(FVS.Final$ht_annual, list(FVS.Final$response.cat), mean)


barchart(FVS.Final$Height_Total~FVS.Final$response.cat)

barchart(sorted.totals$Freq~sorted.totals$FVS.Final.response.cat, names = "Quantile Bin",
         xlab = "Bin", ylab = "Frequency",type=density,
         main = "FVS Predicted Height Growth Response 
        sorted by Quantile Category")


for(i in 1:nrow(FVS.Final)){
  FVS.Final$response.cat[i]<-valid.func(
    FVS.Final$srHeight_Total[i], 
    FVS.Final$CrownLength[i],
    FVS.Final$diff.G.1m[i],
    FVS.Final$TPA.OS[i],
    FVS.Final$SiteIndex_Value[i],
    FVS.Final$ht_annual[i])
}



FVS.Final$count<-1

sorted.totals<-as.data.frame(xtabs(FVS.Final$count~FVS.Final$response.cat)/nrow(FVS.Final))

sum(sorted.totals$Freq)

library(lattice )

barchart(sorted.totals$Freq~sorted.totals$FVS.Final.response.cat, names = "Quantile Bin",
         xlab = "Bin", ylab = "Frequency",type=density,
         main = "KC Actual Height Growth Response 
         sorted by Quantile Category")

#Find the average annual height by response category
aggregate(FVS.Final$ht_annual, list(FVS.Final$response.cat), mean)


###higher resolution by including quantiles .1 to .9 by .1

qr.SI.1<-  rq(ht_annual~srHeight_Total+CrownLength+diff.G.1m+TPA.OS+SiteIndex_Value,tau=c(.1),data=annual.gr4)
qr.SI.2 <- rq(ht_annual~srHeight_Total+CrownLength+diff.G.1m+TPA.OS+SiteIndex_Value,tau=c(.2),data=annual.gr4)
qr.SI.3 <- rq(ht_annual~srHeight_Total+CrownLength+diff.G.1m+TPA.OS+SiteIndex_Value,tau=c(.3),data=annual.gr4)
qr.SI.4 <- rq(ht_annual~srHeight_Total+CrownLength+diff.G.1m+TPA.OS+SiteIndex_Value,tau=c(.4),data=annual.gr4)
qr.SI.5 <- rq(ht_annual~srHeight_Total+CrownLength+diff.G.1m+TPA.OS+SiteIndex_Value,tau=c(.5),data=annual.gr4)
qr.SI.6 <- rq(ht_annual~srHeight_Total+CrownLength+diff.G.1m+TPA.OS+SiteIndex_Value,tau=c(.6),data=annual.gr4)
qr.SI.7 <- rq(ht_annual~srHeight_Total+CrownLength+diff.G.1m+TPA.OS+SiteIndex_Value,tau=c(.7),data=annual.gr4)
qr.SI.8 <- rq(ht_annual~srHeight_Total+CrownLength+diff.G.1m+TPA.OS+SiteIndex_Value,tau=c(.8),data=annual.gr4)
qr.SI.9 <- rq(ht_annual~srHeight_Total+CrownLength+diff.G.1m+TPA.OS+SiteIndex_Value,tau=c(.9),data=annual.gr4)




valid.func10<-function(sqht, stcw, stran, tpaos, si, annualht){
  #sqht<-1
  #stcw<-4
  #s.tran<-40
  #tpa.os<-30
  #si<-60
  # annual.ht<-15
  
  qr.pred.one <-qr.SI.1$coefficients[1]+qr.SI.1$coefficients[2]*sqht+
    qr.SI.1$coefficients[3]*stcw+qr.SI.1$coefficients[4]*stran+
    qr.SI.1$coefficients[5]*tpaos+qr.SI.1$coefficients[6]*si
  
  qr.pred.two <-qr.SI.2$coefficients[1]+qr.SI.2$coefficients[2]*sqht+
    qr.SI.2$coefficients[3]*stcw+qr.SI.2$coefficients[4]*stran+
    qr.SI.2$coefficients[5]*tpaos+qr.SI.2$coefficients[6]*si
  
  qr.pred.three <-qr.SI.3$coefficients[1]+qr.SI.3$coefficients[2]*sqht+
    qr.SI.3$coefficients[3]*stcw+qr.SI.3$coefficients[4]*stran+
    qr.SI.3$coefficients[5]*tpaos+qr.SI.3$coefficients[6]*si
  
  qr.pred.four <-qr.SI.4$coefficients[1]+qr.SI.4$coefficients[2]*sqht+
    qr.SI.4$coefficients[3]*stcw+qr.SI.4$coefficients[4]*stran+
    qr.SI.4$coefficients[5]*tpaos+qr.SI.4$coefficients[6]*si
  
  qr.pred.five <-qr.SI.5$coefficients[1]+qr.SI.5$coefficients[2]*sqht+
    qr.SI.5$coefficients[3]*stcw+qr.SI.5$coefficients[4]*stran+
    qr.SI.5$coefficients[5]*tpaos+qr.SI.5$coefficients[6]*si
  
  qr.pred.six <-qr.SI.6$coefficients[1]+qr.SI.6$coefficients[2]*sqht+
    qr.SI.6$coefficients[3]*stcw+qr.SI.6$coefficients[4]*stran+
    qr.SI.6$coefficients[5]*tpaos+qr.SI.6$coefficients[6]*si
  
  qr.pred.seven <-qr.SI.7$coefficients[1]+qr.SI.7$coefficients[2]*sqht+
    qr.SI.7$coefficients[3]*stcw+qr.SI.7$coefficients[4]*stran+
    qr.SI.7$coefficients[5]*tpaos+qr.SI.7$coefficients[6]*si
  
  
  qr.pred.eight <-qr.SI.8$coefficients[1]+qr.SI.8$coefficients[2]*sqht+
    qr.SI.8$coefficients[3]*stcw+qr.SI.8$coefficients[4]*stran+
    qr.SI.8$coefficients[5]*tpaos+qr.SI.8$coefficients[6]*si
  
  qr.pred.nine <-qr.SI.9$coefficients[1]+qr.SI.9$coefficients[2]*sqht+
    qr.SI.9$coefficients[3]*stcw+qr.SI.9$coefficients[4]*stran+
    qr.SI.9$coefficients[5]*tpaos+qr.SI.9$coefficients[6]*si
  
  ifelse(annualht>qr.pred.nine, 
         #yes
         cat<-"9", 
         #no
         ifelse(qr.pred.eight<annualht&&annualht<qr.pred.nine,
                #yes
                cat<- "8", 
                #no
                ifelse(qr.pred.seven<annualht&&annualht<qr.pred.eight,
                       #yes
                       cat<-"7",
                       #no
                       ifelse(qr.pred.six<annualht&&annualht<qr.pred.seven,
                              #yes
                              cat<-"6",
                              #no
                              ifelse(qr.pred.five<annualht&&annualht<qr.pred.six,
                                     #yes
                                     cat<-"5",
                                     #no
                                     ifelse(qr.pred.four<annualht&&annualht<qr.pred.five,
                                            #yes
                                            cat<-"4",
                                            #no
                                            ifelse(qr.pred.three<annualht&&annualht<qr.pred.four,
                                                   #yes
                                                   cat<-"3",
                                                   #no
                                                   ifelse(qr.pred.two<annualht&&annualht<qr.pred.three,
                                                          #yes
                                                          cat<-"2",
                                                          #no
                                                          ifelse(qr.pred.one<annualht&&annualht<qr.pred.two,
                                                                 #yes
                                                                 cat<-"1",
                                                                 #no
                                                                 cat<-"0")))))))))
  cat
}



FVS.Final$response.cat<-0

FVS.Final$diff.S[is.na(FVS.Final$diff.S)] <- 0
FVS.Final$CrownWidth[is.na(FVS.Final$CrownWidth)] <- 0

for(i in 1:nrow(FVS.Final)){
  FVS.Final$response.cat[i]<-valid.func10(
    FVS.Final$srHeight_Total[i], 
    FVS.Final$CrownLength[i],
    FVS.Final$diff.G.1m[i],
    FVS.Final$TPA.OS[i],
    FVS.Final$SiteIndex_Value[i],
    FVS.Final$FVS.pred[i])
}

FVS.Final$count<-1

sorted.totals<-as.data.frame(xtabs(FVS.Final$count~FVS.Final$response.cat)/nrow(FVS.Final))

sum(sorted.totals$Freq)


barchart(sorted.totals$Freq~sorted.totals$FVS.Final.response.cat, names = "Quantile Bin",
         xlab = "Bin", ylab = "Frequency",type=density,
         main = "FVS Predicted Data Height Growth Response 
         sorted by Quantile Category")

#Find the average annual height by response category
FVS.Final.avg<-aggregate(FVS.Final$ht_annual, list(FVS.Final$response.cat), mean)

plot(FVS.Final.avg$Group.1,FVS.Final.avg$x)


for(i in 1:nrow(FVS.Final)){
  FVS.Final$response.cat[i]<-valid.func10(
    FVS.Final$srHeight_Total[i], 
    FVS.Final$CrownLength[i],
    FVS.Final$diff.G.1m[i],
    FVS.Final$TPA.OS[i],
    FVS.Final$SiteIndex_Value[i],
    FVS.Final$ht_annual[i])
}

FVS.Final$count<-1

sorted.totals<-as.data.frame(xtabs(FVS.Final$count~FVS.Final$response.cat)/nrow(FVS.Final))

sum(sorted.totals$Freq)


barchart(sorted.totals$Freq~sorted.totals$FVS.Final.response.cat, names = "Quantile Bin",
         xlab = "Bin", ylab = "Frequency",type=density,
         main = "Actual Height Growth Response 
         sorted by Quantile Category")

#Find the average annual height by response category
FVS.Final.avg<-aggregate(FVS.Final$ht_annual, list(FVS.Final$response.cat), mean)

plot(FVS.Final.avg$Group.1,FVS.Final.avg$x)

###For loop for finding assinging a column for a given trees Q50

FVS.Final$Q50<-0

for(i in 1:nrow(FVS.Final)){
  FVS.Final$Q50[i]<-qr.SI.5$coefficients[1]+
    qr.SI.5$coefficients[2]*FVS.Final$srHeight_Total[i]+
    qr.SI.5$coefficients[3]*FVS.Final$CrownLength[i]+
    qr.SI.5$coefficients[4]*FVS.Final$diff.G.1m[i]+
    qr.SI.5$coefficients[5]*FVS.Final$TPA.OS[i]+
    qr.SI.5$coefficients[6]*FVS.Final$SiteIndex_Value[i]
  }


plot(FVS.Final$Q50~FVS.Final$ht_annual,xlim=c(0,2),ylim=c(0,2))
abline(reg=lm(FVS.Final$Q50 ~ FVS.Final$ht_annual))


plot(FVS.Final$Q50~FVS.Final$FVS.pred)

plot(FVS.Final$FVS.pred~FVS.Final$ht_annual)
