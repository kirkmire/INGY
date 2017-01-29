
#Reads in previous scripts required (takes ~10min)
source(paste(getwd(),'/1.readdatabase.2016jun2.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.1annualizedht.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.2.UT.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.3.UV.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.4.OS.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.5.SQ.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.6.1.annual.gr.6.withelddata.2016.dec.2016.r',sep=""), echo=TRUE)

#combines all aic.lists into one dataframe

colnames(UT.aic)<-c("Variable","n","AIC")

colnames(UV.aic)<-c("Variable","n","AIC")

colnames(OS.aic)<-c("Variable","n","AIC")

colnames(SQ.aic)<-c("Variable","n","AIC")

aic.lists<-rbind(UT.aic,UV.aic,OS.aic,SQ.aic)

aic.lists$AIC<-round(aic.lists$AIC,2)

final.aic<-data.frame(matrix("", nrow = 14, ncol = 9),stringsAsFactors=F)
final.aic$X1<-c(aic.lists$Variable[1:8],"","","","","","")
final.aic$X2<-c(aic.lists$n[1:8],"","","","","","")
final.aic$X3<-c(aic.lists$AIC[1:8],"","","","","","")
final.aic$X4[1:14]<-as.character(aic.lists$Variable[9:22])
final.aic$X5[1:14]<-aic.lists$n[9:22]
final.aic$X6[1:14]<-aic.lists$AIC[9:22]
final.aic$X7[1:4]<-as.character(aic.lists$Variable[23:26])
final.aic$X8[1:4]<-aic.lists$n[23:26]
final.aic$X9[1:4]<-aic.lists$AIC[23:26]
final.aic$X7[5]<-"Site"
final.aic$X8[5]<-"n"
final.aic$X9[5]<-"AIC"
final.aic$X7[6:9]<-as.character(aic.lists$Variable[27:30])
final.aic$X8[6:9]<-aic.lists$n[27:30]
final.aic$X9[6:9]<-aic.lists$AIC[27:30]



#The code below will produce output that can then be copied over to the .tex file
library(Hmisc)

latex(final.aic, file="")            # If you want all the data



#illustrates how the effects of predictors 
#cary over quantiles  and how the magnitude of 
#the effects at varouis quantiles differ considerably fromthe OLS coefficients
#even in terms of the CI 
library(quantreg)
qr.SI.5<-  rq(ht_annual~srHeight_Total+CrownLength+treeminus+TPA.OS+slopePercent*aspect*elevation,tau=c(.5),data=annual.gr4)
qr.SI.1 <- rq(ht_annual~srHeight_Total+CrownLength+treeminus+TPA.OS+slopePercent*aspect*elevation,tau=c(.1),data=annual.gr4)
qr.SI.9 <- rq(ht_annual~srHeight_Total+CrownLength+treeminus+TPA.OS+slopePercent*aspect*elevation,tau=c(.9),data=annual.gr4)

fit1 <- summary(rq(ht_annual~srHeight_Total+CrownLength+treeminus+TPA.OS+slopePercent*aspect*elevation,tau=c(1:9/10),data=annual.gr4))
plot(fit1)



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

annual.gr6$response.cat<-0


annual.gr6<-annual.gr6[!(is.na(annual.gr6$CrownLength)==T),] 
annual.gr6<-annual.gr6[!(is.na(annual.gr6$CrownWidth)==T),] 

for(i in 1:nrow(annual.gr6)){
  annual.gr6$response.cat[i]<-valid.func(
    annual.gr6$srHeight_Total[i], 
    annual.gr6$CrownLength[i],
    annual.gr6$treeminus[i],
    annual.gr6$TPA.OS[i],
    annual.gr6$slopePercent*aspect*elevation[i],
    annual.gr6$ht_annual[i])
  }

annual.gr6$count<-1

sorted.totals<-as.data.frame(xtabs(annual.gr6$count~annual.gr6$response.cat)/nrow(annual.gr6))

sum(sorted.totals$Freq)


barchart(sorted.totals$Freq~sorted.totals$annual.gr6.response.cat, names = "Quantile Bin",
        xlab = "Bin", ylab = "Frequency",type=density,
        main = "Witheld Data Height Growth Response 
        sorted by Quantile Category",ylim=c(0,.60))


#Sorted responses for trees <10 in initial dbh

annual.gr6.lessthan10in<-annual.gr6[annual.gr6$DBH<10,]

sum(is.na(annual.gr6.lessthan10in$DBH==T))

#Removes NA values of DBH, should be done in model building?
annual.gr6.lessthan10in<-annual.gr6.lessthan10in[!is.na(annual.gr6.lessthan10in$DBH),]

for(i in 1:nrow(annual.gr6.lessthan10in)){
  annual.gr6.lessthan10in$response.cat[i]<-valid.func(
    annual.gr6.lessthan10in$srHeight_Total[i], 
    annual.gr6.lessthan10in$CrownLength[i],
    annual.gr6.lessthan10in$treeminus[i],
    annual.gr6.lessthan10in$TPA.OS[i],
    annual.gr6.lessthan10in$slopePercent*aspect*elevation[i],
    annual.gr6.lessthan10in$ht_annual[i])
}

annual.gr6.lessthan10in$count<-1

sorted.totals<-as.data.frame(xtabs(annual.gr6.lessthan10in$count~
                                     annual.gr6.lessthan10in$response.cat)/
                               nrow(annual.gr6.lessthan10in))

sum(sorted.totals$Freq)


barchart(sorted.totals$Freq~sorted.totals$annual.gr6.lessthan10in.response.cat, names = "Quantile Bin",
         xlab = "Bin", ylab = "Frequency",type=density,
         main = "Witheld Data Height Growth Response 
         sorted by Quantile Category (DBH>10in)", ylim=c(0,.5))


###higher resolution by including quantiles .4 to .9 by .4

qr.SI.1<-  rq(ht_annual~srHeight_Total+CrownLength+treeminus+TPA.OS+slopePercent*aspect*elevation,tau=c(.1),data=annual.gr4)
qr.SI.2 <- rq(ht_annual~srHeight_Total+CrownLength+treeminus+TPA.OS+slopePercent*aspect*elevation,tau=c(.2),data=annual.gr4)
qr.SI.3 <- rq(ht_annual~srHeight_Total+CrownLength+treeminus+TPA.OS+slopePercent*aspect*elevation,tau=c(.3),data=annual.gr4)
qr.SI.4 <- rq(ht_annual~srHeight_Total+CrownLength+treeminus+TPA.OS+slopePercent*aspect*elevation,tau=c(.4),data=annual.gr4)
qr.SI.5 <- rq(ht_annual~srHeight_Total+CrownLength+treeminus+TPA.OS+slopePercent*aspect*elevation,tau=c(.5),data=annual.gr4)
qr.SI.6 <- rq(ht_annual~srHeight_Total+CrownLength+treeminus+TPA.OS+slopePercent*aspect*elevation,tau=c(.6),data=annual.gr4)
qr.SI.7 <- rq(ht_annual~srHeight_Total+CrownLength+treeminus+TPA.OS+slopePercent*aspect*elevation,tau=c(.7),data=annual.gr4)
qr.SI.8 <- rq(ht_annual~srHeight_Total+CrownLength+treeminus+TPA.OS+slopePercent*aspect*elevation,tau=c(.8),data=annual.gr4)
qr.SI.9 <- rq(ht_annual~srHeight_Total+CrownLength+treeminus+TPA.OS+slopePercent*aspect*elevation,tau=c(.9),data=annual.gr4)



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



annual.gr6$response.cat<-0

annual.gr6$treeminus[is.na(annual.gr6$treeminus)] <- 0
annual.gr6$CrownLength[is.na(annual.gr6$CrownLength)] <- 0

for(i in 1:nrow(annual.gr6)){
  annual.gr6$response.cat[i]<-valid.func10(
    annual.gr6$srHeight_Total[i], 
    annual.gr6$CrownLength[i],
    annual.gr6$treeminus[i],
    annual.gr6$TPA.OS[i],
    annual.gr6$slopePercent*aspect*elevation[i],
    annual.gr6$ht_annual[i])
}

annual.gr6$count<-1

sorted.totals<-as.data.frame(xtabs(annual.gr6$count~annual.gr6$response.cat)/nrow(annual.gr6))

sum(sorted.totals$Freq)


barchart(sorted.totals$Freq~sorted.totals$annual.gr6.response.cat, names = "Quantile Bin",
         xlab = "Bin", ylab = "Frequency",type=density,
         main = "Witheld Data Height Growth Response 
        sorted by Quantile Category", ylim=c(0,.3))

###Higher resolution of lessthan 10in DBH


annual.gr6.lessthan10in$response.cat<-0

#annual.gr6.lessthan10in$CrownLength[is.na(annual.gr6.lessthan10in$CrownLength)] <- 0


for(i in 1:nrow(annual.gr6.lessthan10in)){
  annual.gr6.lessthan10in$response.cat[i]<-valid.func10(
    annual.gr6.lessthan10in$srHeight_Total[i], 
    annual.gr6.lessthan10in$CrownLength[i],
    annual.gr6.lessthan10in$treeminus[i],
    annual.gr6.lessthan10in$TPA.OS[i],
    annual.gr6.lessthan10in$slopePercent*aspect*elevation[i],
    annual.gr6.lessthan10in$ht_annual[i])
}

annual.gr6.lessthan10in$count<-1

sorted.totals<-as.data.frame(xtabs(annual.gr6.lessthan10in$count~annual.gr6.lessthan10in$response.cat)/nrow(annual.gr6.lessthan10in))

sum(sorted.totals$Freq)


barchart(sorted.totals$Freq~sorted.totals$annual.gr6.lessthan10in.response.cat, names = "Quantile Bin",
         xlab = "Bin", ylab = "Frequency",type=density,
         main = "Witheld Data Height Growth Response 
         sorted by Quantile Category (DBH>10in)",ylim=c(0,.3))

min(sorted.totals$Freq)
max(sorted.totals$Freq)

#range in value from .075 to .148 in each category



#########Residual Plots for Q50 ########
cw.resid <- rq(ht_annual~CrownWidth,tau=c(.5),data=annual.gr4)

plot(cw.resid$residuals~cw.resid$fitted.values)


TPA.resid <- rq(ht_annual~annual.gr4$TPA.OS,tau=c(.5),data=annual.gr4)

plot(TPA.resid$residuals~TPA.resid$fitted.values)

SI.resid <-rq(ht_annual~annual.gr4$slopePercent*aspect*elevation,tau=c(.5),data=annual.gr4)

plot(SI.resid$residuals~SI.resid$fitted.values)

###Sequential GAM Plots###
dev.off()

par(mfrow=c(2,3))
sr.ht.gam<-gam(annual.gr4$ht_annual~s(annual.gr4$srHeight_Total))
plot(sr.ht.gam,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

cl.ht.gam<-gam(annual.gr4$ht_annual~annual.gr4$srHeight_Total+s(annual.gr4$CrownLength))
plot(cl.ht.gam,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)

veg.ht.gam<-gam(annual.gr4$ht_annual~annual.gr4$srHeight_Total+annual.gr4$CrownLength+s(annual.gr4$diff.F.1m))
plot(veg.ht.gam,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)


tpa.os.gam<-gam(annual.gr4$ht_annual~annual.gr4$srHeight_Total+annual.gr4$CrownLength+
           s(annual.gr4$TPA.OS))
plot(tpa.os.gam,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)



si.gam<-gam(annual.gr4$ht_annual~annual.gr4$srHeight_Total+annual.gr4$CrownLength+
           +annual.gr4$TPA.OS+s(annual.gr4$slopePercent*aspect*elevation))
plot(si.gam,residuals=T,se=T,pch=".",ask=F,cex.lab=1.5)





