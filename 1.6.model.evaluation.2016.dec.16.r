
#Reads in previous scripts required
source(paste(getwd(),'/1.readdatabase.2016jun2.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.1annualizedht.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.2.UT.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.3.UV.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.4.OS.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.5.SQ.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)



plot(summary(qr.SI, se = "nid"), level = 0.95)
#illustrates how the effects of predictors 
#cary over quantiles  and how the magnitude of 
#the effects at varouis quantiles differ considerably fromthe OLS coefficients
#even in terms of the CI 

qr.SI.5<-  rq(ht_annual~srHeight_Total+CrownWidth+diff.S+TPA.OS+SiteIndex_Value,tau=c(.5),data=annual.gr4)
qr.SI.1 <- rq(ht_annual~srHeight_Total+CrownWidth+diff.S+TPA.OS+SiteIndex_Value,tau=c(.1),data=annual.gr4)
qr.SI.9 <- rq(ht_annual~srHeight_Total+CrownWidth+diff.S+TPA.OS+SiteIndex_Value,tau=c(.9),data=annual.gr4)

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

annual.gr6$response.cat<-0

annual.gr6$diff.S[is.na(annual.gr6$diff.S)] <- 0
annual.gr6$CrownWidth[is.na(annual.gr6$CrownWidth)] <- 0

for(i in 1:nrow(annual.gr6)){
  annual.gr6$response.cat[i]<-valid.func(
    annual.gr6$srHeight_Total[i], 
    annual.gr6$CrownWidth[i],
    annual.gr6$diff.S[i],
    annual.gr6$TPA.OS[i],
    annual.gr6$SiteIndex_Value[i],
    annual.gr6$ht_annual[i])
  }

annual.gr6$count<-1

sorted.totals<-as.data.frame(xtabs(annual.gr6$count~annual.gr6$response.cat)/nrow(annual.gr6))

sum(sorted.totals$Freq)


barchart(sorted.totals$Freq~sorted.totals$annual.gr6.response.cat, names = "Quantile Bin",
        xlab = "Bin", ylab = "Frequency",type=density,
        main = "Witheld Data Height Growth Response 
        sorted by Quantile Category")





