
#Reads in previous scripts required (takes ~10min)
# source(paste(getwd(),'/1.readdatabase.2016jun2.r',sep = ""), echo=TRUE)
# source(paste(getwd(),'/other_code/18.database.error.corrections.2016jun16.r',sep=""), echo=TRUE)
# source(paste(getwd(),'/1.1annualizedht.2016.dec.16.r',sep = ""), echo=TRUE)
# source(paste(getwd(),'/1.2.UT.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
# source(paste(getwd(),'/1.3.UV.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
# source(paste(getwd(),'/1.4.OS.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
# source(paste(getwd(),'/1.5.SQ.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
# source(paste(getwd(),'/1.6.1.annual.gr.6.withelddata.2016.dec.2016.r',sep=""), echo=TRUE)
# source(paste(getwd(),'/7.ge.ctrl.veg.2016jun2.r',sep=""), echo=TRUE)



# #combines all aic.lists into one dataframe
# colnames(UV.aic)<-c("Variable", "n", "AIC")
# colnames(UT.aic)<-c("Variable", "n", "AIC")
# colnames(OS.aic)<-c("Variable", "n", "AIC")
# colnames(SQ.aic)<-c("Variable", "n", "AIC")
# aic.lists<-rbind(UT.aic,UV.aic,OS.aic,SQ.aic)
# #
# #
# aic.lists$AIC<-round(aic.lists$AIC,0)

# 
# #Gonna need to adjust this...
# final.aic<-data.frame(matrix("", nrow = 18, ncol = 12),stringsAsFactors=F)
# final.aic$X1[1:9]<-as.character(aic.lists$Variable[1:9])
# final.aic$X2[1:9]<-aic.lists$n[1:9]
# final.aic$X3[1:9]<-aic.lists$AIC[1:9]
# final.aic$X4[1:18]<-as.character(aic.lists$Variable[10:27])
# final.aic$X5[1:18]<-aic.lists$n[10:27]
# final.aic$X6[1:18]<-aic.lists$AIC[10:27]
# final.aic$X7[1:5]<-as.character(aic.lists$Variable[28:32])
# final.aic$X8[1:5]<-aic.lists$n[28:32]
# final.aic$X9[1:5]<-aic.lists$AIC[28:32]
# final.aic$X10[1:5]<-as.character(aic.lists$Variable[33:37])
# final.aic$X11[1:5]<-aic.lists$n[33:37]
# final.aic$X12[1:5]<-aic.lists$AIC[33:37]
#
# colnames(final.aic)<-c("Variable","n","AIC","Variable","n","AIC","Variable","n","AIC","Variable","n","AIC")
# #
# #
# # #
# # # #The code below will produce output that can then be copied over to the .tex file
# library(Hmisc)
# # #
# latex(final.aic, file="",rowname = "")

#
#
# qr.sum<-data.frame(summary(qr.SI.1))
#
# latex(coef9,file="")
#
# coef1<-data.frame(coef(qr.SI.1))
# coef5<-data.frame(coef(qr.SI.5))
# coef9<-data.frame(coef(qr.SI.9))


# finalcoef<-data.frame(matrix("", nrow = 14, ncol = 9),stringsAsFactors=F)
# finalcoef$X1[1:9]<-as.character(aic.lists$Variable[1:9])
# finalcoef$X2[1:9]<-aic.lists$n[1:9]
# finalcoef$X3[1:9]<-aic.lists$AIC[1:9]
# finalcoef$X4[1:13]<-as.character(aic.lists$Variable[10:22])
# finalcoef$X5[1:13]<-aic.lists$n[10:22]
# finalcoef$X6[1:13]<-aic.lists$AIC[10:22]
# finalcoef$X7[1:4]<-as.character(aic.lists$Variable[23:26])
# finalcoef$X8[1:4]<-aic.lists$n[23:26]
# finalcoef$X9[1:4]<-aic.lists$AIC[23:26]
# finalcoef$X7[5]<-"Site"
# finalcoef$X8[5]<-"n"
# finalcoef$X9[5]<-"AIC"
# finalcoef$X7[6:10]<-as.character(aic.lists$Variable[27:31])
# finalcoef$X8[6:10]<-aic.lists$n[27:31]
# finalcoef$X9[6:10]<-aic.lists$AIC[27:31]
# 



library(quantreg)

annual.gr4<-annual.gr4[!annual.gr4$ht_annual==0,]


qr.SI.1 <-rq(log(ht_annual)~srHeight_Total+
               cratio+
               TPA.OS+
               slopePercent +
               slopePercent:cos_rad_asp +
               slopePercent:sin_rad_asp +
               slopePercent:log(elevation+1) +
               slopePercent:log(elevation+1):cos_rad_asp +
               slopePercent:log(elevation+1):sin_rad_asp +
               slopePercent:I(elevation^2) +
               slopePercent:I(elevation^2):cos_rad_asp +
               slopePercent:I(elevation^2):sin_rad_asp +
               elevation +
               I(elevation^2) ,
             tau=.1 ,  data=annual.gr4)







qr.SI.5 <-rq(ht_annual~srHeight_Total+
               cratio+
               TPA.OS+
               slopePercent +
               slopePercent:cos_rad_asp +
               slopePercent:sin_rad_asp +
               slopePercent:log(elevation+1) +
               slopePercent:log(elevation+1):cos_rad_asp +
               slopePercent:log(elevation+1):sin_rad_asp +
               slopePercent:I(elevation^2) +
               slopePercent:I(elevation^2):cos_rad_asp +
               slopePercent:I(elevation^2):sin_rad_asp +
               elevation +
               I(elevation^2) ,
             tau=.5,  data=annual.gr4)

qr.SI.9 <-rq(ht_annual~srHeight_Total+
               cratio+
               TPA.OS+
               slopePercent +
               slopePercent:cos_rad_asp +
               slopePercent:sin_rad_asp +
               slopePercent:log(elevation+1) +
               slopePercent:log(elevation+1):cos_rad_asp +
               slopePercent:log(elevation+1):sin_rad_asp +
               slopePercent:I(elevation^2) +
               slopePercent:I(elevation^2):cos_rad_asp +
               slopePercent:I(elevation^2):sin_rad_asp +
               elevation +
               I(elevation^2) ,
             tau=.9 ,  data=annual.gr4)

#below is for the latex summarys of parameter estimates

# pred.tab<-read.csv("Pred.table.csv")
# 
# library(Hmisc)
# latex(pred.tab, file="", rowname = "")
# 
# qr.SI.1.sum<-signif(summary(qr.SI.1)$coefficient,digits=3)
# latex(qr.SI.1.sum, file="")
# 
# qr.SI.5.sum<-signif(summary(qr.SI.5)$coefficient,digits=3)
# latex(qr.SI.5.sum, file="")
# 
# qr.SI.9.sum<-signif(summary(qr.SI.9)$coefficient,digits=3)
# latex(qr.SI.9.sum, file="")



annual.gr6$qr.pred.one <- predict.rq(qr.SI.1, annual.gr6)
annual.gr6$qr.pred.five <- predict.rq(qr.SI.5, annual.gr6)
annual.gr6$qr.pred.nine <- predict.rq(qr.SI.9, annual.gr6)

# curiosity<-annual.gr6[annual.gr6$qr.pred.one<0,]

valid.func<-function(annualht,qr.pred.one,qr.pred.five,qr.pred.nine){
  # annualht<-1.2
  # qr.pred.one<-1
  # qr.pred.five<-1.6
  # qr.pred.nine<-1.6

 ifelse(annualht>qr.pred.nine,
       #yes
       cat<-"> .9",
       #no
       ifelse(qr.pred.five<annualht&&annualht<qr.pred.nine,
              #yes
              cat<- ".5 - .9",
              #no
              ifelse(qr.pred.one<annualht&&annualht<qr.pred.five,
                     cat<-".1 - .5",
                     cat<-"< .1")))

cat

}

annual.gr6$response.cat<-0

for(i in 1:nrow(annual.gr6)){
  annual.gr6$response.cat[i]<-valid.func(
    annual.gr6$ht_annual[i],
    annual.gr6$qr.pred.one[i],
    annual.gr6$qr.pred.five[i],
    annual.gr6$qr.pred.nine[i])

}


annual.gr6$counts<-1
sorted.totals<-aggregate(annual.gr6$counts,
                         by=list(Category=annual.gr6$response.cat), FUN=sum)


# library(RColorBrewer)
# library(lattice)
# 
sorted.totals$x<-sorted.totals$x/sum(sorted.totals$x)



sorted.totals$Category= factor(sorted.totals$Category,
                                levels = c("< .1",
                                           ".1 - .5",
                                           ".5 - .9",
                                           "> .9"),
                                ordered=TRUE)

barchart(sorted.totals$x~sorted.totals$Category, names = "Quantile Bin",
        xlab = "Bin", ylab = "Fraction of Total Witheld Trees",type=density,
        main = "Witheld Data Height Growth Response
        sorted by Quantile Category",ylim=c(0,.60),
        # par.settings = my.settings,
        par.strip.text=list(col="white", font=2),
        panel=function(x,y,...){
          panel.grid(h=-1, v=0);
          panel.barchart(x,y,...)
        })

#Creates Coefficient Plots Across Quantiles

qr.SI.all <-rq(ht_annual~srHeight_Total+
               cratio+
               TPA.OS+
               slopePercent +
               slopePercent:cos_rad_asp +
               slopePercent:sin_rad_asp +
               slopePercent:log(elevation+1) +
               slopePercent:log(elevation+1):cos_rad_asp +
               slopePercent:log(elevation+1):sin_rad_asp +
               slopePercent:I(elevation^2) +
               slopePercent:I(elevation^2):cos_rad_asp +
               slopePercent:I(elevation^2):sin_rad_asp +
               elevation +
               (elevation^2) ,
             tau=c(1:9/10) ,  data=annual.gr4)



# summary(qr.SI.all)

# plot(summary(qr.SI.all),parm="srHeight_Total",main="")
# plot(summary(qr.SI.all),parm="cratio",main="")
# plot(summary(qr.SI.all),parm="TPA.OS",main="")
# 
# 


#Creates column of SEA effect

SEA.func<-function(slopePercent,cos_rad_asp,sin_rad_asp,
                   elevation){
  pred<-(  
    coef(qr.SI.5)[5]*slopePercent+
      coef(qr.SI.5)[6]*elevation+
      coef(qr.SI.5)[7]*I(elevation^2)+
      coef(qr.SI.5)[8]*slopePercent*cos_rad_asp+
      coef(qr.SI.5)[9]*slopePercent*sin_rad_asp+
      coef(qr.SI.5)[10]*slopePercent*log(elevation+1)+
      coef(qr.SI.5)[11]*slopePercent*I(elevation^2)+
      coef(qr.SI.5)[12]*slopePercent*cos_rad_asp*log(elevation+1)+
      coef(qr.SI.5)[13]*slopePercent*sin_rad_asp*log(elevation+1)+
      coef(qr.SI.5)[14]*slopePercent*cos_rad_asp*I(elevation^2)+
      coef(qr.SI.5)[15]*slopePercent*sin_rad_asp*I(elevation^2))
  
  pred}

# annual.gr6<-annual.gr6[!annual.gr6$DBH>3.5,]
annual.gr4$SEA.val<-0


for(i in 1:nrow(annual.gr4)){
  annual.gr4$SEA.val[i]<-SEA.func(
    annual.gr4$slopePercent[i],
    annual.gr4$cos_rad_asp[i],
    annual.gr4$sin_rad_asp[i],
    annual.gr4$elevation[i])
}



#Chi Squared test of homogeneity


# annual.gr6.lessthan5<-annual.gr6[annual.gr6$Height_Total<5,]
# annual.gr6.5to10<-annual.gr6[5<annual.gr6$Height_Total&annual.gr6$Height_Total<10,]
# annual.gr6.10plus<-annual.gr6[annual.gr6$Height_Total>10,]
# #
# x.table1<-t(aggregate(annual.gr6.lessthan5$counts, by=list(Category=annual.gr6.lessthan5$response.cat), FUN=sum))
# x.table1<-x.table1["x",]
# x.table1<-as.numeric(x.table1)
# 
# x.table2<-t(aggregate(annual.gr6.5to10$counts, by=list(Category=annual.gr6.5to10$response.cat), FUN=sum))
# x.table2<-x.table2["x",]
# x.table2<-as.numeric(x.table2)
# 
# x.table3<-t(aggregate(annual.gr6.10plus$counts, by=list(Category=annual.gr6.10plus$response.cat), FUN=sum))
# x.table3<-x.table3["x",]
# x.table3<-as.numeric(x.table3)
# 
# brkdwn<-rbind(x.table1,x.table2,x.table3)
# rownames(brkdwn)<-c("<5","5-10",">10")
# colnames(brkdwn)<-c(".1-.5",".5-.9","<.1",">.9")
# 
# 
# xsq1<-chisq.test(x.table1,p=c(.4,.4,.1,.1))
# xsq1$observed
# xsq2<-chisq.test(x.table2,p=c(.4,.4,.1,.1))
# xsq2$observed
# xsq3<-chisq.test(x.table3,p=c(.4,.4,.1,.1))
# xsq3$observed
# 
# exp.tab<-rbind(xsq1$expected,xsq2$expected,xsq3$expected)
# rownames(exp.tab)<-c("<5","5-10",">10")
# colnames(exp.tab)<-c(".1-.5",".5-.9","<.1",">.9")
# 
# 
# totals<-c(sum(brkdwn[,1]),sum(brkdwn[,2]),sum(brkdwn[,3]),sum(brkdwn[,4]))
# tot.test<-chisq.test(totals,p=c(.4,.4,.1,.1))
# tot.test$expected
# summary(tot.test)
# tot.test$observed
# 
# chisq<-cbind(xsq1$p.value,xsq2$p.value,xsq3$p.value,tot.test$p.value)
# numb.trees.ht.class<-c(sum(x.table1),sum(x.table2),sum(x.table3),sum(x.table1,x.table2,x.table3))
# 
# chisq<-rbind(chisq,numb.trees.ht.class)
# chisq<-round(chisq,3)
# 
# 
# rownames(chisq)<-c("P-Value","Number of Trees")
# colnames(chisq)<-c("<5","5-10",">10","All")

# #
# #
# # #The code below will produce output that can then be copied over to the .tex file
#
# ptab<-read.csv("P_table.csv")
# library(Hmisc)
# #
# latex(ptab,file="",rowname = "")
#

#
# #The code below will produce output that can then be copied over to the .tex file
# library(Hmisc)
#
# latex(chitable, file="")
# #
# # ###
# actual<-rbind(x.table1,x.table2,x.table3)
# rownames(actual)<-c("<4","4-6",">6")
# colnames(actual)<-c("<.1",".1to.5",".5tpo.9",">.9")
# #
# #
# # #The code below will produce output that can then be copied over to the .tex file
# # library(Hmisc)
# #
# latex(actual, file="")




#Look at one installations trees
# KC_trees<-annual.gr4[which(annual.gr4$Installation=="KC"&
#                              annual.gr4$Year_Measurement==2002),]
# 
# #Select only first year measurements (2002)
# 
# KC_2002<-stagm[which(stagm$Installation=="KC"),]
# KC_2002<-KC_2002[which(KC_2002$Year_Measurement==2002),]
# KC_2006<-stagm[which(stagm$Installation=="KC"),]
# KC_2006<-KC_2006[which(KC_2006$Year_Measurement==2006),]
# 
# KC_both<-merge(KC_2002,KC_2006,by=c("Plot","STP","Tree"))
# 
# KC_all<-merge(KC_both,KC_trees,by=c("Plot","STP","Tree"),all.y=T)
# 
# KC_all$meas.diff<-KC_all$Height_Total.y-KC_all$Height_Total.x
# 
# KC_all<-KC_all[which(!KC_all$Damage.y=="DEAD"),]
# KC_all<-KC_all[which(!KC_all$Damage.y=="D"),]
# KC_all<-KC_all[which(!KC_all$Damage.y=="DT"),]
# 
# 
# KC_all$qr.pred.one <- predict.rq(qr.SI.1, KC_all)*4
# KC_all$qr.pred.five <- predict.rq(qr.SI.5, KC_all)*4
# KC_all$qr.pred.nine <- predict.rq(qr.SI.9, KC_all)*4
# 
# KC_all<-KC_all[!is.na(KC_all$Height_Total.y),]
# 
# KC_all$response.cat<-0
# 
# 
# for(i in 1:nrow(KC_all)){
#   KC_all$response.cat[i]<-valid.func(
#     KC_all$meas.diff[i],
#     KC_all$qr.pred.one[i],
#     KC_all$qr.pred.five[i],
#     KC_all$qr.pred.nine[i])
#   
# }
# 
# 
# plot(KC_all$qr.pred.five,KC_all$meas.diff)
# 
# KC_all$qr.pred.five<-as.numeric((KC_all$qr.pred.five))
# KC_all$meas.diff<-as.numeric((KC_all$meas.diff))
# 
# length(KC_all$qr.pred.five)
# length(KC_all$meas.diff)
# 
# hist(KC_all$qr.pred.five)
# hist(KC_all$qr.pred.nine)
# 
# library(plyr)
# sorted.totals1<-count(annual.gr6, 'response.cat')
# 
# 
# trellis.device(color = FALSE)
# 
# lattice.options(default.theme = modifyList(standard.theme(color = 
#                                                             FALSE), list(strip.background = list(col = "transparent")))) 
# 
# library(RColorBrewer)
# display.brewer.all()
# 
# 
# myColours <- brewer.pal(6,"Greens")
# ## Create your own list with
# my.settings <- list(
#   superpose.polygon=list(col=myColours[2:5], border="transparent"),
#   strip.background=list(col=myColours[6]),
#   strip.border=list(col="black")
# )
# 
# 
# library(plyr)
# sorted.totals1<-count(KC_all, 'response.cat')
# 
# 
# barchart(sorted.totals1$freq/length(KC_all$InstPlot)~sorted.totals1$response.cat, names = "Quantile Bin",
#          xlab = "Bin", ylab = "Fraction of Installations Trees",type=density,
#          main = "KC Height Growth b/t 2002 and 2006
#          sorted by predicted quantiles",ylim=c(0,.80),
#          par.settings = my.settings,
#          par.strip.text=list(col="white", font=2),
#          panel=function(x,y,...){
#            panel.grid(h=-1, v=0); 
#            panel.barchart(x,y,...)
#          })
# 
# 
# ###looking at predicted median vs actual ht growth increments
# annual.gr6<-annual.gr6[!annual.gr6$ht_annual<0,]
# annual.gr6$lm_ht<-predict(least_squares,annual.gr6)
# 
# plot(sqrt(annual.gr6$ht_annual),annual.gr6$qr.pred.five,col="blue")
# abline(fit<-lm(annual.gr6$qr.pred.one~sqrt(annual.gr6$ht_annual)),col="red")
# abline(fit<-lm(annual.gr6$qr.pred.five~sqrt(annual.gr6$ht_annual)),col="blue")
# abline(fit<-lm(annual.gr6$qr.pred.nine~sqrt(annual.gr6$ht_annual)),col="green")
# # points(annual.gr6$ht_annual,annual.gr6$qr.pred.one,col="red")
# # points(annual.gr6$ht_annual,annual.gr6$qr.pred.nine,col="green")
# # points(annual.gr6$ht_annual,annual.gr6$lm_ht,col="yellow")
# abline(fit<-lm(annual.gr6$lm_ht~sqrt(annual.gr6$ht_annual)),col="black")


###looking at predicted median vs init height
# annual.gr6<-annual.gr6[!annual.gr6$height_annual<0,]
# annual.gr6$lm_ht<-predict(least_squares,annual.gr6)

# plot(sqrt(annual.gr6$Height_Total),annual.gr6$qr.pred.five,col="blue")
# abline(fit<-lm(annual.gr6$qr.pred.one~sqrt(annual.gr6$Height_Total)),col="red")
# abline(fit<-lm(annual.gr6$qr.pred.five~sqrt(annual.gr6$Height_Total)),col="blue")
# abline(fit<-lm(annual.gr6$qr.pred.nine~sqrt(annual.gr6$Height_Total)),col="green")
# points(annual.gr6$Height_Total,annual.gr6$qr.pred.one,col="red")
# points(annual.gr6$Height_Total,annual.gr6$qr.pred.nine,col="green")
# points(annual.gr6$Height_Total,annual.gr6$lm_ht,col="yellow")
# abline(fit<-lm(annual.gr6$lm_ht~sqrt(annual.gr6$Height_Total)),col="black")

# rmse <- round(sqrt(mean(resid(fit)^2)), 2)

# summary(fit)


# #Sorted responses for trees <10 in initial dbh
# 
# annual.gr6.lessthan10in<-annual.gr6[annual.gr6$DBH<10,]
# 
# sum(is.na(annual.gr6.lessthan10in$DBH==T))
# 
# #Removes NA values of DBH, should be done in model building?
# annual.gr6.lessthan10in<-annual.gr6.lessthan10in[!is.na(annual.gr6.lessthan10in$DBH),]
# 
# for(i in 1:nrow(annual.gr6.lessthan10in)){
#   annual.gr6.lessthan10in$response.cat[i]<-valid.func(
#     annual.gr6.lessthan10in$srHeight_Total[i],
#     annual.gr6.lessthan10in$CrownLength[i],
#     annual.gr6.lessthan10in$treeminus[i],
#     annual.gr6.lessthan10in$TPA.OS[i],
#     annual.gr6.lessthan10in$slopePercent*aspect*elevation[i],
#     annual.gr6.lessthan10in$ht_annual[i])
# }
# 
# 
# 
# annual.gr6.lessthan10in$count<-1
# 
# sorted.totals<-as.data.frame(xtabs(annual.gr6.lessthan10in$count~
#                                      annual.gr6.lessthan10in$response.cat)/
#                                nrow(annual.gr6.lessthan10in))
# 
# sum(sorted.totals$Freq)
# 
# 
# barchart(sorted.totals$Freq~sorted.totals$annual.gr6.lessthan10in.response.cat, names = "Quantile Bin",
#          xlab = "Bin", ylab = "Frequency",type=density,
#          main = "Witheld Data Height Growth Response
#          sorted by Quantile Category (DBH>10in)", ylim=c(0,.5))



###higher resolution by including quantiles .1 to .9 by .1



# valid.func10<-function(sqht, stcw, stran, tpaos, slope, asp,
#                        elev, annualht){
#   #sqht<-1
#   #stcw<-4
#   #s.tran<-40
#   #tpa.os<-30
#   #si<-60
#   # annual.ht<-15
# 
#   qr.pred.one <-qr.SI.1$coefficients[1]+qr.SI.1$coefficients[2]*sqht+
#     qr.SI.1$coefficients[3]*stcw+qr.SI.1$coefficients[4]*stran+
#     qr.SI.1$coefficients[5]*tpaos+
#     qr.SI.1$coefficients[6]*slope+
#     qr.SI.1$coefficients[7]*asp+
#     qr.SI.1$coefficients[8]*elev+
#     qr.SI.1$coefficients[9]*slope*asp+
#     qr.SI.1$coefficients[10]*slope*elev+
#     qr.SI.1$coefficients[11]*asp*elev+
#     qr.SI.1$coefficients[12]*slope*asp*elev
# 
#   qr.pred.two <-qr.SI.2$coefficients[1]+qr.SI.2$coefficients[2]*sqht+
#     qr.SI.2$coefficients[3]*stcw+qr.SI.2$coefficients[4]*stran+
#     qr.SI.2$coefficients[5]*tpaos+
#     qr.SI.2$coefficients[6]*slope+
#     qr.SI.2$coefficients[7]*asp+
#     qr.SI.2$coefficients[8]*elev+
#     qr.SI.2$coefficients[9]*slope*asp+
#     qr.SI.2$coefficients[10]*slope*elev+
#     qr.SI.2$coefficients[11]*asp*elev+
#     qr.SI.2$coefficients[12]*slope*asp*elev
# 
#   qr.pred.three <-qr.SI.3$coefficients[1]+qr.SI.3$coefficients[2]*sqht+
#     qr.SI.3$coefficients[3]*stcw+qr.SI.3$coefficients[4]*stran+
#     qr.SI.3$coefficients[5]*tpaos+
#     qr.SI.3$coefficients[6]*slope+
#     qr.SI.3$coefficients[7]*asp+
#     qr.SI.3$coefficients[8]*elev+
#     qr.SI.3$coefficients[9]*slope*asp+
#     qr.SI.3$coefficients[10]*slope*elev+
#     qr.SI.3$coefficients[11]*asp*elev+
#     qr.SI.3$coefficients[12]*slope*asp*elev
# 
# 
#   qr.pred.four <-qr.SI.4$coefficients[1]+qr.SI.4$coefficients[2]*sqht+
#     qr.SI.4$coefficients[3]*stcw+qr.SI.4$coefficients[4]*stran+
#     qr.SI.4$coefficients[5]*tpaos+
#     qr.SI.4$coefficients[6]*slope+
#     qr.SI.4$coefficients[7]*asp+
#     qr.SI.4$coefficients[8]*elev+
#     qr.SI.4$coefficients[9]*slope*asp+
#     qr.SI.4$coefficients[10]*slope*elev+
#     qr.SI.4$coefficients[11]*asp*elev+
#     qr.SI.4$coefficients[12]*slope*asp*elev
# 
#   qr.pred.five <-qr.SI.5$coefficients[1]+qr.SI.5$coefficients[2]*sqht+
#     qr.SI.5$coefficients[3]*stcw+qr.SI.5$coefficients[4]*stran+
#     qr.SI.5$coefficients[5]*tpaos+
#     qr.SI.5$coefficients[6]*slope+
#     qr.SI.5$coefficients[7]*asp+
#     qr.SI.5$coefficients[8]*elev+
#     qr.SI.5$coefficients[9]*slope*asp+
#     qr.SI.5$coefficients[10]*slope*elev+
#     qr.SI.5$coefficients[11]*asp*elev+
#     qr.SI.5$coefficients[12]*slope*asp*elev
# 
#   qr.pred.six <-qr.SI.6$coefficients[1]+qr.SI.6$coefficients[2]*sqht+
#     qr.SI.6$coefficients[3]*stcw+qr.SI.6$coefficients[4]*stran+
#     qr.SI.6$coefficients[5]*tpaos+
#     qr.SI.6$coefficients[6]*slope+
#     qr.SI.6$coefficients[7]*asp+
#     qr.SI.6$coefficients[8]*elev+
#     qr.SI.6$coefficients[9]*slope*asp+
#     qr.SI.6$coefficients[10]*slope*elev+
#     qr.SI.6$coefficients[11]*asp*elev+
#     qr.SI.6$coefficients[12]*slope*asp*elev
# 
#   qr.pred.seven <-qr.SI.7$coefficients[1]+qr.SI.7$coefficients[2]*sqht+
#     qr.SI.7$coefficients[3]*stcw+qr.SI.7$coefficients[4]*stran+
#     qr.SI.7$coefficients[5]*tpaos+
#     qr.SI.7$coefficients[6]*slope+
#     qr.SI.7$coefficients[7]*asp+
#     qr.SI.7$coefficients[8]*elev+
#     qr.SI.7$coefficients[9]*slope*asp+
#     qr.SI.7$coefficients[10]*slope*elev+
#     qr.SI.7$coefficients[11]*asp*elev+
#     qr.SI.7$coefficients[12]*slope*asp*elev
# 
#   qr.pred.eight <-qr.SI.8$coefficients[1]+qr.SI.8$coefficients[2]*sqht+
#     qr.SI.8$coefficients[3]*stcw+qr.SI.8$coefficients[4]*stran+
#     qr.SI.8$coefficients[5]*tpaos+
#     qr.SI.8$coefficients[6]*slope+
#     qr.SI.8$coefficients[7]*asp+
#     qr.SI.8$coefficients[8]*elev+
#     qr.SI.8$coefficients[9]*slope*asp+
#     qr.SI.8$coefficients[10]*slope*elev+
#     qr.SI.8$coefficients[11]*asp*elev+
#     qr.SI.8$coefficients[12]*slope*asp*elev
# 
#   qr.pred.nine <-qr.SI.9$coefficients[1]+qr.SI.9$coefficients[2]*sqht+
#     qr.SI.9$coefficients[3]*stcw+qr.SI.9$coefficients[4]*stran+
#     qr.SI.9$coefficients[5]*tpaos+
#     qr.SI.9$coefficients[6]*slope+
#     qr.SI.9$coefficients[7]*asp+
#     qr.SI.9$coefficients[8]*elev+
#     qr.SI.9$coefficients[9]*slope*asp+
#     qr.SI.9$coefficients[10]*slope*elev+
#     qr.SI.9$coefficients[11]*asp*elev+
#     qr.SI.9$coefficients[12]*slope*asp*elev
# 
# 
#   ifelse(annualht>qr.pred.nine,
#          #yes
#          cat<-"9",
#          #no
#          ifelse(qr.pred.eight<annualht&&annualht<qr.pred.nine,
#                 #yes
#                 cat<- "8",
#                 #no
#                 ifelse(qr.pred.seven<annualht&&annualht<qr.pred.eight,
#                        #yes
#                        cat<-"7",
#                        #no
#                        ifelse(qr.pred.six<annualht&&annualht<qr.pred.seven,
#                               #yes
#                               cat<-"6",
#                               #no
#                               ifelse(qr.pred.five<annualht&&annualht<qr.pred.six,
#                                      #yes
#                                      cat<-"5",
#                                      #no
#                                      ifelse(qr.pred.four<annualht&&annualht<qr.pred.five,
#                                             #yes
#                                             cat<-"4",
#                                             #no
#                                             ifelse(qr.pred.three<annualht&&annualht<qr.pred.four,
#                                                    #yes
#                                                    cat<-"3",
#                                                    #no
#                                                    ifelse(qr.pred.two<annualht&&annualht<qr.pred.three,
#                                                           #yes
#                                                           cat<-"2",
#                                                           #no
#                                                           ifelse(qr.pred.one<annualht&&annualht<qr.pred.two,
#                                                                  #yes
#                                                                  cat<-"1",
#                                                                  #no
#                                                                  cat<-"0")))))))))
#   cat
# }
# 
# 
# 
# annual.gr6$response.cat<-0
# 
# annual.gr6$treeminus[is.na(annual.gr6$treeminus)] <- 0
# annual.gr6$CrownLength[is.na(annual.gr6$CrownLength)] <- 0
# 
# for(i in 1:nrow(annual.gr6)){
#   annual.gr6$response.cat[i]<-valid.func10(
#     annual.gr6$srHeight_Total[i],
#     annual.gr6$CrownLength[i],
#     annual.gr6$treeminus[i],
#     annual.gr6$TPA.OS[i],
#     annual.gr6$slopePercent[i],
#     annual.gr6$aspect[i],
#     annual.gr6$elevation[i],
#     annual.gr6$ht_annual[i])
# }
# 
# annual.gr6$count<-1
# 
# sorted.totals<-as.data.frame(xtabs(annual.gr6$count~annual.gr6$response.cat)/nrow(annual.gr6))
# 
# sum(sorted.totals$Freq)
# 
# 
# barchart(sorted.totals$Freq~sorted.totals$annual.gr6.response.cat, names = "Quantile Bin",
#          xlab = "Bin", ylab = "Frequency",type=density,
#          main = "Witheld Data Height Growth Response
#         sorted by Quantile Category", ylim=c(0,.2))

#Chi Squared test of homogeneity

# 
# x.table<-(xtabs(annual.gr6$count~annual.gr6$response.cat))
# 
# annual.gr6.lessthan1<-annual.gr6[annual.gr6$Height_Total<3,]
# annual.gr6.1to3<-annual.gr6[3<annual.gr6$Height_Total&&annual.gr6$Height_Total<5,]
# annual.gr6.3plus<-annual.gr6[annual.gr6$Height_Total>5,]
# 
# x.table1<-(xtabs(annual.gr6.lessthan1$count~annual.gr6.lessthan1$response.cat))
# x.table2<-(xtabs(annual.gr6.1to3$count~annual.gr6.1to3$response.cat))
# x.table3<-(xtabs(annual.gr6.3plus$count~annual.gr6.3plus$response.cat))
# 
# brkdwn<-rbind(x.table1,x.table2,x.table3)
# 
# min(annual.gr6$Height_Total)
# 
# hist(annual.gr6$Height_Total)
# 
# xsq<-chisq.test(x.table3)
# xsq$expected
###Higher resolution of lessthan 10in DBH


# annual.gr6.lessthan10in$response.cat<-0

#annual.gr6.lessthan10in$CrownLength[is.na(annual.gr6.lessthan10in$CrownLength)] <- 0


# for(i in 1:nrow(annual.gr6.lessthan10in)){
#   annual.gr6.lessthan10in$response.cat[i]<-valid.func10(
#     annual.gr6.lessthan10in$srHeight_Total[i], 
#     annual.gr6.lessthan10in$CrownLength[i],
#     annual.gr6.lessthan10in$treeminus[i],
#     annual.gr6.lessthan10in$TPA.OS[i],
#     annual.gr6.lessthan10in$slopePercent*aspect*elevation[i],
#     annual.gr6.lessthan10in$ht_annual[i])
# }
# 
# annual.gr6.lessthan10in$count<-1
# 
# sorted.totals<-as.data.frame(xtabs(annual.gr6.lessthan10in$count~annual.gr6.lessthan10in$response.cat)/nrow(annual.gr6.lessthan10in))
# 
# sum(sorted.totals$Freq)
# 
# 
# barchart(sorted.totals$Freq~sorted.totals$annual.gr6.lessthan10in.response.cat, names = "Quantile Bin",
#          xlab = "Bin", ylab = "Frequency",type=density,
#          main = "Witheld Data Height Growth Response 
#          sorted by Quantile Category (DBH>10in)",ylim=c(0,.3))
# 
# min(sorted.totals$Freq)
# max(sorted.totals$Freq)
# 
# #range in value from .075 to .148 in each category
# 
# 
# 
# #########Residual Plots for Q50 ########
# cw.resid <- rq(ht_annual~CrownWidth,tau=c(.5),data=annual.gr4)
#
# plot(cw.resid$residuals~cw.resid$fitted.values)
#
#
# TPA.resid <- rq(ht_annual~annual.gr4$TPA.OS,tau=c(.5),data=annual.gr4)
#
# plot(TPA.resid$residuals~TPA.resid$fitted.values)
#
# SI.resid <-rq(ht_annual~annual.gr4$slopePercent*aspect*elevation,tau=c(.5),data=annual.gr4)
#
# plot(SI.resid$residuals~SI.resid$fitted.values)
#
###Sequential GAM Plots###
# dev.off()
# 
# par(mfrow=c(2,3))
# sr.ht.gam<-gam(annual.gr4$ht_annual~s(annual.gr4$srHeight_Total))
# plot(sr.ht.gam,residuals=T,se=T,pch=".",ask=F)
# 
# cl.ht.gam<-gam(annual.gr4$ht_annual~annual.gr4$srHeight_Total+s(annual.gr4$CrownLength))
# plot(cl.ht.gam,residuals=T,se=T,pch=".",ask=F)
# 
# veg.ht.gam<-gam(annual.gr4$ht_annual~annual.gr4$srHeight_Total+annual.gr4$CrownLength)
# plot(veg.ht.gam,residuals=T,se=T,pch=".",ask=F)
# 
# 
# tpa.os.gam<-gam(annual.gr4$ht_annual~annual.gr4$srHeight_Total+annual.gr4$CrownLength+
#            s(annual.gr4$TPA.OS))
# plot(tpa.os.gam,residuals=T,se=T,pch=".",ask=F)

#should each SEA term be added as a smoothed term?

# si.gam<-gam(ht_annual~srHeight_Total+CrownLength+TPA.OS+
#              #how to make this below a smoothed term
#              slopePercent + # goes with coefficient b1
#              slopePercent:cos_rad_asp + #with b2
#              slopePercent:sin_rad_asp + #with b3
#              slopePercent:log(elevation+1) + #b4
#              slopePercent:log(elevation+1):cos_rad_asp + #b5
#              slopePercent:log(elevation+1):sin_rad_asp + #b6
#              slopePercent:I(elevation^2) +   #b7
#              slopePercent:I(elevation^2):cos_rad_asp +   #b8
#              slopePercent:I(elevation^2):sin_rad_asp +   #b9
#              elevation + # b10
#              I(elevation^2) ,data=annual.gr4) #b11
# 
# plot(si.gam,residuals=T,se=T,pch=".",ask=F)


#Table of SI and SEA values
# unique(annual.gr4[,c("Installation","SiteIndex_Value"])
