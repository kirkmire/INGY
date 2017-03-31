
#Reads in previous scripts required (takes ~10min)
source(paste(getwd(),'/1.readdatabase.2016jun2.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/other_code/18.database.error.corrections.2016jun16.r',sep=""), echo=TRUE)
source(paste(getwd(),'/1.1annualizedht.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.2.UT.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.3.UV.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.4.OS.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.5.SQ.variable.selection.2016.dec.16.r',sep = ""), echo=TRUE)
source(paste(getwd(),'/1.6.1.annual.gr.6.withelddata.2016.dec.2016.r',sep=""), echo=TRUE)
source(paste(getwd(),'/7.ge.ctrl.veg.2016jun2.r',sep=""), echo=TRUE)



#combines all aic.lists into one dataframe

# colnames(UT.aic)<-c("Variable","n","AIC")
# 
# colnames(UV.aic)<-c("Variable","n","AIC")
# 
# colnames(OS.aic)<-c("Variable","n","AIC")
# 
# colnames(SQ.aic)<-c("Variable","n","AIC")
# 
# aic.lists<-rbind(UT.aic,UV.aic,OS.aic,SQ.aic)
# 
# aic.lists$AIC<-round(aic.lists$AIC,2)
# 
# final.aic<-data.frame(matrix("", nrow = 14, ncol = 9),stringsAsFactors=F)
# final.aic$X1[1:9]<-as.character(aic.lists$Variable[1:9])
# final.aic$X2[1:9]<-aic.lists$n[1:9]
# final.aic$X3[1:9]<-aic.lists$AIC[1:9]
# final.aic$X4[1:13]<-as.character(aic.lists$Variable[10:22])
# final.aic$X5[1:13]<-aic.lists$n[10:22]
# final.aic$X6[1:13]<-aic.lists$AIC[10:22]
# final.aic$X7[1:4]<-as.character(aic.lists$Variable[23:26])
# final.aic$X8[1:4]<-aic.lists$n[23:26]
# final.aic$X9[1:4]<-aic.lists$AIC[23:26]
# final.aic$X7[5]<-"Site"
# final.aic$X8[5]<-"n"
# final.aic$X9[5]<-"AIC"
# final.aic$X7[6:10]<-as.character(aic.lists$Variable[27:31])
# final.aic$X8[6:10]<-aic.lists$n[27:31]
# final.aic$X9[6:10]<-aic.lists$AIC[27:31]



#The code below will produce output that can then be copied over to the .tex file
# library(Hmisc)
# 
# latex(final.aic, file="")            # If you want all the data
#
#
#
# qr.sum<-data.frame(qr.SI.all$coefficients)
#
# latex(qr.sum,file="")


# annual.gr4<-annual.gr4[!is.na(annual.gr4$cratio)==T,]
# 
# 
# 
# #Would it be okay to use a mixed model in variable selection to account for within group
# #(subject) correlation and then use regular rq in prediction?
# 
# 
# annual.gr6<-annual.gr6[!is.na(annual.gr6$cratio)==T,]
# annual.gr6<-annual.gr6[!is.na(annual.gr6$ht_annual)==T,]
# 
# 
# 
# 
# 
# library(quantreg)
# least_squares <-lm(ht_annual~srHeight_Total+
#                cratio+
#                TPA.OS+
#                slopePercent +
#                slopePercent:cos_rad_asp +
#                slopePercent:sin_rad_asp +
#                slopePercent:log(elevation+1) +
#                slopePercent:log(elevation+1):cos_rad_asp +
#                slopePercent:log(elevation+1):sin_rad_asp +
#                slopePercent:I(elevation^2) +
#                slopePercent:I(elevation^2):cos_rad_asp +
#                slopePercent:I(elevation^2):sin_rad_asp +
#                elevation +
#                I(elevation^2) ,
#             data=annual.gr4)
# 
# 
# qr.SI.1 <-rq(ht_annual~srHeight_Total+
#                cratio+
#                TPA.OS+
#                slopePercent +
#                slopePercent:cos_rad_asp +
#                slopePercent:sin_rad_asp +
#                slopePercent:log(elevation+1) +
#                slopePercent:log(elevation+1):cos_rad_asp +
#                slopePercent:log(elevation+1):sin_rad_asp +
#                slopePercent:I(elevation^2) +
#                slopePercent:I(elevation^2):cos_rad_asp +
#                slopePercent:I(elevation^2):sin_rad_asp +
#                elevation +
#                I(elevation^2) ,
#              tau=.1 ,  data=annual.gr4)
# 
# 
# # qr.SI.all <-rq(ht_annual~srHeight_Total+
# #                cratio+
# #                TPA.OS+
# #                slopePercent +
# #                slopePercent:cos_rad_asp +
# #                slopePercent:sin_rad_asp +
# #                slopePercent:log(elevation+1) +
# #                slopePercent:log(elevation+1):cos_rad_asp +
# #                slopePercent:log(elevation+1):sin_rad_asp +
# #                slopePercent:I(elevation^2) +
# #                slopePercent:I(elevation^2):cos_rad_asp +
# #                slopePercent:I(elevation^2):sin_rad_asp +
# #                elevation +
# #                (elevation^2) ,
# #              tau=c(1:9/10) ,  data=annual.gr4)
# 
# # qr.SI.all <-rq(ht_annual~srHeight_Total+
# #                  cratio+
# #                  TPA.OS+
# #                  SiteIndex_Value ,
# #                tau=c(1:9/10) ,  data=annual.gr4)
# 
# 
# # summary(qr.SI.all)
# 
# 
# # plot(summary(qr.SI.all),parm="TPA.OS")
# # plot(summary(qr.SI.all),parm="cratio")
# # plot(summary(qr.SI.all),parm="srHeight_Total")
# 
# 
# 
# 
# qr.SI.5 <-rq(ht_annual~srHeight_Total+
#                cratio+
#                TPA.OS+
#                slopePercent +
#                slopePercent:cos_rad_asp +
#                slopePercent:sin_rad_asp +
#                slopePercent:log(elevation+1) +
#                slopePercent:log(elevation+1):cos_rad_asp +
#                slopePercent:log(elevation+1):sin_rad_asp +
#                slopePercent:I(elevation^2) +
#                slopePercent:I(elevation^2):cos_rad_asp +
#                slopePercent:I(elevation^2):sin_rad_asp +
#                elevation +
#                I(elevation^2) ,
#              tau=.5,  data=annual.gr4)
# 
# qr.SI.9 <-rq(ht_annual~srHeight_Total+
#                cratio+
#                TPA.OS+
#                slopePercent +
#                slopePercent:cos_rad_asp +
#                slopePercent:sin_rad_asp +
#                slopePercent:log(elevation+1) +
#                slopePercent:log(elevation+1):cos_rad_asp +
#                slopePercent:log(elevation+1):sin_rad_asp +
#                slopePercent:I(elevation^2) +
#                slopePercent:I(elevation^2):cos_rad_asp +
#                slopePercent:I(elevation^2):sin_rad_asp +
#                elevation +
#                I(elevation^2) ,
#              tau=.9 ,  data=annual.gr4)
# 
# 
# 
# annual.gr6$qr.pred.one <- predict.rq(qr.SI.1, annual.gr6)
# annual.gr6$qr.pred.five <- predict.rq(qr.SI.5, annual.gr6)
# annual.gr6$qr.pred.nine <- predict.rq(qr.SI.9, annual.gr6)
# 
# 
#     
# valid.func<-function(annualht,qr.pred.one,qr.pred.five,qr.pred.nine){
#   # annualht<-1.2
#   # qr.pred.one<-1
#   # qr.pred.five<-1.6
#   # qr.pred.nine<-1.6
#   
#  ifelse(annualht>qr.pred.nine,
#        #yes
#        cat<-"top10",
#        #no
#        ifelse(qr.pred.five<annualht&&annualht<qr.pred.nine,
#               #yes
#               cat<- "fiftytoninety",
#               #no
#               ifelse(qr.pred.one<annualht&&annualht<qr.pred.five,
#                      cat<-"bottom10tofifty",
#                      cat<-"bottom10")))
# 
# cat
# 
# }
# 
# annual.gr6$response.cat<-0
# 
# for(i in 1:nrow(annual.gr6)){
#   annual.gr6$response.cat[i]<-valid.func(
#     annual.gr6$ht_annual[i],
#     annual.gr6$qr.pred.one[i],
#     annual.gr6$qr.pred.five[i],
#     annual.gr6$qr.pred.nine[i])
# 
# }
# 
# library(plyr)
# sorted.totals<-count(annual.gr6, 'response.cat')
# 
# 
# library(RColorBrewer)
# library(lattice)
# 
# 
# 
# barchart(sorted.totals$freq/length(annual.gr6$InstPlot)~sorted.totals$response.cat, names = "Quantile Bin",
#         xlab = "Bin", ylab = "Fraction of Total Witheld Trees",type=density,
#         main = "Witheld Data Height Growth Response
#         sorted by Quantile Category",ylim=c(0,.60),
#         par.settings = my.settings,
#         par.strip.text=list(col="white", font=2),
#         panel=function(x,y,...){
#           panel.grid(h=-1, v=0); 
#           panel.barchart(x,y,...)
#         })

#Chi Squared test of homogeneity

# 
# annual.gr6.lessthan1<-annual.gr6[annual.gr6$Height_Total<4,]
# annual.gr6.1to3<-annual.gr6[4<annual.gr6$Height_Total&annual.gr6$Height_Total<6,]
# annual.gr6.3plus<-annual.gr6[annual.gr6$Height_Total>6,]
# 
# x.table1<-t(count(annual.gr6.lessthan1, 'response.cat'))
# x.table1<-x.table1["freq",]
# x.table1<-as.numeric(x.table1)
# 
# x.table2<-t(count(annual.gr6.1to3, 'response.cat'))
# x.table2<-x.table2["freq",]
# x.table2<-as.numeric(x.table2)
# 
# x.table3<-t(count(annual.gr6.3plus, 'response.cat'))
# x.table3<-x.table3["freq",]
# x.table3<-as.numeric(x.table3)
# 
# brkdwn<-rbind(x.table1,x.table2,x.table3)
# 
# 
# min(annual.gr6$Height_Total)
# 
# hist(annual.gr6$Height_Total)
# 
# xsq1<-chisq.test(x.table1,p=c(.1,.4,.4,.1))
# xsq1
# xsq2<-chisq.test(x.table2,p=c(.1,.4,.4,.1))
# xsq2
# xsq3<-chisq.test(x.table3,p=c(.1,.4,.4,.1))
# xsq3
# #Exact test
# 
# library(XNomial)
# xmulti1<-xmulti(x.table1,c(.1,.4,.4,.1),"Chisq")
# xmulti2<-xmulti(x.table2,c(.1,.4,.4,.1),"Chisq")
# xmulti3<-xmulti(x.table3,c(.1,.4,.4,.1),"Chisq")
# 
#  
# chisq<-cbind(xsq1$p.value,xsq2$p.value,xsq3$p.value)
# xact<-cbind(xmulti1$pChi,xmulti2$pChi,xmulti3$pChi)
# 
# chitable<-rbind(chisq,xact)
# chitable<-round(chitable,digits=4)
# 
# rownames(chitable)<-c("ChiSq","Xact")
# colnames(chitable)<-c("<4","4-6",">6")
# 
# #The code below will produce output that can then be copied over to the .tex file
# library(Hmisc)
# 
# latex(chitable, file="")     
# 
# ###
# actual<-rbind(x.table1,x.table2,x.table3)
# rownames(actual)<-c("<4","4-6",">6")
# colnames(actual)<-c("<.1",".1to.5",".5tpo.9",">.9")
# 
# 
# #The code below will produce output that can then be copied over to the .tex file
# library(Hmisc)
# 
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
