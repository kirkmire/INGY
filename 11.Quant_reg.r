#selecting Installations
#LL has most PIPO tagged trees
#CTRL Plots are 7 and 3

merged_stagm_stag <- merge(stagm, stag,by=c("Installation","Plot","STP","Tree"))

LL<-merged_stagm_stag[merged_stagm_stag$Installation=="LL"&merged_stagm_stag$Species=="PIPO"&merged_stagm_stag$Plot%in%c(1:7)&
                        merged_stagm_stag$Year_Measurement%in%c(2004,2008),]

(min(LL$Year_Measurement))
                   

library(plyr)

test_df<-ddply(LL, .(LL$Installation, LL$Plot, LL$STP, LL$Tree), summarize, -diff(Height_Total))



dev.off()


#LL_initial<-LL$Height_Total[LL$Year_Measurement==2004]
#LL_growth<-LL$Height_Total[LL$Year_Measurement==2008]


library(quantreg)

help(rq)

plot(LL_initial,test_df$..1,cex=.25,type="n",xlab="Initial Height",ylab="Height Growth")
points(LL_initial,test_df$..1,cex=.5,col="blue")
abline(rq(test_df$..1~LL_initial, tau=.5),col="blue")
abline(lm(test_df$..1~LL_initial),lty=2,col="red")
taus<-c(.05,.1,.25,.75,.9,.95)
for(i in 1:length(taus)){
  abline(rq(test_df$..1~LL_initial,tau=taus[i]),col="gray")
}
fit2<-summary(rq(test_df$..1~LL_initial,tau=c(.05,.25,.5,.75,.95)))

