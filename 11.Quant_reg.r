#selecting Installations
#LL has most PIPO tagged trees
#CTRL Plots are 7 and 3

merged_stagm_stag <- merge(stagm, stag,by=c("Installation","Plot","STP","Tree"))

LL<-merged_stagm_stag[merged_stagm_stag$Installation=="LL"&merged_stagm_stag$Species=="PIPO"&merged_stagm_stag$Plot%in%c(7,3)&
                        merged_stagm_stag$Year_Measurement%in%c(2001,2008),]


                      
                   



dev.off()




LL_initial<-LL$Height_Total[LL$Year_Measurement==2001]
LL_growth<-LL$Height_Total[LL$Year_Measurement==2008]


library(quantreg)

help(rq)

plot(LL_initial,LL_growth,cex=.25,type="n",xlab="Initial Height",ylab="Hieght Growth")
points(LL_initial,LL_growth,cex=.5,col="blue")
abline(rq(LL_growth~LL_initial, tau=.5),col="blue")
abline(lm(LL_growth~LL_initial),lty=2,col="red")
taus<-c(.05,.1,.25,.75,.9,.95)
for(i in 1:length(taus)){
  abline(rq(LL_growth~LL_initial,tau=taus[i]),col="gray")
}
fit2<-summary(rq(LL_growth~LL_initial,tau=c(.05,.25,.5,.75,.95)))

