
int1<-read.csv("C:/Users/Colin/Desktop/R-Projects/INGY/GEE_project/1STCV_plot_ndvi_med.csv")
int2<-read.csv("C:/Users/Colin/Desktop/R-Projects/INGY/GEE_project/2STCV_plot_ndvi_med.csv")
int3<-read.csv("C:/Users/Colin/Desktop/R-Projects/INGY/GEE_project/3STCV_plot_ndvi_med.csv")
int4<-read.csv("C:/Users/Colin/Desktop/R-Projects/INGY/GEE_project/4STCV_plot_ndvi_med.csv")


library(RColorBrewer)

myColours <- brewer.pal(6,"Greys")
my.settings <- list(
  superpose.polygon=list(col=myColours[2:5], border="transparent"),
  strip.background=list(col=myColours[6]),
  strip.border=list(col="black"))



int1$int<-1
int2$int<-2
int3$int<-3
int4$int<-4


ndvi_merge<-annual.gr4


year.func<-function(year){
  ifelse(year<2003,
         int<-1,
         ifelse(2007>year&&year>2003,
                #yes
                int<-2,
                #no
                ifelse(2011>year&&year>2007,
                       int<-3,
                       int<-4)))
  int
}

ndvi_merge$int<-0

for(i in 1:nrow(ndvi_merge)){
  ndvi_merge$int[i]<-year.func(
    ndvi_merge$Year_Measurement[i])
  
}


rbind_meds<-rbind(int1,int2,int3,int4)


colnames(rbind_meds)[2]<-"InstPlot"

ndvi_merged<-merge(rbind_meds,ndvi_merge,by=c("InstPlot","int"))

library(lattice)

xyplot(ndvi_merged$p95~ndvi_merged$Treatment|ndvi_merged$Installation,group=ndvi_merged$int,auto.key = T)

ndvi_merged$Treatment.x<-factor(ndvi_merged$Treatment)
xyplot(ndvi_merged$p95~ndvi_merged$Year_Measurement|ndvi_merged$Installation,
       group=ndvi_merged$Treatment,auto.key = T,pch=16)

plot(ndvi_merged$p95~ndvi_merged$TPA.OS,col=ndvi_merged$Treatment.x)
abline(ge,col="red")
abline(ctrl,col="green")
abline(one)
ge<-lm(ndvi_merged$p95[ndvi_merged$Treatment.x=="GE"]
       ~ndvi_merged$TPA.OS[ndvi_merged$Treatment.x=="GE"])
ctrl<-lm(ndvi_merged$p95[ndvi_merged$Treatment.x=="CTRL"]
         ~ndvi_merged$TPA.OS[ndvi_merged$Treatment.x=="CTRL"])
one<-lm(ndvi_merged$p95[ndvi_merged$Treatment.x=="1X"]
        ~ndvi_merged$TPA.OS[ndvi_merged$Treatment.x=="1X"])


plot(ndvi_merged$p95~ndvi_merged$Year_Measurement,col=ndvi_merged$Treatment.x)
abline(ge,col="red")
abline(ctrl,col="green")
abline(one)
ge<-lm(ndvi_merged$p95[ndvi_merged$Treatment.x=="GE"]
       ~ndvi_merged$Year_Measurement[ndvi_merged$Treatment.x=="GE"])
ctrl<-lm(ndvi_merged$p95[ndvi_merged$Treatment.x=="CTRL"]
         ~ndvi_merged$Year_Measurement[ndvi_merged$Treatment.x=="CTRL"])
one<-lm(ndvi_merged$p95[ndvi_merged$Treatment.x=="1X"]
        ~ndvi_merged$Year_Measurement[ndvi_merged$Treatment.x=="1X"])



xyplot(ndvi_merged$p95~ndvi_merged$bapa.OS,group=ndvi_merged$Treatment.x,auto.key = T)

lm.os.trt<-lm(ndvi_merged$p95~ndvi_merged$TPA.OS+ndvi_merged$Treatment.x+ndvi_merged$Installation)

summary(lm.os.trt)

#below shows all plots, not just those with tagged trees
Plots.lat.long$InstPlot<-paste(Plots.lat.long$Installation,Plots.lat.long$Plot,sep="")
ndvi_merged_all<-merge(rbind_meds,Plots.lat.long,by=c("InstPlot"))
ndvi_merged_all<-ndvi_merged_all[!ndvi_merged_all$Installation %in% drp60,]

xyplot(ndvi_merged_all$p95~ndvi_merged_all$int|ndvi_merged_all$Installation
       ,group=ndvi_merged_all$Treatment,auto.key = T,pch=16,type=c("p","r"), 
       strip = strip.custom(bg="lightgrey"),
       # col=myColours[4],
       par.settings = my.settings)