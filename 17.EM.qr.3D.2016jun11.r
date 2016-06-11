source('C:/Users/Colin/Desktop/R-Projects/INGY/13.veg_volumes_by_treatment.r')
source('C:/Users/Colin/Desktop/R-Projects/INGY/4.explor.plots.2016.jun2.r')

###Below is copied from 11.1 EM QR w/ temporal adjustments for 2002-2006

merged_stagm_stag <- merge(stagm, stag,by=c("Installation","Plot","STP","Tree"))

#Refer to meast (measurement timeline for appropriate meas years)
EM<-merged_stagm_stag[merged_stagm_stag$Installation=="EM"&
                        merged_stagm_stag$Species=="PIPO"&
                        merged_stagm_stag$Plot%in%c(1:7)&
                        merged_stagm_stag$Year_Measurement%in%c(2002,2006),]




#removing certain trees
#Appears to be errors in data entry
#Tree 7, Plot 732 goes from 10.2 to 7ft, removed
EM<-EM[!(EM$Plot=="7"&EM$Tree=="732"),]

#Select only tagged trees from 2004 (initial meas)
EM1_init<-(EM[EM$Year_Measurement=="2002",])

#Note that tree numbers are unique
length(unique(EM1_init$Tree))

#Random sample of 50 PIPO trees from LL1_init
EM_samp<-EM1_init[sample(nrow(EM1_init),50),]

#Select only rows for which Tree==Tree
EM<-EM[EM$Plot %in% EM_samp$Plot&EM$STP %in%EM_samp$STP&
         EM$Tree %in% EM_samp$Tree,]


EM1_both<-merge(EM1_init,EM, by=c("Plot","STP","Tree"))

#Create H-H inc column

EM1_both$inc<-EM1_both$Height_Total.x-EM1_both$Height_Total.y


#Merge with splot for treatment column#
#EM1_both<-merge(EM1_both,splot,by.x=c("Installation.x","Plot"),
#          by.y=c("Installation","Plot"))

#removing all 2002 rows
EM1_both<-EM1_both[!EM1_both$Year_Measurement.y==2002,]

#Finding the mean coverage by plot and stp#
#veg
Veg<-aggregate(EM_plots_veg$Coverage~
                 EM_plots_veg$Plot+EM_plots_veg$STP, FUN=mean)


EM1_both<-merge(Veg,EM1_both,by.y=c("Plot","STP"),
               by.x=c("EM_plots_veg$Plot","EM_plots_veg$STP"))



#EM Basal area in large trees by Plot#

EM1_both<-merge(BAPA.by.plot,EM1_both, by.x=c("Installation","Plot"),
      by.y=c("Installation.x","EM_plots_veg$Plot"))



#trying rgl#
library(rgl)

#myColorRamp <- function(colors, values) {
#  v <- (values - min(values))/diff(range(values))
# x <- colorRamp(colors)(v)
#  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
#}

plot3d(EM1_both$BAPA,
       EM1_both$`EM_plots_veg$Coverage`,
       EM1_both$inc,
       axes=FALSE,size=7,
       col="black",
       xlab="", ylab="", zlab="")
axes3d(c("x+", "y-", "z-"))
grid3d(side=c('x+', 'y-', 'z'), col="gray")
title3d(main="Loon Lake QR of Ht Inc on % Vegetative Volume and Sum of CW",
        ylab = "Ave Percent. Cover Veg. by STP",
        zlab = "Height Growth Increment (ft)",
        xlab = "BAPA by Plot",
        col="black",cex.main=2)

#Adding vertical droplines#
#plot3d(threeDVeg$SiteIndex_Value,threeDVeg$ave.BAPA,threeDVeg$volume,type='h',add=T,
#col=myColorRamp(c("blue","green","yellow","red"),threeDVeg$volume))

#Fitting&Plotting RQ Planes#
library(quantreg)

#QR for Median#
fit5=rq(EM_both$inc~EM1_both$BAPA+EM_both$`EM_plots_veg$Coverage`, tau=.5)
summary(fit5)

coefs <- coef(fit5)
planes3d(a=coefs["EM1_both$BAPA"], b=coefs["EM_both$`EM_plots_veg$Coverage`"],
         -1, coefs["(Intercept)"], alpha=0.50, col="plum2")

#QR for .10
fit1=rq(EM_both$inc~EM1_both$BAPA+EM_both$`EM_plots_veg$Coverage`, tau=.10)
summary(fit1)

coefs <- coef(fit1)
planes3d(a=coefs["EM1_both$BAPA"], b=coefs["EM_both$`EM_plots_veg$Coverage`"],
         -1, coefs["(Intercept)"], alpha=0.50, col="red")

#QR for .95
fit9=rq(EM_both$inc~EM1_both$BAPA+EM_both$`EM_plots_veg$Coverage`, tau=.95)
summary(fit9)

coefs <- coef(fit9)
planes3d(a=coefs["EM1_both$BAPA"], b=coefs["EM_both$`EM_plots_veg$Coverage`"],
         -1, coefs["(Intercept)"], alpha=0.50, col="blue")



#making a movie#

# Create a movie

movie3d(spin3d(axis = c(0, 0, 1)), duration = 15,
        dir = getwd())

getwd()

#Extracts coordinates from 3D plot for installation labels#
#s3D.coords <- s3D$xyz.convert(threeDVeg$BAPA.inst,threeDVeg$SiteIndex_Value,threeDVeg$volume)
#text(s3D.coords$x, s3D.coords$y,     # x and y coordinates
#     labels=(threeDVeg$Installation),       # text to plot
#    pos=4, cex=.5)    

dev.off()