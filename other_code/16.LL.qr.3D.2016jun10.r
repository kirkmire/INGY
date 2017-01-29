source('C:/Users/Colin/Desktop/R-Projects/INGY/11.Quant_reg_LL.2016jun10.r')
source('C:/Users/Colin/Desktop/R-Projects/INGY/13.veg_volumes_by_treatment.r')

###Below is copied from 16.LL.lines w/ temporal adjustments for 2004-2008
merged_stagm_stag <- merge(stagm, stag,by=c("Installation","Plot","STP","Tree"))

LL<-merged_stagm_stag[merged_stagm_stag$Installation=="LL"&
                        merged_stagm_stag$Species=="PIPO"&
                        merged_stagm_stag$Plot%in%c(1:7)&
                        merged_stagm_stag$Year_Measurement%in%c(2004,2008),]

#removing certain trees
#Appears to be errors in data entry
#Tree 663 goes from 8.5ft to 1.6ft, should read 9.6ft
LL$Height_Total[LL$Year_Measurement==2008&LL$Plot==1&LL$Tree==663]<-9.6
#Tree 53 goes from 8.6ft to 1.5ft, should read 9.5ft
LL$Height_Total[LL$Plot==3&LL$Tree==53&LL$Year_Measurement==2008]<-9.5
#Tree 19, Plot 3 has Height total of 0 for 2008 measurement, removed#
LL<-LL[!(LL$Plot=="3"&LL$Tree=="19"),]
#Tree 242, Plot 6 has Height total of 0 for 2008 measurement, removed#
LL<-LL[!(LL$Plot=="6"&LL$Tree=="242"),]
#tree 683 plot 4 stp 5#
LL<-LL[!(LL$Plot=="4"&LL$Tree=="683"),]
#tree 111 plot 1 stp 6#
LL<-LL[!(LL$Plot=="1"&LL$Tree=="111"),]

#Select only tagged trees from 2004 (initial meas)
LL1_init<-(LL[LL$Year_Measurement=="2004",])

#Note that tree numbers are not unique
length(unique(LL1_init$Tree))

#Random sample of 50 PIPO trees from LL1_init
LL_samp<-LL1_init[sample(nrow(LL1_init),50),]

#Select only rows for which Tree==Tree
LL<-LL[LL$Plot %in% LL_samp$Plot&LL$STP %in%LL_samp$STP&
         LL$Tree %in% LL_samp$Tree,]


LL1_both<-merge(LL1_init,LL, by=c("Plot","STP","Tree"))

#Create H-H inc column

LL1_both$inc<-LL1_both$Height_Total.y-LL1_both$Height_Total.x


#Merge with splot for treatment column#
LL1_both<-merge(LL1_both,splot,by.x=c("Installation.x","Plot"),
                by.y=c("Installation","Plot"))


#Merging with veg volume from script13#

LL_both<-merge(LL_veg_2004,LL1_both,by=("Treatment"))

#removing all 2004
LL_both<-LL_both[!LL_both$Year_Measurement.y==2004,]


#Finding the sum of all crown widths by plot and stp#

CW<-aggregate(LL_both$CrownWidth.x~LL_both$Plot+LL_both$STP, FUN=sum)

LL_both<-merge(CW,LL_both,by.y=c("Plot","STP"),by.x=c("LL_both$Plot","LL_both$STP"))

#veg
Veg<-aggregate(LL_plots_veg$Coverage~
                 LL_plots_veg$Plot+LL_plots_veg$STP, FUN=mean)


LL_both<-merge(Veg,LL_both,by.y=c("LL_both$Plot","LL_both$STP"),
               by.x=c("LL_plots_veg$Plot","LL_plots_veg$STP"))



#trying rgl#
library(rgl)

#myColorRamp <- function(colors, values) {
#  v <- (values - min(values))/diff(range(values))
 # x <- colorRamp(colors)(v)
#  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
#}

plot3d(LL_both$`LL_both$CrownWidth.x`, LL_both$`LL_plots_veg$Coverage`,
       LL_both$inc, 
       axes=FALSE,size=7,
       col="black",
       xlab="", ylab="", zlab="")
axes3d(c("x+", "y-", "z-"))
grid3d(side=c('x+', 'y-', 'z'), col="gray")
title3d(main="Loon Lake QR of Ht Inc on % Vegetative Volume and Sum of CW",
        ylab = "Ave Percent. Cover Veg. by STP",
        zlab = "Height Growth Increment (ft)",
        xlab = "Sum of CW by STP",
        col="black",cex.main=2)

#Adding vertical droplines#
#plot3d(threeDVeg$SiteIndex_Value,threeDVeg$ave.BAPA,threeDVeg$volume,type='h',add=T,
       #col=myColorRamp(c("blue","green","yellow","red"),threeDVeg$volume))

#Fitting&Plotting RQ Planes#
library(quantreg)

#QR for Median#
fit5=rq(LL_both$inc~LL_both$`LL_both$CrownWidth.x`+LL_both$`LL_plots_veg$Coverage`, tau=.5)
summary(fit5)

#QR for Median
coefs <- coef(fit5)
planes3d(a=coefs["LL_both$`LL_both$CrownWidth.x`"], b=coefs["LL_both$`LL_plots_veg$Coverage`"],
    -1, coefs["(Intercept)"], alpha=0.50, col="plum2")

#QR for .10
fit1=rq(LL_both$inc~LL_both$`LL_both$CrownWidth.x`+LL_both$`LL_plots_veg$Coverage`, tau=.1)
summary(fit1)

coefs <- coef(fit1)
planes3d(a=coefs["LL_both$`LL_both$CrownWidth.x`"], b=coefs["LL_both$`LL_plots_veg$Coverage`"],
         -1, coefs["(Intercept)"], alpha=0.50, col="red")

#QR for .95
fit9=rq(LL_both$inc~LL_both$`LL_both$CrownWidth.x`+LL_both$`LL_plots_veg$Coverage`, tau=.95)
summary(fit9)

coefs <- coef(fit9)
planes3d(a=coefs["LL_both$`LL_both$CrownWidth.x`"], b=coefs["LL_both$`LL_plots_veg$Coverage`"],
         -1, coefs["(Intercept)"], alpha=0.5, col="blue")


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
