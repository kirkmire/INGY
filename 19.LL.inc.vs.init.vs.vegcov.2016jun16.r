# need to run step 1 to get data
source("1.readdatabase.2016jun2.r")
source('18.database.error.corrections.2016jun16.r')

# identify small tree forms of damage that we'll drop
levels(stagm$Damage)
bad.damage <- c("BIG TREE ON TOP OF IT","CRUSHED","CUT",
                "CUT AND DEAD","CUT AND DOWN","CUT AND GONE",
                "CUT TOP","CUT/DEAD","DEAD","DEAD-CRUSHED",
                "DEAD-CUT","DEAD-CUT DOWN","DEAD ","DEAD  ","DEAD BS",
                "DEAD GALL","DEAD NO TAG","DEAD UNDER LOG","DEAD/ GALL",
                "DEAD/ GALL RUST","DEAD/ MIA","DEAD/BROWSED",
                "DEAD/GALL RUST","DEAD/LOGGING","DEAD/MIA",
                "GALL DEAD","KILLED BY LOG/ MIA","LARGE TREE FELL ON TOP",
                "LEAN DEAD","lOGGING kILLED","MIA","MIA/LOGGING",
                "SMUSHED BY TREE","TOPPED BY LOGGING","TREE FELL ON IT",
                "TREE FELL ON","UPROOTED & DYING")

get.interval <- function(install,yr1,yr2){
  # install <- "LL"; yr1 <- 2004; yr2<-2008
  
  #tagged tree stuff
  tagged1 <- stagm[stagm$Installation==install &
                     stagm$Year_Measurement==yr1,
                   c(2:10,12,14,15)]
  tagged2 <- stagm[stagm$Installation==install &
                     stagm$Year_Measurement==yr2,
                   c(2:10,12,14,15)]
  tagged <- merge(tagged1,tagged2,
                  by=c("Installation","Plot","STP","Tree"))
  
  tagged$yr1 <- tagged$Year_Growth.x
  tagged$yr.inc <- yr2-yr1
  
  tagged$init.ht <- tagged$Height_Total.x
  tagged$init.cw <- tagged$CrownWidth.x
  tagged$init.cr <- with(tagged,(Height_Total.x-Height_CrownBase.x)/Height_Total.x)
  tagged$ht.inc <- with(tagged,Height_Total.y - Height_Total.x)/(yr2-yr1)
  tagged$dam1 <- tagged$Damage.x
  tagged$dam2 <- tagged$Damage.y
  
  tagged <- tagged[,c("Installation","Plot","STP","Tree",
                      "yr1","yr.inc","init.ht","init.cw","init.cr","ht.inc",
                      "dam1","dam2")]
  tagged <- tagged[!(tagged$dam1 %in% bad.damage) &
                     !(tagged$dam2 %in% bad.damage),]
  
  # add species info
  tagged <- merge(tagged,stag[,c(2:6)],all.x=T)
  
  # add treatment type
  tagged <- merge(tagged,splot[,c(2:4)])
  
  # add overstory (ba, tpa)
  over <- soverhist[soverhist$Installation==install &
                      !(soverhist$Damage %in% c("Dead","DEAD","DEAD-CUT",
                                                "DEAD-CUT DOWN","CUT","Cut","MIA")),]
  yrs <- unique(over$Year_Measurement)
  closest.yr <- yrs[which.min(abs(yrs-yr1))]
  over <- over[over$Year_Measurement==closest.yr,]
  over$tpa <- with(over, 0 + (DBH<10.5)/.26 + (DBH>=10.5)/.46)  # is this right over time?
  over$bpa <- with(over, tpa*pi*(DBH^2)/144/4)
  oversumm <- aggregate(cbind(tpa,bpa) ~ Plot, data=over, sum)
  tagged <- merge(tagged,oversumm)
  
  # add 1 m2 polyveg
  onemeter <- sstp1[sstp1$Installation==install & 
                      sstp1$Year_Measurement==yr1 &
                      sstp1$Lifeform!="POLV",]
  onemetersumm <- aggregate(Coverage ~ Plot+STP, data=onemeter, sum, na.rm=T)
  tagged <- merge(tagged,onemetersumm)
  
  tagged
}



# get the LL data
LL1 <- get.interval("LL",2001,2004)
LL2 <- get.interval("LL",2004,2008)
LL3 <- get.interval("LL",2008,2012)

# combine
LL <- rbind(LL1,LL2,LL3)

# plot
xyplot(ht.inc ~ init.ht | Species, data=LL, groups=yr1,
       auto.key = list(space = "top",columns=3))


# plot some quantile response functions in 3D
library(quantreg)
library(rgl)
incdata <- LL[LL$Species=="PIPO",]
basicqr <- rq(ht.inc ~ sqrt(init.ht)*Coverage, data=incdata,
              tau=c(.1,.5,.9))

plot3d(incdata$init.ht,incdata$Coverage,incdata$ht.inc,
       xlab="Initial height",
       ylab="Understory veg cover",
       zlab="Height increment")

xs <- seq(min(incdata$init.ht,na.rm=T),max(incdata$init.ht,na.rm=T),length=30)
ys <- seq(min(incdata$Coverage,na.rm=T),max(incdata$Coverage,na.rm=T),length=30)
zs <- function(tau.column){
  matrix(coef(basicqr)[1,tau.column] + 
           coef(basicqr)[2,tau.column]*rep(sqrt(xs),each=length(ys)) +
           coef(basicqr)[3,tau.column]*rep(ys,length(xs)) + 
           coef(basicqr)[4,tau.column]*rep(sqrt(xs),each=length(ys))*rep(ys,length(xs)),
         nrow=length(xs),byrow=T)
}

surface3d(xs,ys,zs(2),col="grey",alpha=.5)
surface3d(xs,ys,zs(1),col="blue",alpha=.5)
surface3d(xs,ys,zs(3),col="red",alpha=.5)

#making a movie#

# Create a movie

#movie3d(spin3d(axis = c(0, 0, 1),rpm=4), duration = 15,
        #dir = getwd())

getwd()
dev.off()

library(quantreg)

plot(incdata$init.ht, incdata$ht.inc, xlab="Initial height",
     ylab="Annualized Height Increment (ft)",
     main="Annualized Height Increment vs Initial Height (ft)")


sq_rq05<-rq(incdata$ht.inc~sqrt(incdata$init.ht), tau=.10)
sq_rq5<-rq(incdata$ht.inc~sqrt(incdata$init.ht), tau=.5)
sq_rq95<-rq(incdata$ht.inc~sqrt(incdata$init.ht), tau=.90)
#points(LL_both$Height_Total.x,LL_both$inc,cex=.8)
curve((sq_rq05$coefficients[2]*sqrt(x))+sq_rq05$coefficients[1], add = TRUE, col = "blue",lwd=2)
curve((sq_rq5$coefficients[2]*sqrt(x))+sq_rq5$coefficients[1], add = TRUE, col = "black",lwd=2)
curve((sq_rq95$coefficients[2]*sqrt(x))+sq_rq95$coefficients[1], add = TRUE, col = "red",lwd=2)

legend(17,.2, c(".90 Quantile",".50 Quantile",".10 Quantile"), 
            lty=c(1,1,1),
            lwd=c(2.5,2.5,2.5),col=c("red","black","blue")) 
