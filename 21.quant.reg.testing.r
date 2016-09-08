# need to run step 1 to get data
source("19.LL.inc.vs.init.vs.vegcov.2016jun16.r")

#Testing the estimated quantile regression relationships
#Do they conform to the location shift hypothesis that
#assumes that all of the conditional quantile functions
#have the same slope parameters? pg.307 Koenker

sq_rq05<-rq(incdata$ht.inc~sqrt(incdata$init.ht), tau=.10)
sq_rq5<-rq(incdata$ht.inc~sqrt(incdata$init.ht), tau=.5)
sq_rq95<-rq(incdata$ht.inc~sqrt(incdata$init.ht), tau=.90)

anova(sq_rq05,sq_rq5,sq_rq95)

#Slopes are not the same at the three quantiles
