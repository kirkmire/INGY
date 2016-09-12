#Lattice graphs practice
#chapter 2

library(lattice)

trt.plt<-xyplot(ht.inc~sqrt(init.ht)|Treatment,data=incdata,type="p")
trt.plt

dim(trt.plt)

dimnames(trt.plt)

summary(trt.plt)

dotplot(ht.inc~sqrt(init.ht),data=incdata,groups=Treatment,
        layout=c(1,6),aspect=c(.7),
        auto.key=list(space="right"))


stag$dummy<-1

spec.freq.tbl<-xtabs(dummy~Species+Installation,data=stag)
spec.freq.tbl<-as.data.frame(spec.freq.tbl)

spec.freq.tbl

bc.tree<-barchart(Species~Freq|Installation,data=spec.freq.tbl)

#Panel function

update(bc.tree,
       panel=function(...){
         panel.grid(h=0,v=-1)
         panel.barchart(...)
       })

update(bc.tree,
       panel=function(...){
         panel.grid(h=0,v=-1)
         panel.barchart(...,border="transparent")
       })

#alternatively

update(bc.tree,border="transparent")

#density plots

densityplot(~init.ht|Treatment,data=incdata,
            kernel="rect",bw=.2,plot.points="rug",
            n=200)






