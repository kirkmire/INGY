

OS.function<-function(x){
  over<-soverhist[soverhist$Installation==x,]
  min_yr<-over[over == min(over$Year_Measurement)]
  max_yr<-over[over == max(over$Year_Measurement)]
  first<-min_yr[1]
  last<-max_yr[1]
  result<-matrix(c(x,first,last,
            x,length(min_yr),length(max_yr)),ncol=3,byrow=T)
  dimnames(result) <-list(rep("", dim(result)[1]), rep("", dim(result)[2]))
  return(result)
}

for(i in unique(soverhist$Installation)){
 print(OS.function(i))
}



