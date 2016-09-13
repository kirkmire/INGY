



int.function<-function(inst,yr){
   <-








mst.function<-function(inst,yr){
  inst<-merged_stagm_stag[merged_stagm_stag$Installation==,]
  n<-length(meast$Year_Measurement)
  fin.yr<-sort(unique(inst$Year_Measurement),partial=n)[n]
  pen.yr<-sort(unique(inst$Year_Measurement),partial=n-1)[n-1]
  antepen.yr<-sort(unique(inst$Year_Measurement),partial=n-2)[n-2]
  preantepen.yr<-sort(unique(inst$Year_Measurement),partial=n-3)[n-3]
  prepreantepen<-sort(unique(inst$Year_Measurement),partial=n-4)[n-4]
  
  
  fin.yr<-sort(unique(inst$Year_Measurement),partial=n)[n]
  pen.yr<-sort(unique(inst$Year_Measurement),partial=n-1)[n-1]
  antepen.yr<-sort(unique(inst$Year_Measurement),partial=n-2)[n-2]
  preantepen.yr<-sort(unique(inst$Year_Measurement),partial=n-3)[n-3]
  prepreantepen<-sort(unique(inst$Year_Measurement),partial=n-4)[n-4]
  
  
  
  
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

