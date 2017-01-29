
tree.data<-read.csv("G95.csv")

colnames(tree.data)[5]<-"diameter"

#need to identify unique trees based on single column
#tree number evidently not unique to a given tree?

tree.data$tree.number.unique<-paste(tree.data$tree.number,tree.data$REPLCT,tree.data$TRT,sep=",")


#Function for linearily estimating day of .95 of max diam. attainment#

ninetyfive.func<-function(tree){
  #tree<-"1.1,1,1"
  #creates df of all records for a given tree #
  treeinfo<-tree.data[tree.data$tree.number.unique==tree,]
  #creates df of max diam and corresp. day
  treeinfo.max<-treeinfo[treeinfo$diameter==max(treeinfo$diameter),]
  treeinfo.max<-treeinfo.max[treeinfo.max$day==min(treeinfo.max$day),]
  treeinfo.second.greatest<-treeinfo[treeinfo$diameter<treeinfo.max$diameter&treeinfo$day<treeinfo.max$day,]
  treeinfo.second.greatest<-treeinfo[treeinfo.second.greatest$diameter==max(treeinfo.second.greatest$diameter),]
  #creates df of second greatest diameterand corresp. day
  ninety.five<-(.9999)*treeinfo.max$diameter
  
  #binds the two df together 
  reg.data<-rbind(treeinfo.max,treeinfo.second.greatest)
  
  diam.lm<-lm(reg.data$diameter~reg.data$day)
  
  day<-(ninety.five-diam.lm$coefficients[1])/diam.lm$coefficients[2]
  
  day
  
 }

#Ceates a new column for day of 95% diam attainment
tree.data$ninety.five.day<-0

#For loop that applies function to every row in data table 
#assigns numeric value to new column

for(i in 1:nrow(tree.data)){
  tree.data$ninety.five.day[i]<-ninetyfive.func(
    tree.data$tree.number.unique[i])
}
