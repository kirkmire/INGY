##Exercise 1

tree_data<-read.csv(url("http://forestlidar.org/added_files/tree_data.csv"))


##Exercise 2

myfunction <- function(x){
  dbh<-c(min(x[,2]),max(x[,2]),mean(x[,2]))
  ht<-c(min(x[,3]),max(x[,3]),mean(x[,3]))
  age<-c(min(x[,4]),max(x[,4]),mean(x[,4]))
  x_coord<-c(min(x[,5]),max(x[,5]),mean(x[,5]))
  y_coord<-c(min(x[,6]),max(x[,6]),mean(x[,6]))
  grt_than<-length(which(X[,2]>25))
  numb_grt_25<-c(grt_than,0,0)
  vec<-c(dbh,ht,age,x_coord,y_coord,numb_grt_25)
  matrix1<-matrix(vec,nrow=3,ncol=6,dimnames=list(c("Min","Max","Avg"),
          c("DBH","HT","Age","X.coord","Y.coord","Number>25in.DBH")))
  matrix2<-round(matrix1,2)
    return(matrix2)
}

myfunction(tree_data)


##Exercise 3

avg_ht_function <- function(lower_bound,upper_bound){
  sub_tree<-subset(tree_data,tree_data[2]>=lower_bound&tree_data[2]<=upper_bound)
  avg_ht<-mean(sub_tree[,3])
  return(avg_ht)
}
  
avg_ht_function(1,5)  
  
avg_ht_function(5,10)    

avg_ht_function(20,30)         

##Exercise 4


avg_ht_function <- function(lower_bound,upper_bound){
  sub_tree<-subset(tree_data,tree_data[2]>=lower_bound&tree_data[2]<=upper_bound)
  avg_ht<-mean(sub_tree[,3])
  return(avg_ht)
}