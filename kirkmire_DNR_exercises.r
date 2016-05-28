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

avg_ht_function_clean <- function(lower_bound,upper_bound){
  sub_tree<-subset(tree_data,tree_data[2]>=lower_bound&tree_data[2]<=upper_bound)
  avg_ht<-mean(sub_tree[,3])
  return(avg_ht)
}

avg_ht_function_clean("a",10)  

avg_ht_function_clean(9999,99999)  

##Exercise 5

# load the required libraries 
library(sp) 
library(rgdal)


library(maptools)


http://forestlidar.org/added_files/wa_state.zip


library(maptools)
tmpdir <- tempdir()
url <- 'http://forestlidar.org/added_files/wa_state.zip'
file <- basename(url)
download.file(url, file)
unzip(file, exdir = tmpdir )


shapeFile <- paste(tmpdir,"/wa_state",sep="")
area<- readShapeSpatial(shapeFile)

library(ggplot2)
library(ggmap)
area.points <- fortify(area)

library(RColorBrewer)
colors <- brewer.pal(9, "BuGn")


mapImage <- get_map(location = c(lon = -118, lat = 37.5),
                    color = "color",
                    source = "osm",
                    # maptype = "terrain",
                    zoom = 6)

ggmap(mapImage) +
  geom_polygon(aes(x = long,
                   y = lat,
                   group = group),
               data = area.points,
               color = colors[9],
               fill = colors[6],
               alpha = 0.5) +
  labs(x = "Longitude",
       y = "Latitude")






























