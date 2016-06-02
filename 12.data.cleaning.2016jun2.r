#Look for Species Changes#
#Not possible because species recorded seperate 
#from measurements in stagm and sover dataframes

#Look for height growths of less than negative 1ft#


stagm
meast

for (Installation in stagm$Installation){
  
    print(paste("The installation is", Installation))
}


if(stagm$Year_Measurement==meast$`1ST_YEAR`&
   stagm$Installation==meast$Installation){
    print("yes")
}
   


dif<--4

if(-dif>1){
  print("ALERT")
}





















