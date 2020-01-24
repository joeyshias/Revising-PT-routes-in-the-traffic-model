setwd("C:/Users/ASUS/Dropbox/實習生宇軒/TN_network/creating shps/")
install.packages(c("rgdal", "sp"))
library(rgdal)
library(sp)


routes <- read.csv(file="creating shps2.csv", header=TRUE, sep=",")



setwd("C:/Users/ASUS/Dropbox/實習生宇軒/TN_network/creating shps/601-635/")
A <- 601
for(x in 601:635){
  temproutes <- routes[routes$路線短編號==A,]
  coordinates(temproutes)<-~X+Y # whatever the equivalent is in your 
  # data.frame
  class(temproutes) # [1] "SpatialPointsDataFrame"# attr(,"package")# [1] "sp"
  writeOGR(obj = temproutes, paste0("route", A,".shp"), layer = "try", driver = "ESRI Shapefile")
  A <- A+1
}


?gsub
years<-c("20 years", "1 years")
gsub("([0-9]+).*([a-z+]).*$", "\\1\\2", years)
?regex
