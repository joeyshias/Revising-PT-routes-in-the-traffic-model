install.packages(c("foreign","rgdal"))
install.packages("dplyr")
install.packages("expss")
library(foreign)
library(rgdal)
library(dplyr)
library(expss)

{#前置設定
setwd(dir = "C:/Users/ASUS/Dropbox/實習生宇軒/TN_network/aggregating dbfs/")
stopname <- read.csv(file="TN_Bus Stops index.csv", header=TRUE, sep=",")
stopname$STOPNAME <- as.character(stopname$STOPNAME) #調整欄位屬性
####stopname[stopname$STOPNAME=="#N/A",3] <- "no name"
Busroutes <- data.frame("ROUTE"=0,"ROUTE_I"=0,"路線短編號"=0,"GoRetrn"=0,"N.SEQ"=0,"NODE"=0,"STOPID"=0,"STOPNAME"=0,"STOPIDNAME"=0,"validstop"=0,"SEQ"=0) #創建DUMMY車站的字典
Notes <- data.frame("路線短編號" = 0,"ROUTE"=0,"SEQ"=0,"NEW_ID"=0,"備註"=0) #創建備註紀錄簿
recordncol <- data.frame("路線短編號" = 0,"ncol"=0) #創建紀錄簿
setwd(dir = "C:/Users/ASUS/Dropbox/實習生宇軒/TN_network/dbf output/dbf pool modified/")
 }

X <- 1 #處理中的編號
for(X in 1:635){
dbfile <- read.dbf(paste0("route", X,".dbf")) #匯入DBF
colnames(dbfile)[1] <- "路線短編號"
colnames(dbfile)[4] <- "GoRetrn"
dbfile <- dbfile[(dbfile$NEW_ID!=99999)|is.na(dbfile$NEW_ID),]#先剔除99999


temp5 <- data.frame("路線短編號" = dbfile[1,1],"ncol"= ncol(dbfile))
recordncol <- rbind(recordncol, temp5)

dbfile[,1] <- dbfile[1,1]#把路線編號確實填好
dbfile[,2] <- dbfile[1,2]
dbfile[,3] <- dbfile[1,3]
dbfile[,4] <- dbfile[1,4]

if(ncol(dbfile)==13){
  temp4 <- dbfile[,c(1,2,11,12,13)]
  colnames(temp4)[5] <- "備註"
  Notes <- rbind(Notes,temp4)
dbfile[13] <- NULL
    } #cheking是否有備註欄




dbfile$validstop <- 1 #辨識站牌是否為dummy
dbfile[dbfile$INDEX==0,13] <- 0 #dummy 指示為0   #INDEX是判斷關鍵
dbfile <- dbfile[(!(dbfile$NEW_ID == 0 & dbfile$validstop ==0))|is.na(dbfile$NEW_ID),]#先剔除Duumy誤植為0的列


dbfile <- dbfile[order(dbfile$SEQ),] #站牌排序
dbfile$N.SEQ <- order(dbfile$SEQ) #重新編號

dbfile$NODE <- dbfile$NEW_ID #彙整NODE ID到新一欄 (這些是要調整過的站牌)
dbfile[(is.na(dbfile$NODE)|dbfile$NODE==0),15] <- dbfile[(is.na(dbfile$NODE)|dbfile$NODE==0),8] #彙整NODE ID到新一欄 (這些是毋須調整的站牌)


dbfile <- merge(dbfile,stopname, by.x = "NODE", by.y = "NODEID", all.x = TRUE, all.y = FALSE) #匯入STOPID & STOPNAME資訊
dbfile <- dbfile[,c(2:15,1,16:20)] #欄位排序
dbfile <- dbfile[order(dbfile$N.SEQ),] #列次排序




names(Busroutes)
names(dbfile)
temp3 <- dbfile[ ,c(2,3,1,4,14:18,13,11)]
names(Busroutes)
names(temp3)
Busroutes <- rbind(Busroutes,temp3)

temp3 <- NULL

X <- X+1
}


###{setwd(dir = "C:/Users/ASUS/Dropbox/實習生宇軒/TN_network/")
###write.csv(Busroutes,file = "aggregating Dbfs/BUSROUTES0211.csv")
###write.csv(Dummyindex,file = "aggregating Dbfs/Dummyindex0211.csv")
###write.csv(Newstopindex,file = "aggregating Dbfs/Newstopindex0211.csv")
###write.csv(Notes,file = "aggregating Dbfs/Notes0211.csv")


###Busroutes <- read.csv(file="aggregating Dbfs/BUSROUTES0118.csv", header=TRUE, sep=",")
###Dummyindex <- read.csv(file="aggregating Dbfs/Dummyindex0118.csv", header=TRUE, sep=",")
###Newstopindex <- read.csv(file="aggregating Dbfs/Dummyindex0118.csv", header=TRUE, sep=",")
{

  
#建立Dummy Index清單並編號
{
Dummyindex <- subset(x = Busroutes, validstop == 0 & is.na(Busroutes$STOPID),select = 6) #抓出dummy工作中，未設站牌的，待會統一設站
nrow(Dummyindex)
Dummyindex <- unique(Dummyindex)
nrow(Dummyindex)
A <- nrow(Dummyindex)
Dummyindex$stopID <- sprintf("DUM%04d",seq(1:A))
}

#建立NewStop Index清單並編號
{
Newstopindex <- subset(x = Busroutes, validstop == 1 & is.na(Busroutes$STOPID),6) #抓出SWAP工作中，未設站牌的，待會統一設站
nrow(Newstopindex)
Newstopindex <- unique(Newstopindex)
nrow(Newstopindex)
B <- nrow(Newstopindex)
Newstopindex$stopID <- sprintf("NEW%04d",seq(1:B))
}

{  
Busroutes$new <- vlookup(Busroutes$NODE, Dummyindex,2,1)
Busroutes$new2 <- vlookup(Busroutes$NODE, Newstopindex,2,1)
Busroutes$STOPNAME <- as.character(Busroutes$STOPNAME) #調整欄位屬性
Busroutes$STOPID <- as.character(Busroutes$STOPID) #調整欄位屬性
Busroutes[Busroutes$validstop==0 & is.na(Busroutes$STOPID),8] <- "Dummy"
Busroutes[Busroutes$validstop==0 & is.na(Busroutes$STOPID),9] <- "Dummy"
Busroutes[Busroutes$validstop==0 & is.na(Busroutes$STOPID),7] <- Busroutes[Busroutes$validstop==0 & is.na(Busroutes$STOPID),12]
Busroutes[Busroutes$validstop==1 & is.na(Busroutes$STOPID),8] <- "NewCreatedStop"
Busroutes[Busroutes$validstop==1 & is.na(Busroutes$STOPID),9] <- "NewCreatedStop"
Busroutes[Busroutes$validstop==1 & is.na(Busroutes$STOPID),7] <- Busroutes[Busroutes$validstop==1 & is.na(Busroutes$STOPID),13]
Busroutes[13] <- NULL
Busroutes[12] <- NULL
?sprintf
Busroutes$test <- Busroutes$路線短編號*100000+ Busroutes$NODE
nrow(Busroutes)
Busroutes$test2 <- c(0,diff(Busroutes$test))
}


setwd(dir = "C:/Users/ASUS/Dropbox/實習生宇軒/TN_network/aggregating dbfs/0212/")
write.csv(Busroutes, file = "TN_ROUTES_refine.csv")
write.csv(Dummyindex,file = "TN_Dummyindex.csv")
write.csv(Newstopindex,file = "TN_Newstopindex.csv")
          
