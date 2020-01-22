setwd("C:\\Users\\david\\Desktop\\Evolution\\Tasks\\Tasks_02")
Data <- read.csv("http://jonsmitchell.com/data/beren.csv", stringsAsFactors=F)
write.csv(Data, "rawdata.csv", quote=F)
Data
length(Data)
nrow(Data)
ncol(Data)
colnames(Data)
head(Data)
Data[1,]
Data[2,]
Data[1:3,]
Data[1:3,4]
Data[1:5, 1:3]
#in Data[a,b] a is what rows to display and b is what columns to display#
Feeds <- which(Data[,9] == "bottle")
#this command asked which data in column 9 equaled "bottle"... wait, what?!#
berenMilk <- Data[Feeds,]
head(berenMilk)
#there are 6 rows noting everytime the event bottle occurs#
Feeds <- which(Data[,"event"] == "bottle")
Feeds <- which(Data$event == "bottle")
#each comand is looking in the "event" column or column 9 for everytime the word "bottle appears. They are just different commands asking R to look in that column#
dayID <- apply(Data, 1, function(x) paste(x[1:3], collapse="-"))
dateID <- sapply(dayID, as.Date, format = "%Y-%n-%d", origin ="2019-04-18")
Data$age <- dateID - dateID[which(Data$event == "birth")]
head(Data)
#but why did it only change in R? can we change it in excel?#
beren <- Data
beren2 <- beren
beren3 <- beren2[order(beren2$age),]
head(beren)
head(beren2)
head(beren3)
#why didn't "beren" show anything?#
write.csv(beren3, "beren_new.csv", quote=F, row.names=FALSE)
