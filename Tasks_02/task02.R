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
dateID <- sapply(dayID, as.Date, format = "%Y-%m-%d", origin ="2019-04-18")
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

#Task 02b Starts here#
setwd()
beren3 <- read.csv("beren_new.csv", stringsAsFactors = F)

Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgMilk
#The unit for avg milk is ounces#
#COME BACK AND ANSWER THIS!!#
#[] single brackets are used to gather a subset of data from an object which in this case is feed.#
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
avgFeed
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=avgFeed, lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height = 4, width=4)
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=avgFeed, lty=2, col='red')
dev.off()
#Question 1: For hypothesis one of course the weight of the food will add on to his weight so there will be a positive correlation. The second one I'm not too sure on although I would assume its because his intake of milk is increasing regardless of how much he sleeps
#Question 2: I couldn't really really tell you... I can interpret it but maybe it doesnt make for a good test.
source("http://jonsmitchell.com/code/plotFxn02b.R")
pdf("r02b-cumulativeMilkByTime.pdf")
#Disclaimer: I worked with Miriam on this because she did not have the correct instructions paper. If i heard correctly you did say we were allowed to work together.

#Task 02c Starts here#
#Hypothesis: The slope of the amount of milk young beren consumes in oz over time will positively correlate with the slope of how much Beren sleeps over time.
setwd()
beren3 <- read.csv("beren_new.csv", stringsAsFactors = F)

Feeds <- which(beren3$event == "bottle")
avgMilk <- mean(beren3$value[Feeds])
avgMilk
#The unit for avg milk is ounces#
#COME BACK AND ANSWER THIS!!#
#[] single brackets are used to gather a subset of data from an object which in this case is feed.#
avgFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], mean)
avgFeed
varFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], var)
totalFeed <- tapply(beren3$value[Feeds], beren3$age[Feeds], sum)
numFeeds <- tapply(beren3$value[Feeds], beren3$age[Feeds], length)
cor(beren3$value[Feeds], beren3$age[Feeds])
cor.test(beren3$value[Feeds], beren3$age[Feeds])
berenCor <- cor.test(beren3$value[Feeds], beren3$age[Feeds])
summary(berenCor)
berenANOVA <- aov(beren3$value[Feeds] ~ beren3$caregiver[Feeds])
par(las=1, mar=c(5,5,1,1), mgp=c(2,0.5,0), tck=-0.01)
plot(as.numeric(names(totalFeed)), totalFeed, type="b", pch=16, xlab="age in days", ylab="ounces of milk")
abline(h=avgFeed, lty=2, col='red')
pdf("r02b-totalMilkByDay.pdf", height = 4, width=4)
dev.off()