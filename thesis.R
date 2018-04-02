weathernew <- read.csv("D:/thesis/waste data set/weathernew.csv")
View(weathernew)
setwd("D:/thesis/waste data set")
weathernew$Date= NULL
weathernew$WindDir = NULL
weathernew$RainTomorrow = NULL
View(weathernew)
View(weathernew)
mydata$MaxTemp[is.na(mydata$MaxTemp)]<-mean(mydata$MaxTemp[!is.na(mydata$MaxTemp)])
mydata$Rainfall[is.na(mydata$Rainfall)]<-mean(mydata$Rainfall[!is.na(mydata$Rainfall)])
mydata$WindSpeed[is.na(mydata$WindSpeed)]<-mean(mydata$WindSpeed[!is.na(mydata$WindSpeed)])
mydata$Humidity[is.na(mydata$Humidity)]<-mean(mydata$Humidity[!is.na(mydata$Humidity)])
mydata$Pressure[is.na(mydata$Pressure)]<-mean(mydata$Pressure[!is.na(mydata$Pressure)])
mydata$Cloud[is.na(mydata$Cloud)]<-mean(mydata$Cloud[!is.na(mydata$Cloud)])
plot(Location ~ MaxTemp, data = mydata)
plot(MinTemp ~ MaxTemp, data = mydata)
plot(Cloud ~ MaxTemp, data = mydata)
-------------------------
  plot(Pressure ~ MaxTemp, data = mydata)
cor(mydata$MinTemp, mydata$MaxTemp)

abline(model1, col="red",lty = 2,lwd = 2)
new = mydata
View(new)
View(new)
table(new$Location)
normalize <- function(x) {}
normalize <- function(x) {
  +     return((x-min(x)) / (max(x)-min(x)))
  + }
new1 = as.data.frame(lapply(new[,c(2,3,4,5,6,7,8)],normalize))
summary(new1)
ans = kmeans(new1, 5)
table(mydata$Location,ans$cluster)
plot(new[c("MinTemp","MaxTemp")],col = ans$cluster)
plot(new[c("MinTemp","MaxTemp")],col = new$Location)
