# in data ####
library(dplyr); library(readxl); library(lubridate)
library(doBy)
rm(list=ls()); dev.off(); cat("\f") # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("~/Git/OakHW/Environmental Data/")
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) {
  assign(temp[i], read.csv(temp[i], header = F))
}
rm(temp, i)
dfs<-Filter(function(x) is(x, "data.frame"), mget(ls()))
dfs<-Map(cbind, filenames = names(dfs), dfs)
newdataset<-dfs[[1]] # Make a shell df to rbind data to
obs.row<-which(newdataset[,2] == "#")
colnames(newdataset)<-NULL
newdataset<-newdataset[-c(obs.row-1),]
obs.row<-which(newdataset[,2] == "#")
colnames(newdataset)<-as.character(unlist(newdataset[1,]))
newdataset<-newdataset[0,]
newdataset<-newdataset[-c(1),]
names(newdataset)[1]<-"FileName"
names(newdataset)[2]<-"ObsNo"
names(newdataset)[3]<-"DateTime"
names(newdataset)[4]<-"Temp_C"
names(newdataset)[5]<-"RH_perc"
for (i in 1:length(dfs)) {
  temp<-dfs[[i]] # Make a shell df to rbind data to
  obs.row<-which(temp[,2] == "#")
  colnames(temp)<-NULL
  temp<-temp[-c(obs.row-1),]
  obs.row<-which(temp[,2] == "#")
  colnames(temp)<-as.character(unlist(temp[1,]))
  temp<-temp[-c(1),]
  names(temp)[1]<-"FileName"
  names(temp)[2]<-"ObsNo"
  names(temp)[3]<-"DateTime"
  names(temp)[4]<-"Temp_C"
  names(temp)[5]<-"RH_perc"
  newdataset<-bind_rows(newdataset, temp)
}
rm(list=setdiff(ls(), c("newdataset")))
newdataset$DateTime<-gsub("\t","",newdataset$DateTime)
newdataset$Temp_C<-gsub("\t","",newdataset$Temp_C)
newdataset$DateTime<-gsub("\t","",newdataset$DateTime)
newdataset$RH_perc<-gsub("\t","",newdataset$RH_perc)
newdataset$block<-substr(newdataset$FileName, 6, 7)
newdataset$trt<-substr(newdataset$FileName, 9, 12)
newdataset$Temp_C<-as.numeric(newdataset$Temp_C)
newdataset$RH_perc<-as.numeric(newdataset$RH_perc)
newdataset$block<-as.factor(newdataset$block)
newdataset$trt<-as.factor(newdataset$trt)
newdataset$Date<-as.factor(newdataset$Date)
newdataset$ObsNo<-as.numeric(newdataset$ObsNo)
newdataset$Date<-as.Date(newdataset$Date, format = "%m/%d/%y")
newdataset$SVP_kPa<-(610.78*(2.71828^(newdataset$Temp_C/(newdataset$Temp_C+238.3) * 17.2694)))/1000
newdataset$VPD_kPa<-newdataset$SVP_kPa * (1 - (newdataset$RH_perc/100))

# write.csv(newdataset, "High-res oak study temp data.csv")

df<-summaryBy(Temp_C + RH_perc + VPD_kPa ~ Date * trt, FUN = c(min, mean, max), na.rm = T, newdataset)
df<-df[order(df$Date),]

rm(newdataset)

df<-subset(df, Date > "2020-03-06")

# start ####

tiff(file = "V_AirTemp + RH + VPD (min,mean,max) ~ Date_cleaned.tiff", height = 6, width = 10, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(3,1), mar = c(1,1,1,1), omi = c(0.5,0.5,0.01,0.01))

# a) ####

plot(Temp_C.max ~ Date, df, ylim = c(0,46), pch = NA, lty = 2, xaxt="n", yaxt="n", xlab = "", ylab = "")
rect(xleft = as.Date("2020-05-22"), xright = as.Date("2020-05-25"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-29"), xright = as.Date("2020-07-03"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()
axis.Date(1, df$Date, at = seq(min(df$Date, na.rm = T), max(df$Date, na.rm = T),"weeks"), las = 2, format = "%m/%d", cex.axis = 1.25, labels = F)
axis(2, at = seq(0,50,5), cex.axis = 1.25, las = 2)


dat<-subset(df, trt == "amnh")
points(Temp_C.max ~ Date, dat, type = "l", col = "grey30", lty = 2)
points(Temp_C.mean ~ Date, dat, type = "l", col = "grey30", lty = 1)
points(Temp_C.min ~ Date, dat, type = "l", col = "grey30", lty = 2)

dat<-subset(df, trt == "amht")
points(Temp_C.max ~ Date, dat, type = "l", col = "firebrick", lty = 2)
points(Temp_C.mean ~ Date, dat, type = "l", col = "firebrick", lty = 1)
points(Temp_C.min ~ Date, dat, type = "l", col = "firebrick", lty = 2)

dat<-subset(df, trt == "waht")
points(Temp_C.max ~ Date, dat, type = "l", col = "orange", lty = 2)
points(Temp_C.mean ~ Date, dat, type = "l", col = "orange", lty = 1)
points(Temp_C.min ~ Date, dat, type = "l", col = "orange", lty = 2)

mtext(side = 2, expression(T[air]~(degree*C)), cex = 1.25, padj = -1.5)

legend("bottom", horiz = T, lty = c(1,2,NA,NA,NA,NA), pch = c(NA,NA, 15,15,15,15),bty = "n", pt.cex = 2,
       c("Mean","Min/Max", "Ambient","HW","Warm+HW","Heatwave Dates"),
       col = c("black","black","grey30","firebrick","orange","grey80"))

# b) ####

plot(RH_perc.max ~ Date, df, ylim = c(10,100), pch = NA, lty = 2, xaxt="n", yaxt="n", xlab = "", ylab = "")
rect(xleft = as.Date("2020-05-22"), xright = as.Date("2020-05-25"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-29"), xright = as.Date("2020-07-03"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()
axis.Date(1, df$Date, at = seq(min(df$Date, na.rm = T), max(df$Date, na.rm = T),"weeks"), las = 2, format = "%m/%d", cex.axis = 1.25, labels = F)
axis(2, at = seq(10,100,15), cex.axis = 1.25, las = 2)


dat<-subset(df, trt == "amnh")
points(RH_perc.max ~ Date, dat, type = "l", col = "grey30", lty = 2)
points(RH_perc.mean ~ Date, dat, type = "l", col = "grey30", lty = 1)
points(RH_perc.min ~ Date, dat, type = "l", col = "grey30", lty = 2)

dat<-subset(df, trt == "amht")
points(RH_perc.max ~ Date, dat, type = "l", col = "firebrick", lty = 2)
points(RH_perc.mean ~ Date, dat, type = "l", col = "firebrick", lty = 1)
points(RH_perc.min ~ Date, dat, type = "l", col = "firebrick", lty = 2)

dat<-subset(df, trt == "waht")
points(RH_perc.max ~ Date, dat, type = "l", col = "orange", lty = 2)
points(RH_perc.mean ~ Date, dat, type = "l", col = "orange", lty = 1)
points(RH_perc.min ~ Date, dat, type = "l", col = "orange", lty = 2)

mtext(side = 2, expression(RH~('%')), cex = 1.25, padj = -1.5)

# c) ####

plot(VPD_kPa.max ~ Date, df, ylim = c(0,7.25), pch = NA, lty = 2, xaxt="n", yaxt="n", xlab = "", ylab = "")
rect(xleft = as.Date("2020-05-22"), xright = as.Date("2020-05-25"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
rect(xleft = as.Date("2020-06-29"), xright = as.Date("2020-07-03"), ybottom = -999, ytop = 999, col = "grey80", bty = "n", border = F)
box()
axis.Date(1, df$Date, at = seq(min(df$Date, na.rm = T), max(df$Date, na.rm = T),"weeks"), las = 2, format = "%m/%d", cex.axis = 1.25)
axis(2, at = seq(0,50,1), cex.axis = 1.25, las = 2)


dat<-subset(df, trt == "amnh")
points(VPD_kPa.max ~ Date, dat, type = "l", col = "grey30", lty = 2)
points(VPD_kPa.mean ~ Date, dat, type = "l", col = "grey30", lty = 1)
points(VPD_kPa.min ~ Date, dat, type = "l", col = "grey30", lty = 2)

dat<-subset(df, trt == "amht")
points(VPD_kPa.max ~ Date, dat, type = "l", col = "firebrick", lty = 2)
points(VPD_kPa.mean ~ Date, dat, type = "l", col = "firebrick", lty = 1)
points(VPD_kPa.min ~ Date, dat, type = "l", col = "firebrick", lty = 2)

dat<-subset(df, trt == "waht")
points(VPD_kPa.max ~ Date, dat, type = "l", col = "orange", lty = 2)
points(VPD_kPa.mean ~ Date, dat, type = "l", col = "orange", lty = 1)
points(VPD_kPa.min ~ Date, dat, type = "l", col = "orange", lty = 2)

mtext(side = 2, expression(VPD~(kPa)), cex = 1.25, padj = -1.4)


dev.off()
