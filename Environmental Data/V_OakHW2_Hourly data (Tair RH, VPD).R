# in data ####
library(dplyr); library(readxl); library(lubridate)
library(doBy)

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("~/Git/OakHW/Environmental Data")
df<-read.csv("High-res oak study temp data.csv")
df<-summaryBy(Temp_C + VPD_kPa + RH_perc ~ Date + Hour + trt, FUN = c(mean), na.rm = T, df)


# Set-up ####

tiff(file = "V_OakHW2_Hourly data (Tair RH, VPD).tiff", height = 8, width = 10, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(3,5), mar = c(1,0,0,0), omi = c(0.6,0.9,0.2,0.01))

# Tair ####

dum<-subset(df, Date == "6/29/2020" & trt == "amnh")
plot(Temp_C.mean ~ Hour, dum, type = "l", col = "black", ylim = c(20,50), xaxs="i", xaxt="n",yaxt="n",xlab="",ylab="", lwd = 2)
dum<-subset(df, Date == "6/29/2020" & trt == "amht")
points(Temp_C.mean ~ Hour, dum, type = "l", col = "firebrick", lwd = 2)
dum<-subset(df, Date == "6/29/2020" & trt == "waht")
points(Temp_C.mean ~ Hour, dum, type = "l", col = "orange2", lwd = 2)
axis(1, seq(3,21,3), labels = F)
axis(2, seq(15,50,5), las = 2, cex.axis = 1.2)
mtext(side = 3, "6-29")
mtext(side = 2, expression(italic(T[air]~(degree*C))), padj = -2, cex = 1.2)

legend("bottomright", horiz = F, c("A","A+HW","W+HW"), lty = 1, col = c("black", "firebrick","orange2"),
       bty = "n", cex = 1.2, lwd = 2)

dum<-subset(df, Date == "6/30/2020" & trt == "amnh")
plot(Temp_C.mean ~ Hour, dum, type = "l", col = "black", ylim = c(20,50), xaxs="i", xaxt="n",yaxt="n",xlab="",ylab="", lwd = 2)
dum<-subset(df, Date == "6/30/2020" & trt == "amht")
points(Temp_C.mean ~ Hour, dum, type = "l", col = "firebrick", lwd = 2)
dum<-subset(df, Date == "6/30/2020" & trt == "waht")
points(Temp_C.mean ~ Hour, dum, type = "l", col = "orange2", lwd = 2)
axis(1, seq(3,21,3), labels = F)
mtext(side = 3, "6-30")

dum<-subset(df, Date == "7/1/2020" & trt == "amnh")
plot(Temp_C.mean ~ Hour, dum, type = "l", col = "black", ylim = c(20,50), xaxs="i", xaxt="n",yaxt="n",xlab="",ylab="", lwd = 2)
dum<-subset(df, Date == "7/1/2020" & trt == "amht")
points(Temp_C.mean ~ Hour, dum, type = "l", col = "firebrick", lwd = 2)
dum<-subset(df, Date == "7/1/2020" & trt == "waht")
points(Temp_C.mean ~ Hour, dum, type = "l", col = "orange2", lwd = 2)
axis(1, seq(3,21,3), labels = F)
mtext(side = 3, "7-1")

dum<-subset(df, Date == "7/2/2020" & trt == "amnh")
plot(Temp_C.mean ~ Hour, dum, type = "l", col = "black", ylim = c(20,50), xaxs="i", xaxt="n",yaxt="n",xlab="",ylab="", lwd = 2)
dum<-subset(df, Date == "7/2/2020" & trt == "amht")
points(Temp_C.mean ~ Hour, dum, type = "l", col = "firebrick", lwd = 2)
dum<-subset(df, Date == "7/2/2020" & trt == "waht")
points(Temp_C.mean ~ Hour, dum, type = "l", col = "orange2", lwd = 2)
axis(1, seq(3,21,3), labels = F)
mtext(side = 3, "7-2")

dum<-subset(df, Date == "7/3/2020" & trt == "amnh")
plot(Temp_C.mean ~ Hour, dum, type = "l", col = "black", ylim = c(20,50), xaxs="i", xaxt="n",yaxt="n",xlab="",ylab="", lwd = 2)
dum<-subset(df, Date == "7/3/2020" & trt == "amht")
points(Temp_C.mean ~ Hour, dum, type = "l", col = "firebrick", lwd = 2)
dum<-subset(df, Date == "7/3/2020" & trt == "waht")
points(Temp_C.mean ~ Hour, dum, type = "l", col = "orange2", lwd = 2)
axis(1, seq(3,21,3), labels = F)
mtext(side = 3, "7-3")

# RH ####

dum<-subset(df, Date == "6/29/2020" & trt == "amnh")
plot(RH_perc.mean ~ Hour, dum, type = "l", col = "black", ylim = c(30,100), xaxs="i", xaxt="n",yaxt="n",xlab="",ylab="", lwd = 2)
dum<-subset(df, Date == "6/29/2020" & trt == "amht")
points(RH_perc.mean ~ Hour, dum, type = "l", col = "firebrick", lwd = 2)
dum<-subset(df, Date == "6/29/2020" & trt == "waht")
points(RH_perc.mean ~ Hour, dum, type = "l", col = "orange2", lwd = 2)
axis(1, seq(3,21,3), labels = F)
axis(2, seq(0,100,10), las = 2, cex.axis = 1.2)
mtext(side = 2, expression(RH~('%')), padj = -2, cex = 1.2)

dum<-subset(df, Date == "6/30/2020" & trt == "amnh")
plot(RH_perc.mean ~ Hour, dum, type = "l", col = "black", ylim = c(30,100), xaxs="i", xaxt="n",yaxt="n",xlab="",ylab="", lwd = 2)
dum<-subset(df, Date == "6/30/2020" & trt == "amht")
points(RH_perc.mean ~ Hour, dum, type = "l", col = "firebrick", lwd = 2)
dum<-subset(df, Date == "6/30/2020" & trt == "waht")
points(RH_perc.mean ~ Hour, dum, type = "l", col = "orange2", lwd = 2)
axis(1, seq(3,21,3), labels = F)

dum<-subset(df, Date == "7/1/2020" & trt == "amnh")
plot(RH_perc.mean ~ Hour, dum, type = "l", col = "black", ylim = c(30,100), xaxs="i", xaxt="n",yaxt="n",xlab="",ylab="", lwd = 2)
dum<-subset(df, Date == "7/1/2020" & trt == "amht")
points(RH_perc.mean ~ Hour, dum, type = "l", col = "firebrick", lwd = 2)
dum<-subset(df, Date == "7/1/2020" & trt == "waht")
points(RH_perc.mean ~ Hour, dum, type = "l", col = "orange2", lwd = 2)
axis(1, seq(3,21,3), labels = F)

dum<-subset(df, Date == "7/2/2020" & trt == "amnh")
plot(RH_perc.mean ~ Hour, dum, type = "l", col = "black", ylim = c(30,100), xaxs="i", xaxt="n",yaxt="n",xlab="",ylab="", lwd = 2)
dum<-subset(df, Date == "7/2/2020" & trt == "amht")
points(RH_perc.mean ~ Hour, dum, type = "l", col = "firebrick", lwd = 2)
dum<-subset(df, Date == "7/2/2020" & trt == "waht")
points(RH_perc.mean ~ Hour, dum, type = "l", col = "orange2", lwd = 2)
axis(1, seq(3,21,3), labels = F)

dum<-subset(df, Date == "7/3/2020" & trt == "amnh")
plot(RH_perc.mean ~ Hour, dum, type = "l", col = "black", ylim = c(30,100), xaxs="i", xaxt="n",yaxt="n",xlab="",ylab="", lwd = 2)
dum<-subset(df, Date == "7/3/2020" & trt == "amht")
points(RH_perc.mean ~ Hour, dum, type = "l", col = "firebrick", lwd = 2)
dum<-subset(df, Date == "7/3/2020" & trt == "waht")
points(RH_perc.mean ~ Hour, dum, type = "l", col = "orange2", lwd = 2)
axis(1, seq(3,21,3), labels = F)

# VPD ####

dum<-subset(df, Date == "6/29/2020" & trt == "amnh")
plot(VPD_kPa.mean ~ Hour, dum, type = "l", col = "black", ylim = c(0,6), xaxs="i", xaxt="n",yaxt="n",xlab="",ylab="", lwd = 2)
dum<-subset(df, Date == "6/29/2020" & trt == "amht")
points(VPD_kPa.mean ~ Hour, dum, type = "l", col = "firebrick", lwd = 2)
dum<-subset(df, Date == "6/29/2020" & trt == "waht")
points(VPD_kPa.mean ~ Hour, dum, type = "l", col = "orange2", lwd = 2)
axis(1, seq(3,21,3), labels = c("03:00", "06:00","09:00","12:00","15:00","18:00","21:00"), las = 2)
axis(2, seq(0,7,1), las = 2, cex.axis = 1.2)
mtext(side = 2, expression(VPD~(kPa)), padj = -2, cex = 1.2)

dum<-subset(df, Date == "6/30/2020" & trt == "amnh")
plot(VPD_kPa.mean ~ Hour, dum, type = "l", col = "black", ylim = c(0,6), xaxs="i", xaxt="n",yaxt="n",xlab="",ylab="", lwd = 2)
dum<-subset(df, Date == "6/30/2020" & trt == "amht")
points(VPD_kPa.mean ~ Hour, dum, type = "l", col = "firebrick", lwd = 2)
dum<-subset(df, Date == "6/30/2020" & trt == "waht")
points(VPD_kPa.mean ~ Hour, dum, type = "l", col = "orange2", lwd = 2)
axis(1, seq(3,21,3), labels = c("03:00", "06:00","09:00","12:00","15:00","18:00","21:00"), las = 2)

dum<-subset(df, Date == "7/1/2020" & trt == "amnh")
plot(VPD_kPa.mean ~ Hour, dum, type = "l", col = "black", ylim = c(0,6), xaxs="i", xaxt="n",yaxt="n",xlab="",ylab="", lwd = 2)
dum<-subset(df, Date == "7/1/2020" & trt == "amht")
points(VPD_kPa.mean ~ Hour, dum, type = "l", col = "firebrick", lwd = 2)
dum<-subset(df, Date == "7/1/2020" & trt == "waht")
points(VPD_kPa.mean ~ Hour, dum, type = "l", col = "orange2", lwd = 2)
axis(1, seq(3,21,3), labels = c("03:00", "06:00","09:00","12:00","15:00","18:00","21:00"), las = 2)

dum<-subset(df, Date == "7/2/2020" & trt == "amnh")
plot(VPD_kPa.mean ~ Hour, dum, type = "l", col = "black", ylim = c(0,6), xaxs="i", xaxt="n",yaxt="n",xlab="",ylab="", lwd = 2)
dum<-subset(df, Date == "7/2/2020" & trt == "amht")
points(VPD_kPa.mean ~ Hour, dum, type = "l", col = "firebrick", lwd = 2)
dum<-subset(df, Date == "7/2/2020" & trt == "waht")
points(VPD_kPa.mean ~ Hour, dum, type = "l", col = "orange2", lwd = 2)
axis(1, seq(3,21,3), labels = c("03:00", "06:00","09:00","12:00","15:00","18:00","21:00"), las = 2)

dum<-subset(df, Date == "7/3/2020" & trt == "amnh")
plot(VPD_kPa.mean ~ Hour, dum, type = "l", col = "black", ylim = c(0,6), xaxs="i", xaxt="n",yaxt="n",xlab="",ylab="", lwd = 2)
dum<-subset(df, Date == "7/3/2020" & trt == "amht")
points(VPD_kPa.mean ~ Hour, dum, type = "l", col = "firebrick", lwd = 2)
dum<-subset(df, Date == "7/3/2020" & trt == "waht")
points(VPD_kPa.mean ~ Hour, dum, type = "l", col = "orange2", lwd = 2)
axis(1, seq(3,21,3), labels = c("03:00", "06:00","09:00","12:00","15:00","18:00","21:00"), las = 2)

# Final ####

mtext(side = 1, "Time (hh:mm)", outer = T, padj = 2.5, cex = 1.5)

dev.off()
