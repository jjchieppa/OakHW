# Bring in data ####

library(car); library(sjPlot); library(lme4)
library(emmeans); library(lmerTest); library(multcomp)
library(plotrix)

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("~/Git/OakHW")

df<-read.csv("Oak_masterfile.csv")
dm<-read.csv("Oak_biomass data.csv")
dm$spp<-NULL; dm$Species<-NULL
names(dm)[3]<-"PlantID"
names(dm)[11]<-"DM_LMA_g.m2"
df<-merge(dm, df, by = "PlantID"); rm(dm)
df<-subset(df, spp != "mich")
df$Tldiff<-df$Tleaf-df$Tair
df$Tcdiff<-df$Tcan-df$Tair
df$Timepoint<-as.factor(df$Timepoint)


# Start ####

tiff(file = "V+A_Tcan~Tair + Tcdiff ~ Species x Treatment.tiff", height = 7, width = 10, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(4,3), omi = c(0.4,0.5,0.1,0.5), mar = c(2,3,1.5,3))
ox<-seq(-500,55,1); oy<-ox; oo<-lm(oy ~ ox)

# hemi ####

plot(Tcan ~ Tair, pch = NA, df, xlim = c(30,54), ylim = c(30,55), axes=F, xlab="", ylab=""); ablineclip(oo, x1 = -500, x2=45, col = "grey50", lty = 2)
dat<-subset(df, spp == "hemi")
mtext(side = 3, c(paste(unique(dat$spp))), cex = 1.1)
legend("topleft", bty = "n", "a)", adj = 1, cex = 1.2)

text(42, 50, "Warming", cex = 0.9, srt = 30, col = "firebrick4")
text(42, 33, "Cooling", cex = 0.9, srt = 30, col = "dodgerblue4")
text(50, 54.5, "Warming", cex = 0.9, col = "firebrick4")
text(50, 30.5, "Cooling", cex = 0.9, col = "dodgerblue4")

axis(1, at = seq(30,45,5), las = 1, cex.axis = 1.15)
axis(2, at = seq(30,55,5), las = 2, cex.axis = 1.15)
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "orange2")
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "orange2")

par(new=T)

plot(Tair ~ 1, pch = NA, df, xlim = c(30,54), ylim = c(-8,15), axes=F, xlab="", ylab="")
axis(1, c(47,50,53), cex.axis = 1.15, labels = F)
axis(4, seq(-6,16,3), cex.axis = 1.15, las = 2)
ablineclip(h = 0, x1 = 45, x2 = 100, col = "grey50", lty = 1)
ablineclip(v = 45, col = "black")


dum<-subset(dat, Treatment.x == "Control")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 47, y1 = mn, y2 = mx)
rect(46, ql, 48, qu, col = "white")
ablineclip(h = xt, x1 = 46, x2 = 48, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Heatwave")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 50, y1 = mn, y2 = mx)
rect(49, ql, 51, qu, col = "firebrick")
ablineclip(h = xt, x1 = 49, x2 = 51, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Warm+Heat")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 53, y1 = mn, y2 = mx)
rect(52, ql, 54, qu, col = "orange2")
ablineclip(h = xt, x1 = 52, x2 = 54, col = "black", lwd = 2)

box()

# chap ####

plot(Tcan ~ Tair, pch = NA, df, xlim = c(30,54), ylim = c(30,55), axes=F, xlab="", ylab=""); ablineclip(oo, x1 = -500, x2=45, col = "grey50", lty = 2)
dat<-subset(df, spp == "chap")
mtext(side = 3, c(paste(unique(dat$spp))), cex = 1.1)
legend("topleft", bty = "n", "b)", adj = 1, cex = 1.2)

# text(48, 50, "Warming", cex = 0.9, srt = 43, col = "firebrick4")
# text(50, 48, "Cooling", cex = 0.9, srt = 43, col = "dodgerblue4")
# text(60, 54.5, "Warming", cex = 0.9, col = "firebrick")
# text(60, 30.5, "Cooling", cex = 0.9, col = "dodgerblue4")

axis(1, at = seq(30,45,5), las = 1, cex.axis = 1.15)
axis(2, at = seq(30,55,5), las = 2, cex.axis = 1.15)
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "orange2")
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "orange2")

par(new=T)

plot(Tair ~ 1, pch = NA, df, xlim = c(30,54), ylim = c(-8,15), axes=F, xlab="", ylab="")
axis(1, c(47,50,53), cex.axis = 1.15, labels = F)
axis(4, seq(-6,16,3), cex.axis = 1.15, las = 2)
ablineclip(h = 0, x1 = 45, x2 = 100, col = "grey50", lty = 1)
ablineclip(v = 45, col = "black")

dum<-subset(dat, Treatment.x == "Control")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 47, y1 = mn, y2 = mx)
rect(46, ql, 48, qu, col = "white")
ablineclip(h = xt, x1 = 46, x2 = 48, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Heatwave")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 50, y1 = mn, y2 = mx)
rect(49, ql, 51, qu, col = "firebrick")
ablineclip(h = xt, x1 = 49, x2 = 51, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Warm+Heat")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 53, y1 = mn, y2 = mx)
rect(52, ql, 54, qu, col = "orange2")
ablineclip(h = xt, x1 = 52, x2 = 54, col = "black", lwd = 2)

box()

# gemi ####

plot(Tcan ~ Tair, pch = NA, df, xlim = c(30,54), ylim = c(30,55), axes=F, xlab="", ylab=""); ablineclip(oo, x1 = -500, x2=45, col = "grey50", lty = 2)
dat<-subset(df, spp == "gemi")
mtext(side = 3, c(paste(unique(dat$spp))), cex = 1.1)
legend("topleft", bty = "n", "c)", adj = 1, cex = 1.2)

# text(48, 50, "Warming", cex = 0.9, srt = 43, col = "firebrick4")
# text(50, 48, "Cooling", cex = 0.9, srt = 43, col = "dodgerblue4")
# text(60, 54.5, "Warming", cex = 0.9, col = "firebrick")
# text(60, 30.5, "Cooling", cex = 0.9, col = "dodgerblue4")

axis(1, at = seq(30,45,5), las = 1, cex.axis = 1.15)
axis(2, at = seq(30,55,5), las = 2, cex.axis = 1.15)
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "orange2")
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "orange2")

par(new=T)

plot(Tair ~ 1, pch = NA, df, xlim = c(30,54), ylim = c(-8,15), axes=F, xlab="", ylab="")
axis(1, c(47,50,53), cex.axis = 1.15, labels = F)
axis(4, seq(-6,16,3), cex.axis = 1.15, las = 2)
ablineclip(h = 0, x1 = 45, x2 = 100, col = "grey50", lty = 1)
ablineclip(v = 45, col = "black")

dum<-subset(dat, Treatment.x == "Control")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 47, y1 = mn, y2 = mx)
rect(46, ql, 48, qu, col = "white")
ablineclip(h = xt, x1 = 46, x2 = 48, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Heatwave")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 50, y1 = mn, y2 = mx)
rect(49, ql, 51, qu, col = "firebrick")
ablineclip(h = xt, x1 = 49, x2 = 51, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Warm+Heat")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 53, y1 = mn, y2 = mx)
rect(52, ql, 54, qu, col = "orange2")
ablineclip(h = xt, x1 = 52, x2 = 54, col = "black", lwd = 2)

box()

# phel ####

plot(Tcan ~ Tair, pch = NA, df, xlim = c(30,54), ylim = c(30,55), axes=F, xlab="", ylab=""); ablineclip(oo, x1 = -500, x2=45, col = "grey50", lty = 2)
dat<-subset(df, spp == "phel")
mtext(side = 3, c(paste(unique(dat$spp))), cex = 1.1)
legend("topleft", bty = "n", "d)", adj = 1, cex = 1.2)

# text(48, 50, "Warming", cex = 0.9, srt = 43, col = "firebrick4")
# text(50, 48, "Cooling", cex = 0.9, srt = 43, col = "dodgerblue4")
# text(60, 54.5, "Warming", cex = 0.9, col = "firebrick")
# text(60, 30.5, "Cooling", cex = 0.9, col = "dodgerblue4")

axis(1, at = seq(30,45,5), las = 1, cex.axis = 1.15)
axis(2, at = seq(30,55,5), las = 2, cex.axis = 1.15)
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "orange2")
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "orange2")

par(new=T)

plot(Tair ~ 1, pch = NA, df, xlim = c(30,54), ylim = c(-8,15), axes=F, xlab="", ylab="")
axis(1, c(47,50,53), cex.axis = 1.15, labels = F)
axis(4, seq(-6,16,3), cex.axis = 1.15, las = 2)
ablineclip(h = 0, x1 = 45, x2 = 100, col = "grey50", lty = 1)
ablineclip(v = 45, col = "black")

dum<-subset(dat, Treatment.x == "Control")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 47, y1 = mn, y2 = mx)
rect(46, ql, 48, qu, col = "white")
ablineclip(h = xt, x1 = 46, x2 = 48, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Heatwave")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 50, y1 = mn, y2 = mx)
rect(49, ql, 51, qu, col = "firebrick")
ablineclip(h = xt, x1 = 49, x2 = 51, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Warm+Heat")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 53, y1 = mn, y2 = mx)
rect(52, ql, 54, qu, col = "orange2")
ablineclip(h = xt, x1 = 52, x2 = 54, col = "black", lwd = 2)

box()

# marg ####

plot(Tcan ~ Tair, pch = NA, df, xlim = c(30,54), ylim = c(30,55), axes=F, xlab="", ylab=""); ablineclip(oo, x1 = -500, x2=45, col = "grey50", lty = 2)
dat<-subset(df, spp == "marg")
mtext(side = 3, c(paste(unique(dat$spp))), cex = 1.1)
legend("topleft", bty = "n", "e)", adj = 1, cex = 1.2)

# text(48, 50, "Warming", cex = 0.9, srt = 43, col = "firebrick4")
# text(50, 48, "Cooling", cex = 0.9, srt = 43, col = "dodgerblue4")
# text(60, 54.5, "Warming", cex = 0.9, col = "firebrick")
# text(60, 30.5, "Cooling", cex = 0.9, col = "dodgerblue4")

axis(1, at = seq(30,45,5), las = 1, cex.axis = 1.15)
axis(2, at = seq(30,55,5), las = 2, cex.axis = 1.15)
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "orange2")
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "orange2")

par(new=T)

plot(Tair ~ 1, pch = NA, df, xlim = c(30,54), ylim = c(-8,15), axes=F, xlab="", ylab="")
axis(1, c(47,50,53), cex.axis = 1.15, labels = F)
axis(4, seq(-6,16,3), cex.axis = 1.15, las = 2)
ablineclip(h = 0, x1 = 45, x2 = 100, col = "grey50", lty = 1)
ablineclip(v = 45, col = "black")

dum<-subset(dat, Treatment.x == "Control")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 47, y1 = mn, y2 = mx)
rect(46, ql, 48, qu, col = "white")
ablineclip(h = xt, x1 = 46, x2 = 48, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Heatwave")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 50, y1 = mn, y2 = mx)
rect(49, ql, 51, qu, col = "firebrick")
ablineclip(h = xt, x1 = 49, x2 = 51, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Warm+Heat")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 53, y1 = mn, y2 = mx)
rect(52, ql, 54, qu, col = "orange2")
ablineclip(h = xt, x1 = 52, x2 = 54, col = "black", lwd = 2)

box()

# virg ####

plot(Tcan ~ Tair, pch = NA, df, xlim = c(30,54), ylim = c(30,55), axes=F, xlab="", ylab=""); ablineclip(oo, x1 = -500, x2=45, col = "grey50", lty = 2)
dat<-subset(df, spp == "virg")
mtext(side = 3, c(paste(unique(dat$spp))), cex = 1.1)
legend("topleft", bty = "n", "f)", adj = 1, cex = 1.2)

# text(48, 50, "Warming", cex = 0.9, srt = 43, col = "firebrick4")
# text(50, 48, "Cooling", cex = 0.9, srt = 43, col = "dodgerblue4")
# text(60, 54.5, "Warming", cex = 0.9, col = "firebrick")
# text(60, 30.5, "Cooling", cex = 0.9, col = "dodgerblue4")

axis(1, at = seq(30,45,5), las = 1, cex.axis = 1.15)
axis(2, at = seq(30,55,5), las = 2, cex.axis = 1.15)
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "orange2")
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "orange2")

par(new=T)

plot(Tair ~ 1, pch = NA, df, xlim = c(30,54), ylim = c(-8,15), axes=F, xlab="", ylab="")
axis(1, c(47,50,53), cex.axis = 1.15, labels = F)
axis(4, seq(-6,16,3), cex.axis = 1.15, las = 2)
ablineclip(h = 0, x1 = 45, x2 = 100, col = "grey50", lty = 1)
ablineclip(v = 45, col = "black")

dum<-subset(dat, Treatment.x == "Control")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 47, y1 = mn, y2 = mx)
rect(46, ql, 48, qu, col = "white")
ablineclip(h = xt, x1 = 46, x2 = 48, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Heatwave")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 50, y1 = mn, y2 = mx)
rect(49, ql, 51, qu, col = "firebrick")
ablineclip(h = xt, x1 = 49, x2 = 51, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Warm+Heat")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 53, y1 = mn, y2 = mx)
rect(52, ql, 54, qu, col = "orange2")
ablineclip(h = xt, x1 = 52, x2 = 54, col = "black", lwd = 2)

box()


# shum ####

plot(Tcan ~ Tair, pch = NA, df, xlim = c(30,54), ylim = c(30,55), axes=F, xlab="", ylab=""); ablineclip(oo, x1 = -500, x2=45, col = "grey50", lty = 2)
dat<-subset(df, spp == "shum")
mtext(side = 3, c(paste(unique(dat$spp))), cex = 1.1)
legend("topleft", bty = "n", "g)", adj = 1, cex = 1.2)

# text(48, 50, "Warming", cex = 0.9, srt = 43, col = "firebrick4")
# text(50, 48, "Cooling", cex = 0.9, srt = 43, col = "dodgerblue4")
# text(60, 54.5, "Warming", cex = 0.9, col = "firebrick")
# text(60, 30.5, "Cooling", cex = 0.9, col = "dodgerblue4")

axis(1, at = seq(30,45,5), las = 1, cex.axis = 1.15)
axis(2, at = seq(30,55,5), las = 2, cex.axis = 1.15)
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "orange2")
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "orange2")

par(new=T)

plot(Tair ~ 1, pch = NA, df, xlim = c(30,54), ylim = c(-8,15), axes=F, xlab="", ylab="")
axis(1, c(47,50,53), cex.axis = 1.15, labels = F)
axis(4, seq(-6,16,3), cex.axis = 1.15, las = 2)
ablineclip(h = 0, x1 = 45, x2 = 100, col = "grey50", lty = 1)
ablineclip(v = 45, col = "black")

dum<-subset(dat, Treatment.x == "Control")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 47, y1 = mn, y2 = mx)
rect(46, ql, 48, qu, col = "white")
ablineclip(h = xt, x1 = 46, x2 = 48, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Heatwave")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 50, y1 = mn, y2 = mx)
rect(49, ql, 51, qu, col = "firebrick")
ablineclip(h = xt, x1 = 49, x2 = 51, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Warm+Heat")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 53, y1 = mn, y2 = mx)
rect(52, ql, 54, qu, col = "orange2")
ablineclip(h = xt, x1 = 52, x2 = 54, col = "black", lwd = 2)

box()


# stel ####

plot(Tcan ~ Tair, pch = NA, df, xlim = c(30,54), ylim = c(30,55), axes=F, xlab="", ylab=""); ablineclip(oo, x1 = -500, x2=45, col = "grey50", lty = 2)
dat<-subset(df, spp == "stel")
mtext(side = 3, c(paste(unique(dat$spp))), cex = 1.1)
legend("topleft", bty = "n", "h)", adj = 1, cex = 1.2)

# text(48, 50, "Warming", cex = 0.9, srt = 43, col = "firebrick4")
# text(50, 48, "Cooling", cex = 0.9, srt = 43, col = "dodgerblue4")
# text(60, 54.5, "Warming", cex = 0.9, col = "firebrick")
# text(60, 30.5, "Cooling", cex = 0.9, col = "dodgerblue4")

axis(1, at = seq(30,45,5), las = 1, cex.axis = 1.15)
axis(2, at = seq(30,55,5), las = 2, cex.axis = 1.15)
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "orange2")
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "orange2")

par(new=T)

plot(Tair ~ 1, pch = NA, df, xlim = c(30,54), ylim = c(-8,15), axes=F, xlab="", ylab="")
axis(1, c(47,50,53), cex.axis = 1.15, labels = F)
axis(4, seq(-6,16,3), cex.axis = 1.15, las = 2)
ablineclip(h = 0, x1 = 45, x2 = 100, col = "grey50", lty = 1)
ablineclip(v = 45, col = "black")

dum<-subset(dat, Treatment.x == "Control")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 47, y1 = mn, y2 = mx)
rect(46, ql, 48, qu, col = "white")
ablineclip(h = xt, x1 = 46, x2 = 48, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Heatwave")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 50, y1 = mn, y2 = mx)
rect(49, ql, 51, qu, col = "firebrick")
ablineclip(h = xt, x1 = 49, x2 = 51, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Warm+Heat")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 53, y1 = mn, y2 = mx)
rect(52, ql, 54, qu, col = "orange2")
ablineclip(h = xt, x1 = 52, x2 = 54, col = "black", lwd = 2)

box()

# blank ####
plot(1~0,pch=NA,axes=F,xlab="",ylab=""); box()

# velu ####

plot(Tcan ~ Tair, pch = NA, df, xlim = c(30,54), ylim = c(30,55), axes=F, xlab="", ylab=""); ablineclip(oo, x1 = -500, x2=45, col = "grey50", lty = 2)
dat<-subset(df, spp == "velu")
mtext(side = 3, c(paste(unique(dat$spp))), cex = 1.1)
legend("topleft", bty = "n", "i)", adj = 1, cex = 1.2)

# text(48, 50, "Warming", cex = 0.9, srt = 43, col = "firebrick4")
# text(50, 48, "Cooling", cex = 0.9, srt = 43, col = "dodgerblue4")
# text(60, 54.5, "Warming", cex = 0.9, col = "firebrick")
# text(60, 30.5, "Cooling", cex = 0.9, col = "dodgerblue4")

axis(1, at = seq(30,45,5), las = 1, cex.axis = 1.15)
axis(2, at = seq(30,55,5), las = 2, cex.axis = 1.15)
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "1")
points(dum$Tcan ~ dum$Tair, pch = 16, col = "orange2")
dum<-subset(dat, Treatment.x == "Control" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "dodgerblue4")
dum<-subset(dat, Treatment.x == "Heatwave" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "firebrick")
dum<-subset(dat, Treatment.x == "Warm+Heat" & Timepoint == "2")
points(dum$Tcan ~ dum$Tair, pch = 17, col = "orange2")

par(new=T)

plot(Tair ~ 1, pch = NA, df, xlim = c(30,54), ylim = c(-8,15), axes=F, xlab="", ylab="")
axis(1, c(47,50,53), cex.axis = 1.15, labels = c("Ambient","A+HW","W+HW"), las = 2)
axis(4, seq(-6,16,3), cex.axis = 1.15, las = 2)
ablineclip(h = 0, x1 = 45, x2 = 100, col = "grey50", lty = 1)
ablineclip(v = 45, col = "black")

dum<-subset(dat, Treatment.x == "Control")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 47, y1 = mn, y2 = mx)
rect(46, ql, 48, qu, col = "white")
ablineclip(h = xt, x1 = 46, x2 = 48, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Heatwave")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 50, y1 = mn, y2 = mx)
rect(49, ql, 51, qu, col = "firebrick")
ablineclip(h = xt, x1 = 49, x2 = 51, col = "black", lwd = 2)

dum<-subset(dat, Treatment.x == "Warm+Heat")
ql<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[2]); qu<-as.numeric(quantile(dum$Tcdiff, na.rm = T)[4])
xt<-median(dum$Tcdiff, na.rm = T); mn<-min(dum$Tcdiff, na.rm = T); mx<-max(dum$Tcdiff, na.rm = T)
ablineclip(v = 53, y1 = mn, y2 = mx)
rect(52, ql, 54, qu, col = "orange2")
ablineclip(h = xt, x1 = 52, x2 = 54, col = "black", lwd = 2)

box()


# blank ####
plot(1~0,pch=NA,axes=F,xlab="",ylab=""); box()

# blank ####
plot(1~0,pch=NA,axes=F,xlab="",ylab=""); box()
legend("center", c("Ambient","A+HW","W+HW"), cex = 1.5, pt.cex = 1.5, bty = "n",
       pch = 16, col = c("dodgerblue4","firebrick","orange2"))
# off ####
mtext(side = 1, expression(italic(T)[air]~(degree*C)),
      outer = T, cex = 1.5, padj = 0.5)
mtext(side = 2, expression(italic(T)[canopy]~(degree*C)),
      outer = T, cex = 1.5, padj = -0.5)
mtext(side = 4, expression(delta[canopy]~(degree*C)),
      outer = T, cex = 1.5, padj = 1)

dev.off()