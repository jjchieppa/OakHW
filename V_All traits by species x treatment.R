# indata ####
library(lme4); library(lmerTest); library(effects)
library(MuMIn); library(emmeans); library(multcomp)
library(doBy); library(plotrix)

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("~/Git/OakHW")
df<-read.csv("Oak_masterfile.csv")
dm<-read.csv("Oak_biomass data.csv")
dm$spp<-NULL; dm$Species<-NULL
names(dm)[3]<-"PlantID"
names(dm)[11]<-"DM_LMA_g.m2"
df<-merge(dm, df, by = "PlantID")
rm(dm)
df<-subset(df, spp != "mich")
df<-subset(df, All.Leaves.Dead != "Y")
df$Tldiff<-df$Tleaf-df$Tair
df$Tcdiff<-df$Tcan-df$Tair
df$Timepoint<-as.factor(df$Timepoint)

spp<-summaryBy(LMA_g.m2 + LDMC_g.g + SD_no.mm2 + SS_um2 + Chlor + LS_cm2 ~
                 spp + Treatment.x + Timepoint, FUN = c(mean,std.error), na.rm = T, df)

# Start ####

tiff(file = "V_All traits by species x treatment.tiff", height = 8, width = 8, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(6,9), mar = c(1,0,0,0), omi = c(1.1,0.8,0.2,0.1))

lf<-0.4
rt<-3.6

# ~~~1) LMA ####
ll<-50
ul<-170

# hemi ####

dum<-subset(spp, spp == "hemi")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

mtext(side = 3, paste(unique(dat$spp)))
axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "a)", adj = 1)

# **leg ####

axis(2, at = seq(ll,ul,25), las = 2)

mtext(side = 2, expression(LMA), cex = 1, padj = -3.5)

# phel ####

dum<-subset(spp, spp == "phel")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

mtext(side = 3, paste(unique(dat$spp)))
axis(1, c(1,2,3), labels = F)

# text(1, 140, "ab", cex = 1.2)
# text(2, 140, "a", cex = 1.2)
# text(3, 140, "b", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "b)", adj = 1)

# shum ####

dum<-subset(spp, spp == "shum")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

mtext(side = 3, paste(unique(dat$spp)))
axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "c)", adj = 1)

# velu ####

dum<-subset(spp, spp == "velu")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

mtext(side = 3, paste(unique(dat$spp)))
axis(1, c(1,2,3), labels = F)

# text(1, 140, "b", cex = 1.2)
# text(2, 140, "b", cex = 1.2)
# text(3, 140, "a", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "d)", adj = 1)
# chap ####

dum<-subset(spp, spp == "chap")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

mtext(side = 3, paste(unique(dat$spp)))
axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "e)", adj = 1)

# marg ####

dum<-subset(spp, spp == "marg")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

mtext(side = 3, paste(unique(dat$spp)))
axis(1, c(1,2,3), labels = F)

# text(1, 140, "a", cex = 1.2)
# text(2, 140, "a", cex = 1.2)
# text(3, 140, "a", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "f)", adj = 1)
# stel ####

dum<-subset(spp, spp == "stel")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

mtext(side = 3, paste(unique(dat$spp)))
axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "g)", adj = 1)

# gemi ####

dum<-subset(spp, spp == "gemi")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

mtext(side = 3, paste(unique(dat$spp)))
axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "h)", adj = 1)

# virg ####

dum<-subset(spp, spp == "virg")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LMA_g.m2.mean, (dat$LMA_g.m2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

mtext(side = 3, paste(unique(dat$spp)))
axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "i)", adj = 1)

# ~~~2) LDMC ####
ll<-0.3
ul<-0.55

# hemi ####

dum<-subset(spp, spp == "hemi")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "j)", adj = 1)

# **leg ####

axis(2, at = seq(ll,ul,0.05), las = 2)

mtext(side = 2, expression(LDMC), cex = 1, padj = -3.5)

# phel ####

dum<-subset(spp, spp == "phel")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "k)", adj = 1)

# shum ####

dum<-subset(spp, spp == "shum")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "l)", adj = 1)

# velu ####

dum<-subset(spp, spp == "velu")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)

# text(1, 0.25, "b", cex = 1.2)
# text(2, 0.25, "b", cex = 1.2)
# text(3, 0.25, "a", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "m)", adj = 1)

# chap ####

dum<-subset(spp, spp == "chap")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "n)", adj = 1)

# marg ####

dum<-subset(spp, spp == "marg")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "o)", adj = 1)

# stel ####

dum<-subset(spp, spp == "stel")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("bottomleft", bty = "n", cex = 1.2, "p)", adj = 1)

# gemi ####

dum<-subset(spp, spp == "gemi")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "q)", adj = 1)

# virg ####

dum<-subset(spp, spp == "virg")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LDMC_g.g.mean, (dat$LDMC_g.g.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)

legend("topleft", bty = "n", cex = 1.2, "r)", adj = 1)
# ~~~3) Chlor ####
ll<-4
ul<-16

# hemi ####

dum<-subset(spp, spp == "hemi")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "s)", adj = 1)

# **leg ####

axis(2, at = seq(ll,ul,4), las = 2)

mtext(side = 2, expression(Chlor), cex = 1, padj = -3.5)

# phel ####

dum<-subset(spp, spp == "phel")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)

# text(1, 12, "a", cex = 1.2)
# text(2, 12, "a", cex = 1.2)
# text(3, 12, "b", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "t)", adj = 1)
# shum ####

dum<-subset(spp, spp == "shum")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "u)", adj = 1)

# velu ####

dum<-subset(spp, spp == "velu")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)

# text(1, 14, "b", cex = 1.2)
# text(2, 14, "ab", cex = 1.2)
# text(3, 14, "a", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "v)", adj = 1)

# chap ####

dum<-subset(spp, spp == "chap")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "w)", adj = 1)

# marg ####

dum<-subset(spp, spp == "marg")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)

# text(1, 14, "b", cex = 1.2)
# text(2, 14, "a", cex = 1.2)
# text(3, 14, "ab", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "x)", adj = 1)
# stel ####

dum<-subset(spp, spp == "stel")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "y)", adj = 1)

# gemi ####

dum<-subset(spp, spp == "gemi")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "z)", adj = 1)

# virg ####

dum<-subset(spp, spp == "virg")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$Chlor.mean, (dat$Chlor.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)

# text(1, 16, "b", cex = 1.2)
# text(2, 16, "ab", cex = 1.2)
# text(3, 16, "a", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "A)", adj = 1)

# ~~~4) SD ####
ll<-50
ul<-170

# hemi ####

dum<-subset(spp, spp == "hemi")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)

# text(1, 160, "a", cex = 1.2)
# text(2, 160, "a", cex = 1.2)
# text(3, 160, "b", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "B)", adj = 1)

# **leg ####

axis(2, at = seq(ll,ul,25), las = 2)

mtext(side = 2, expression(SD), cex = 1, padj = -3.5)

# phel ####

dum<-subset(spp, spp == "phel")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "C)", adj = 1)

# shum ####

dum<-subset(spp, spp == "shum")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "D)", adj = 1)

# velu ####

dum<-subset(spp, spp == "velu")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)

# text(1, 160, "ab", cex = 1.2)
# text(2, 160, "b", cex = 1.2)
# text(3, 160, "a", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "E)", adj = 1)
# chap ####

dum<-subset(spp, spp == "chap")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "F)", adj = 1)

# marg ####

dum<-subset(spp, spp == "marg")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "G)", adj = 1)

# stel ####

dum<-subset(spp, spp == "stel")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "H)", adj = 1)

# gemi ####

dum<-subset(spp, spp == "gemi")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)

# text(1, 160, "a", cex = 1.2)
# text(2, 160, "ab", cex = 1.2)
# text(3, 160, "b", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "I)", adj = 1)
# virg ####

dum<-subset(spp, spp == "virg")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SD_no.mm2.mean, (dat$SD_no.mm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F)
legend("topleft", bty = "n", cex = 1.2, "J)", adj = 1)

# ~~~5) SS ####
ll<-40
ul<-150

# hemi ####

dum<-subset(spp, spp == "hemi")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F, cex.axis = 1.2, las = 2)
legend("topleft", bty = "n", cex = 1.2, "K)", adj = 1)
# **leg ####

axis(2, at = seq(50,ul,25), las = 2)

mtext(side = 2, expression(SS), cex = 1, padj = -3.5)

# phel ####

dum<-subset(spp, spp == "phel")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F, cex.axis = 1.2, las = 2)
legend("topleft", bty = "n", cex = 1.2, "L)", adj = 1)

# shum ####

dum<-subset(spp, spp == "shum")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F, cex.axis = 1.2, las = 2)
legend("topleft", bty = "n", cex = 1.2, "M)", adj = 1)

# velu ####

dum<-subset(spp, spp == "velu")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F, cex.axis = 1.2, las = 2)

# text(1, 150, "b", cex = 1.2)
# text(2, 150, "a", cex = 1.2)
# text(3, 150, "c", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "N)", adj = 1)
# chap ####

dum<-subset(spp, spp == "chap")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F, cex.axis = 1.2, las = 2)
legend("topleft", bty = "n", cex = 1.2, "O)", adj = 1)

# marg ####

dum<-subset(spp, spp == "marg")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F, cex.axis = 1.2, las = 2)
legend("topleft", bty = "n", cex = 1.2, "P)", adj = 1)

# stel ####

dum<-subset(spp, spp == "stel")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F, cex.axis = 1.2, las = 2)

# text(1, 150, "a", cex = 1.2)
# text(2, 150, "ab", cex = 1.2)
# text(3, 150, "b", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "Q)", adj = 1)
# gemi ####

dum<-subset(spp, spp == "gemi")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F, cex.axis = 1.2, las = 2)
legend("bottomleft", bty = "n", cex = 1.2, "R)", adj = 1)

# virg ####

dum<-subset(spp, spp == "virg")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$SS_um2.mean, (dat$SS_um2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = F, cex.axis = 1.2, las = 2)

# text(1, 150, "b", cex = 1.2)
# text(2, 150, "ab", cex = 1.2)
# text(3, 150, "a", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "S)", adj = 1)






# ~~~6) SS ####
ll<-0
ul<-70

# hemi ####

dum<-subset(spp, spp == "hemi")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = c("Ambient","A+HW","W+HW"), cex.axis = 1.2, las = 2)
legend("topleft", bty = "n", cex = 1.2, "T)", adj = 1)
# **leg ####

axis(2, at = seq(0,ul,10), las = 2)

mtext(side = 2, expression(LS), cex = 1, padj = -3.5)

# phel ####

dum<-subset(spp, spp == "phel")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = c("Ambient","A+HW","W+HW"), cex.axis = 1.2, las = 2)
legend("topleft", bty = "n", cex = 1.2, "U)", adj = 1)

# shum ####

dum<-subset(spp, spp == "shum")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = c("Ambient","A+HW","W+HW"), cex.axis = 1.2, las = 2)
legend("topleft", bty = "n", cex = 1.2, "V)", adj = 1)

# velu ####

dum<-subset(spp, spp == "velu")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = c("Ambient","A+HW","W+HW"), cex.axis = 1.2, las = 2)

# text(1, 150, "b", cex = 1.2)
# text(2, 150, "a", cex = 1.2)
# text(3, 150, "c", cex = 1.2)
legend("bottomleft", bty = "n", cex = 1.2, "W)", adj = 1)
# chap ####

dum<-subset(spp, spp == "chap")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = c("Ambient","A+HW","W+HW"), cex.axis = 1.2, las = 2)
legend("topleft", bty = "n", cex = 1.2, "X)", adj = 1)

# marg ####

dum<-subset(spp, spp == "marg")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = c("Ambient","A+HW","W+HW"), cex.axis = 1.2, las = 2)
legend("topleft", bty = "n", cex = 1.2, "Y)", adj = 1)

# stel ####

dum<-subset(spp, spp == "stel")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = c("Ambient","A+HW","W+HW"), cex.axis = 1.2, las = 2)

# text(1, 150, "a", cex = 1.2)
# text(2, 150, "ab", cex = 1.2)
# text(3, 150, "b", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "Z)", adj = 1)
# gemi ####

dum<-subset(spp, spp == "gemi")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = c("Ambient","A+HW","W+HW"), cex.axis = 1.2, las = 2)
legend("topleft", bty = "n", cex = 1.2, "aa)", adj = 1)

# virg ####

dum<-subset(spp, spp == "virg")

dat<-subset(dum, Treatment.x == "Control" & Timepoint == "1")
plotCI(0.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Control" & Timepoint == "2")
plotCI(1.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "blue", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "1")
plotCI(1.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Heatwave" & Timepoint == "2")
plotCI(2.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "firebrick", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "1")
plotCI(2.8, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 1, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
par(new=T)
dat<-subset(dum, Treatment.x == "Warm+Heat" & Timepoint == "2")
plotCI(3.2, dat$LS_cm2.mean, (dat$LS_cm2.std.error), err = "y", col = "orange2", pch = 16, cex = 1.500, xlim = c(lf,rt), ylim = c(ll,ul),sfrac=0,axes=F,xlab="",ylab="")
box()

axis(1, c(1,2,3), labels = c("Ambient","A+HW","W+HW"), cex.axis = 1.2, las = 2)

# text(1, 150, "b", cex = 1.2)
# text(2, 150, "ab", cex = 1.2)
# text(3, 150, "a", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "bb)", adj = 1)

# off ####

mtext(side = 1, outer = T, cex = 1.5, "Treatment", padj = 3.5)

dev.off(); 1+1