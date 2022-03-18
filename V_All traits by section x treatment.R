# indata ####
library(lme4); library(lmerTest); library(effects)
library(MuMIn); library(emmeans); library(multcomp)

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

sect<-summaryBy(LMA_g.m2 + LDMC_g.g + SD_no.mm2 + SS_um2 + Chlor + LS_cm2 ~
                 section + Treatment.x + Timepoint, FUN = c(mean,std.error), na.rm = T, df)

# Start ####


tiff(file = "V_All traits by section x treatment.tiff", height = 8, width = 6, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(6,3), mar = c(1,0,0,0), omi = c(0.8,0.8,0.2,0.1))

lf<-0.4
rt<-3.6

# ~~~1) LMA ####
ll<-60
ul<-150

# lobate ####

dum<-subset(sect, section == "lobatae")

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

mtext(side = 3, "Lobatae")
axis(1, c(1,2,3), labels = F)

# text(2, 180, "A", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "a)", adj = 1)

# **leg ####

axis(2, at = seq(50,ul,25), las = 2)

mtext(side = 2, expression(LMA~(g~m^-2)), cex = 0.7, padj = -3)

# quercus ####

dum<-subset(sect, section == "quercus")

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

mtext(side = 3, "Quercus")
axis(1, c(1,2,3), labels = F)

# text(2, 180, "B", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "b)", adj = 1)

# virentes ####

dum<-subset(sect, section == "virentes")

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

mtext(side = 3, "Virentes")
axis(1, c(1,2,3), labels = F)

# text(2, 180, "C", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "c)", adj = 1)

# ~~~2) LDMC ####
ll<-0.35
ul<-0.55

# lobate ####

dum<-subset(sect, section == "lobatae")

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
legend("topleft", bty = "n", cex = 1.2, "d)", adj = 1)

# **leg ####

axis(2, at = seq(ll,ul,0.05), las = 2)

mtext(side = 2, expression(LDMC~(g~g^-1)), cex = 0.7, padj = -3)

# quercus ####

dum<-subset(sect, section == "quercus")

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
legend("topleft", bty = "n", cex = 1.2, "e)", adj = 1)

# virentes ####

dum<-subset(sect, section == "virentes")

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
legend("topleft", bty = "n", cex = 1.2, "f)", adj = 1)

# ~~~3) Chlor ####
ll<-4
ul<-16

# lobate ####

dum<-subset(sect, section == "lobatae")

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

# text(2, 18, "A", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "g)", adj = 1)

# **leg ####

axis(2, at = seq(ll,ul,4), las = 2)

mtext(side = 2, expression(Chlor~(mu*mol~m^-2)), cex = 0.7, padj = -3)

# quercus ####

dum<-subset(sect, section == "quercus")

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

# text(2, 18, "B", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "h)", adj = 1)

# virentes ####

dum<-subset(sect, section == "virentes")

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

# text(2, 18, "C", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "i)", adj = 1)

# ~~~4) SD ####
ll<-60
ul<-150

# lobate ####

dum<-subset(sect, section == "lobatae")

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

# text(1, 175, "ab", cex = 1.2)
# text(2, 175, "ab", cex = 1.2)
# text(3, 175, "a", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "j)", adj = 1)

# **leg ####

axis(2, at = seq(50,ul,25), las = 2)

mtext(side = 2, expression(SD~(mm^-2)), cex = 0.7, padj = -3)

# quercus ####

dum<-subset(sect, section == "quercus")

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

# text(1, 175, "c", cex = 1.2)
# text(2, 175, "c", cex = 1.2)
# text(3, 175, "c", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "k)", adj = 1)

# virentes ####

dum<-subset(sect, section == "virentes")

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

# text(1, 175, "bc", cex = 1.2)
# text(2, 175, "c", cex = 1.2)
# text(3, 175, "c", cex = 1.2)
legend("topleft", bty = "n", cex = 1.2, "l)", adj = 1)

# ~~~5) SS ####
ll<-50
ul<-150

# lobate ####

dum<-subset(sect, section == "lobatae")

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
legend("topleft", bty = "n", cex = 1.2, "m)", adj = 1)

# **leg ####

axis(2, at = seq(ll,ul,25), las = 2)

mtext(side = 2, expression(SS~(mu*m^2)), cex = 0.7, padj = -3)
# text(2, 150, "A", cex = 1.2)

# quercus ####

dum<-subset(sect, section == "quercus")

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
legend("topleft", bty = "n", cex = 1.2, "n)", adj = 1)

# text(2, 150, "A", cex = 1.2)

# virentes ####
dum<-subset(sect, section == "virentes")

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
legend("topleft", bty = "n", cex = 1.2, "o)", adj = 1)

# text(2, 150, "B", cex = 1.2)




# ~~~6) LS ####
ll<-0
ul<-25

# lobate ####

dum<-subset(sect, section == "lobatae")

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
legend("topleft", bty = "n", cex = 1.2, "p)", adj = 1)

# **leg ####

axis(2, at = seq(ll,ul,5), las = 2)

mtext(side = 2, expression(LS~(cm^2)), cex = 0.7, padj = -3)
# text(2, 150, "A", cex = 1.2)

# quercus ####

dum<-subset(sect, section == "quercus")

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
legend("topleft", bty = "n", cex = 1.2, "q)", adj = 1)

# text(2, 150, "A", cex = 1.2)

# virentes ####
dum<-subset(sect, section == "virentes")

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
legend("topleft", bty = "n", cex = 1.2, "r)", adj = 1)

# text(2, 150, "B", cex = 1.2)

# off ####

mtext(side = 1, outer = T, cex = 1.5, "Treatment", padj = 3.5)

dev.off(); 1+1
