# Bring in data ####

library(car); library(sjPlot); library(doBy)
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
df<-merge(dm, df, by = "PlantID")
rm(dm)
df<-subset(df, spp != "mich")
df$Tldiff<-df$Tleaf-df$Tair
df$Tcdiff<-df$Tcan-df$Tair
df$Timepoint<-as.factor(df$Timepoint)
df<-subset(df, Treatment.x != "Control")

df<-summaryBy(DIA_mm + HT_mm + Chlor + Tleaf + Tcan + Tair + RH + Tldiff + Tcdiff + LCdiff + LDMC_g.g
              + LMA_g.m2 + Aperture_um + SD_no.mm2 + SS_um2 + gwmax + TotalLeafArea_cm2 + LS_cm2
              ~ spp, na.rm = T,
              FUN = c(mean, std.error), df)

# Start plot ####

tiff(file = "V+A_TcdiffCI ~ Leaf Traits (no ambients).tiff", height = 7, width = 10, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(2,3), mar = c(2,2,3,1), omi = c(0.5,0.5,0.3,0.01))
df$yy<-df$Tcdiff.mean
df$ys<-df$Tcdiff.std.error
y1=-2
y2=3

# LDMC ####

df$xx<-df$LDMC_g.g.mean
df$xs<-df$LDMC_g.g.std.error

x1=0.395
x2=0.5
plotCI(df$xx, df$yy, 0, err = "x", pch = NA, axes = F, xlim = c(x1,x2), ylim = c(y1,y2)); par(new=T)

axis(2, seq(-3,4,1), las = 2, cex.axis = 1.4)
axis(1, cex.axis = 1.4)

m<-lm(yy ~ xx, df); anova(m); summary(m)
legend("topleft", "a)", bty = "n", cex = 1.2, adj = 1)
legend("topright", bty = "n", expression(italic(R)^2~'= 0.04,'~italic(P)~'= 0.62'), cex = 1.2)
mtext(side = 1, expression(LDMC~(g~g^-1)), padj = 2)
# ablineclip(m, x1 = min(df$xx)-max(df$xs), x2 = max(df$xx)+max(df$xs))

text(0.41, 0.25, "Warming", cex = 1.2, col = "firebrick")
text(0.41, -0.25, "Cooling", cex = 1.2, col = "dodgerblue4")

abline(h = 0, lty = 2, col = "grey"); par(new=T)
dum<-subset(df, spp == "stel")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "firebrick", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "firebrick", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "velu")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "orange2", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "orange2", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "virg")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "yellow3", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "yellow3", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "chap")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "green4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "green4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "gemi")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "dodgerblue", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "dodgerblue", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "hemi")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "violet", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "violet", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "marg")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "purple4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "purple4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "phel")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "grey10", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "grey10", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "shum")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "grey50", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "grey50", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)

# LMA ####

df$xx<-df$LMA_g.m2.mean
df$xs<-df$LMA_g.m2.std.error

x1=60
x2=140
plotCI(df$xx, df$yy, 0, err = "x", pch = NA, axes = F, xlim = c(x1,x2), ylim = c(y1,y2)); par(new=T)

axis(2, seq(-3,4,1), las = 2, cex.axis = 1.4)
axis(1, cex.axis = 1.4)

m<-lm(yy ~ xx, df); anova(m); summary(m)
legend("topleft", "b)", bty = "n", cex = 1.2, adj = 1)
legend("topright", bty = "n", expression(italic(R)^2~'= 0.37,'~italic(P)~'= 0.08'), cex = 1.2)
mtext(side = 1, expression(LMA~(g~m^-2)), padj = 2)
ablineclip(m, x1 = min(df$xx)-max(df$xs), x2 = max(df$xx)+max(df$xs))

abline(h = 0, lty = 2, col = "grey"); par(new=T)
dum<-subset(df, spp == "stel")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "firebrick", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "firebrick", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "velu")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "orange2", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "orange2", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "virg")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "yellow3", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "yellow3", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "chap")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "green4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "green4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "gemi")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "dodgerblue", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "dodgerblue", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "hemi")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "violet", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "violet", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "marg")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "purple4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "purple4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "phel")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "grey10", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "grey10", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "shum")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "grey50", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "grey50", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)

# SD ####

df$xx<-df$SD_no.mm2.mean
df$xs<-df$SD_no.mm2.std.error

x1=60
x2=140
plotCI(df$xx, df$yy, 0, err = "x", pch = NA, axes = F, xlim = c(x1,x2), ylim = c(y1,y2)); par(new=T)

axis(2, seq(-3,4,1), las = 2, cex.axis = 1.4)
axis(1, cex.axis = 1.4)

m<-lm(yy ~ xx, df); anova(m); summary(m)
ablineclip(m, x1 = min(df$xx)-max(df$xs), x2 = max(df$xx)+max(df$xs))
legend("topleft", "c)", bty = "n", cex = 1.2, adj = 1)
legend("topright", bty = "n", expression(italic(R)^2~'= 0.67,'~italic(P)~'= 0.007'), cex = 1.2)
mtext(side = 1, expression(SD~(mm^-2)), padj = 2)

abline(h = 0, lty = 2, col = "grey"); par(new=T)
dum<-subset(df, spp == "stel")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "firebrick", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "firebrick", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "velu")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "orange2", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "orange2", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "virg")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "yellow3", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "yellow3", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "chap")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "green4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "green4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "gemi")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "dodgerblue", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "dodgerblue", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "hemi")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "violet", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "violet", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "marg")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "purple4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "purple4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "phel")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "grey10", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "grey10", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "shum")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "grey50", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "grey50", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)

# SS ####

df$xx<-df$SS_um2.mean
df$xs<-df$SS_um2.std.error

x1=40
x2=150
plotCI(df$xx, df$yy, 0, err = "x", pch = NA, axes = F, xlim = c(x1,x2), ylim = c(y1,y2)); par(new=T)

axis(2, seq(-3,4,1), las = 2, cex.axis = 1.4)
axis(1, cex.axis = 1.4)

m<-lm(yy ~ xx, df); anova(m); summary(m)
legend("topleft", "d)", bty = "n", cex = 1.2, adj = 1)
legend("topright", bty = "n", expression(italic(R)^2~'= 0.30,'~italic(P)~'= 0.13'), cex = 1.2)
mtext(side = 1, expression(SS~(mu*m^2)), padj = 2)
# ablineclip(m, x1 = min(df$xx)-max(df$xs), x2 = max(df$xx)+max(df$xs))

abline(h = 0, lty = 2, col = "grey"); par(new=T)
dum<-subset(df, spp == "stel")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "firebrick", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "firebrick", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "velu")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "orange2", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "orange2", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "virg")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "yellow3", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "yellow3", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "chap")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "green4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "green4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "gemi")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "dodgerblue", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "dodgerblue", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "hemi")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "violet", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "violet", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "marg")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "purple4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "purple4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "phel")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "grey10", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "grey10", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "shum")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "grey50", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "grey50", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)

# Chlor ####

df$xx<-df$Chlor.mean
df$xs<-df$Chlor.std.error

x1=5
x2=15
plotCI(df$xx, df$yy, 0, err = "x", pch = NA, axes = F, xlim = c(x1,x2), ylim = c(y1,y2)); par(new=T)

axis(2, seq(-3,4,1), las = 2, cex.axis = 1.4)
axis(1, cex.axis = 1.4)

m<-lm(yy ~ xx, df); anova(m); summary(m)
legend("topleft", "e)", bty = "n", cex = 1.2, adj = 1)
legend("topright", bty = "n", expression(italic(R)^2~'= 0.39, '~italic(P)~'= 0.07'), cex = 1.2)
mtext(side = 1, expression(Chlor~(mu*mol~m^-2)), padj = 2)
ablineclip(m, x1 = min(df$xx)-max(df$xs), x2 = max(df$xx)+max(df$xs))

abline(h = 0, lty = 2, col = "grey"); par(new=T)
dum<-subset(df, spp == "stel")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "firebrick", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "firebrick", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "velu")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "orange2", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "orange2", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "virg")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "yellow3", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "yellow3", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "chap")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "green4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "green4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "gemi")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "dodgerblue", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "dodgerblue", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "hemi")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "violet", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "violet", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "marg")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "purple4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "purple4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "phel")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "grey10", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "grey10", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "shum")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "grey50", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "grey50", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)

# LS ####

df$xx<-df$LS_cm2.mean
df$xs<-df$LS_cm2.std.error

x1=-5
x2=55
plotCI(df$xx, df$yy, 0, err = "x", pch = NA, axes = F, xlim = c(x1,x2), ylim = c(y1,y2)); par(new=T)

axis(2, seq(-3,4,1), las = 2, cex.axis = 1.4)
axis(1, cex.axis = 1.4)

m<-lm(yy ~ xx, df); anova(m); summary(m)
ablineclip(m, x1 = min(df$xx)-max(df$xs), x2 = max(df$xx)+max(df$xs))

dum<-subset(df, spp != "velu")
m<-lm(yy ~ xx, dum); anova(m); summary(m)
# ablineclip(m, x1 = min(dum$xx)-max(dum$xs), x2 = max(dum$xx)+max(dum$xs), lty = 2)


legend("topleft", "f)", bty = "n", cex = 1.2, adj = 1)
legend("top", bty = "n", expression(italic(R)^2~'= 0.72,'~italic(P)~'= 0.004'), cex = 1.2)
legend("bottom", bty = "n", expression(italic(R)^2~'= 0.37,'~italic(P)~'= 0.11'), cex = 1.2)


mtext(side = 1, expression(LS~(cm^2)), padj = 2)


abline(h = 0, lty = 2, col = "grey"); par(new=T)
dum<-subset(df, spp == "stel")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "firebrick", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "firebrick", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "velu")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "orange2", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "orange2", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "virg")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "yellow3", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "yellow3", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "chap")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "green4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "green4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "gemi")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "dodgerblue", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "dodgerblue", pch = 17, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "hemi")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "violet", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "violet", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "marg")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "purple4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "purple4", pch = 15, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "phel")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "grey10", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "grey10", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)
par(new=T)
dum<-subset(df, spp == "shum")
plotCI(dum$xx, dum$yy, dum$xs, err = "x", col = "grey50", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75); par(new=T)
plotCI(dum$xx, dum$yy, dum$ys, err = "y", col = "grey50", pch = 16, sfrac = 0, xaxt="n", yaxt="n", xlim = c(x1,x2), ylim = c(y1,y2), cex = 1.75)

# end ####

mtext(side = 2, outer = T, expression(delta[canopy]~(degree*C)), cex = 1.5, padj = -0.6)


par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 0), mar = c(0, 0, 0, 0), new = TRUE)
plot(0, 0, type = "n", bty = "n", xaxt = "n", yaxt = "n")
legend("top", bty="n", cex = 1.5, c("stel","chap","marg"),
       col = c("firebrick","green4","purple4"), pch = 15, horiz = T)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 1.5, 0), mar = c(0, 0, 0, 0), new = TRUE)
legend("top", bty="n", cex = 1.5, c("velu","hemi","phel","shum"), 
       col = c("orange2","violet","grey10","grey50"), pch = 16, horiz = T)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 3.0, 0), mar = c(0, 0, 0, 0), new = TRUE)
legend("top", bty="n", cex = 1.5, c("virg","gemi"),
       col = c("yellow3","dodgerblue"), pch = 17, horiz = T)

par(fig = c(0, 1, 0, 1), oma = c(0, 0, 0, 15), mar = c(0, 0, 0, 0), new = TRUE)
legend("topright", bty="n", cex = 1.5, c("Quercus","Lobatae","Virentes"), pch = c(15,16,17), horiz = F,
       col = "black")

# plot(1 ~ 0, pch=NA,xaxt="n",yaxt="n",xlab="",ylab="")
# 
# legend(0.55, 1.3, bty="n", cex = 1.5, c("stel","chap","marg"),
# col = c("firebrick","green4","purple4"), pch = 15, horiz = T)
# legend(0.55, 1.2, bty="n", cex = 1.5, c("velu","hemi","phel","shum"), 
#        col = c("orange2","violet","grey10","grey50"), pch = 16, horiz = T)
# legend(0.55, 1.1, bty="n", cex = 1.5, c("virg","gemi"),
#        col = c("yellow3","dodgerblue"), pch = 17, horiz = T)
# 
# legend("bottomleft", bty="n", cex = 1.5, c("Quercus","Lobatae","Virentes"), pch = c(15,16,17), horiz = F,
#        col = "black")

# stel15firebrick chap15green4 marg15purple4
# velu16orange2 hemi16violet phel16grey10 shum16grey50
# virg17yellow gemi17dodgerblue

# off ####
dev.off()