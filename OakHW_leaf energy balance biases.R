# in data ####
library(plantecophys); library(car); library(doBy)
library(plotrix)

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("C:/Users/jjchi/OneDrive - Auburn University/Other Projects/Oak Heatwaves")
df<-read.csv("Oak_masterfile.csv")
df$SVP_kPa<-(610.78*(2.71828^(df$Tair/(df$Tair+238.3) * 17.2694)))/1000
df$VPD_kPa<-df$SVP_kPa * (1 - (df$RH/100))
df$PA<-df$Perimeter_cm/df$JArea_cm2
df<-with(df, data.frame(Tair,VPD_kPa,RH,Qin,Tcan,df$Tleaf,LDMC_g.g,PA, PAR,
                        LMA_g.m2,LS_cm2,SD_no.mm2,SS_um2, Chlor, Wleaf_cm,spp,PlantID))

# aci<-read.csv("OakWarming_Mastersheet ACi.csv")
# aci<-subset(aci, KEEP == "Y")
# aci<-subset(aci, CO2R_in < 430); aci<-subset(aci, CO2R_in > 410)
# aci$Cond_out<-ifelse(aci$Cond_out>1,aci$Cond_out/1000,aci$Cond_out)
# # aci<-with(aci, data.frame(Cond_out, UserIDs_in))
# aci$spp<-as.factor(substr(aci$UserIDs_in, 1, 4))
# aci<-summaryBy(Cond_out ~ spp, FUN = c(mean), na.rm = T, aci)
# df<-merge(aci, df, by = c("spp"), all = T)

df<-df[complete.cases(df),]


# model & prep ####

out<-PhotosynEB(Tair = df$Tair,
                VPD = df$VPD_kPa,
                PPFD = as.numeric(df$Qin),
                RH = df$RH,
                # GS = df$Cond_out.mean,
                Wleaf=as.numeric(df$Wleaf_cm/100),
                Wind = 1.075,
                StomatalRatio = 1
)

new<-cbind(out,df)
trt<-read.csv("Oak treatment assignments.csv")
new<-merge(trt, new, by = "PlantID")
rm(trt,out,df)
new<-subset(new, Treatment != "Control")
new$spp<-as.factor(new$spp)
df<-summaryBy(Tcan+df.Tleaf+Tleaf+
                LDMC_g.g+PA+LMA_g.m2+LS_cm2+SD_no.mm2
              +SS_um2+Chlor+Wleaf_cm~ spp, FUN = c(mean, std.error), na.rm = T, new)

# start ####

# tiff

par(mfrow = c(2,1), omi = c(0.5,0.5,0.1,0.1), mar = c(1.5,1.5,1.5,1.5))

# a) pred ~ Tcan ####

xl<-30
xh<-50
yl<-30
yh<-50

plotCI(df$Tleaf.mean, df$Tcan.mean, df$Tleaf.std.error, col = "white", xlim = c(xl,xh), ylim = c(yl,yh))

df$xx<-df$Tleaf.mean
df$xs<-df$Tleaf.std.error
df$yy<-df$Tcan.mean
df$ys<-df$Tcan.std.error

dat<-subset(df, spp == "stel")
par(new=T); plotCI(dat$xx, dat$yy, dat$ys, col = "firebrick", err = "y", pch = 15, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)
par(new=T); plotCI(dat$xx, dat$yy, dat$xs, col = "firebrick", err = "x", pch = 15, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)
dat<-subset(df, spp == "chap")
par(new=T); plotCI(dat$xx, dat$yy, dat$ys, col = "forestgreen", err = "y", pch = 15, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)
par(new=T); plotCI(dat$xx, dat$yy, dat$xs, col = "forestgreen", err = "x", pch = 15, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)
dat<-subset(df, spp == "marg")
par(new=T); plotCI(dat$xx, dat$yy, dat$ys, col = "purple4", err = "y", pch = 15, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)
par(new=T); plotCI(dat$xx, dat$yy, dat$xs, col = "purple4", err = "x", pch = 15, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)

dat<-subset(df, spp == "velu")
par(new=T); plotCI(dat$xx, dat$yy, dat$ys, col = "orange2", err = "y", pch = 16, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)
par(new=T); plotCI(dat$xx, dat$yy, dat$xs, col = "orange2", err = "x", pch = 16, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)
dat<-subset(df, spp == "hemi")
par(new=T); plotCI(dat$xx, dat$yy, dat$ys, col = "violet", err = "y", pch = 16, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)
par(new=T); plotCI(dat$xx, dat$yy, dat$xs, col = "violet", err = "x", pch = 16, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)
dat<-subset(df, spp == "phel")
par(new=T); plotCI(dat$xx, dat$yy, dat$ys, col = "grey10", err = "y", pch = 16, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)
par(new=T); plotCI(dat$xx, dat$yy, dat$xs, col = "grey10", err = "x", pch = 16, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)
dat<-subset(df, spp == "shum")
par(new=T); plotCI(dat$xx, dat$yy, dat$ys, col = "grey50", err = "y", pch = 16, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)
par(new=T); plotCI(dat$xx, dat$yy, dat$xs, col = "grey50", err = "x", pch = 16, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)

dat<-subset(df, spp == "virg")
par(new=T); plotCI(dat$xx, dat$yy, dat$ys, col = "yellow3", err = "y", pch = 17, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)
par(new=T); plotCI(dat$xx, dat$yy, dat$xs, col = "yellow3", err = "x", pch = 17, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)
dat<-subset(df, spp == "gemi")
par(new=T); plotCI(dat$xx, dat$yy, dat$ys, col = "dodgerblue", err = "y", pch = 17, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)
par(new=T); plotCI(dat$xx, dat$yy, dat$xs, col = "dodgerblue", err = "x", pch = 17, xlim = c(xl,xh), ylim = c(yl,yh), sfrac = 0, axes = F)

m<-lm(df$yy ~ df$xx); Anova(m)
ablineclip(m, x1 = min(df$xx), x2 = max(df$xx))
abline(1,1, lwd = 2, lty = 2, col = "grey50")

# bias ####

df$bias<-((predict(m)-df$Tcan.mean)/df$Tcan.mean)*100

df$x<-df$SD_no.mm2.mean
plot(df$bias ~ df$x, col = "white")


dat<-subset(df, spp == "stel")
points(dat$bias ~ dat$x, pch = 15, col = "firebrick")
dat<-subset(df, spp == "chap")
points(dat$bias ~ dat$x, pch = 15, col = "forestgreen")
dat<-subset(df, spp == "marg")
points(dat$bias ~ dat$x, pch = 15, col = "purple4")

dat<-subset(df, spp == "velu")
points(dat$bias ~ dat$x, pch = 16, col = "orange2")
dat<-subset(df, spp == "hemi")
points(dat$bias ~ dat$x, pch = 16, col = "violet")
dat<-subset(df, spp == "phel")
points(dat$bias ~ dat$x, pch = 16, col = "grey10")
dat<-subset(df, spp == "shum")
points(dat$bias ~ dat$x, pch = 16, col = "grey50")

dat<-subset(df, spp == "virg")
points(dat$bias ~ dat$x, pch = 17, col = "yellow3")
dat<-subset(df, spp == "gemi")
points(dat$bias ~ dat$x, pch = 17, col = "dodgerblue")

m<-lm(df$bias ~ df$x); Anova(m)
abline(m)
abline(h=0, lty = 2, col = "grey")
