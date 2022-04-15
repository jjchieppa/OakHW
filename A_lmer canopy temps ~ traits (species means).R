# Bring in data ####

library(car); library(sjPlot); library(doBy)
library(plotrix); library(effects); library(lme4)
library(lmerTest)

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("~/Git/OakHW")
df<-read.csv("Oak_masterfile.csv")
df$Timepoint<-as.factor(df$Timepoint)
df<-subset(df, Treatment != "Control")
df<-subset(df, Species != "mich")
df$Tcdiff<-df$Tcan-df$Tair

df<-summaryBy(Tcan + Tcdiff 
              + LS_cm2 + LMA_g.m2 + LDMC_g.g
              + SD_no.mm2 + SS_um2 + Chlor + PA_cm.cm2 + Wleaf_cm
              ~ Species, FUN = c(mean,std.error),na.rm = T, df)

df$x<-df$Wleaf_cm.mean
# df<-subset(df, Species != "velu")
m<-lm(Tcdiff.mean ~ x, df)
Anova(m)
ms<-summary(m); round(ms$r.squared, 2)
round(coef(m)[2],2)
round(coef(m)[1],2)

plot(Tcdiff.mean ~ x, df)
abline(m)