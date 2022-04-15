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

# df$x<-df$SD_no.mm2

m<-lmer(LR ~ Treatment + LS_cm2 + LS_cm2:Treatment
        + LMA_g.m2 + LMA_g.m2:Treatment
        + LDMC_g.g + LDMC_g.g:Treatment
        + SD_no.mm2 + SD_no.mm2:Treatment
        + SS_um2 + SS_um2:Treatment
        + Chlor + Chlor:Treatment
        + PA_cm.cm2 + PA_cm.cm2:Treatment
        + Wleaf_cm + Wleaf_cm:Treatment
        + (1|Timepoint) + (1|Block), REML = T, df)
anova(m, ddf = "Kenward-Roger"); r.squaredGLMM(m)

x<-plot_model(m, type = "pred", terms = c("Wleaf_cm","Treatment")); x
# x<-plot_model(m, type = "pred", terms = c("reg")); x
x<-data.frame(x$data)

coef(lm(predicted ~ x, x))
# d<-subset(x, group == "Heatwave")
# coef(lm(predicted ~ x, d))
# d<-subset(x, group == "Warm+Heat")
# coef(lm(predicted ~ x, d))
