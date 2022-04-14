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

df$x<-df$LS_cm2

m<-lmer(LR ~ x * Treatment  + (1|Timepoint) + (1|Block), REML = T, df)
anova(m, ddf = "Kenward-Roger"); r.squaredGLMM(m)

# x<-plot_model(m, type = "pred", terms = c("x","Treatment"))
# x<-plot_model(m, type = "pred", terms = c("reg")); x
# x<-data.frame(x$data)

# coef(lm(predicted ~ x, x))
# d<-subset(x, group == "Heatwave")
# coef(lm(predicted ~ x, d))
# d<-subset(x, group == "Warm+Heat")
# coef(lm(predicted ~ x, d))
