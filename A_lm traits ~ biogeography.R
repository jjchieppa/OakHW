# Bring in data ####

library(car); library(sjPlot); library(doBy)
library(plotrix); library(effects); library(lme4)
library(lmerTest)

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("~/Git/OakHW/")

df<-read.csv("Oak_masterfile.csv")
df<-subset(df, Species != "mich")
df<-summaryBy(med1+r1+med2+r2+LR+Chlor+LDMC_g.g+LMA_g.m2+SD_no.mm2+SS_um2+LS_cm2+Wleaf_cm+PA_cm.cm2
              ~Species+section+Treatment, FUN = mean, na.rm = T, df)
names(df)[4]<-"med1"
names(df)[5]<-"r1"
names(df)[6]<-"med2"
names(df)[7]<-"r2"
names(df)[8]<-"LR"
names(df)[9]<-"Chlor"
names(df)[10]<-"LDMC"
names(df)[11]<-"LMA"
names(df)[12]<-"SD"
names(df)[13]<-"SS"
names(df)[14]<-"LS"
names(df)[15]<-"LW"
names(df)[16]<-"PA"



# model selection ####

# dum<-subset(df, Treatment != "Control") # only for LR
dum<-df
dum$reg<-dum$r2

mod<-lm(dum$LW ~ reg + reg:Treatment, dum); Anova(mod); ms<-summary(mod); ms$r.squared
# summary(mod)
x<-plot_model(mod, type = "pred", terms = c("reg","Treatment")); x
