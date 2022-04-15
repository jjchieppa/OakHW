# indata ####
library(lme4); library(lmerTest); library(effects)
library(MuMIn); library(emmeans); library(multcomp)
library(doBy); library(plotrix); library(sjPlot)
library(car)

rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

setwd("~/Git/OakHW")
df<-read.csv("Oak_masterfile.csv")
# dm<-read.csv("Oak_biomass data.csv")
# dm$spp<-NULL; dm$Species<-NULL
# names(dm)[3]<-"PlantID"
# names(dm)[11]<-"DM_LMA_g.m2"
# df<-merge(dm, df, by = "PlantID")
# rm(dm)
df<-subset(df, spp != "mich")
# df<-subset(df, All.Leaves.Dead != "Y")
df$Tldiff<-df$Tleaf-df$Tair
df$Tcdiff<-df$Tcan-df$Tair
df$Timepoint<-as.factor(df$Timepoint)


# good mod ####

# df<-subset(df, Treatment != "Control")

df$x<-df$Wleaf_cm
summaryBy(x ~ Section+Treatment, FUN = c(mean, std.error), na.rm = T, df)
m1<-lmer(x ~ Treatment + Section + Section:Treatment + Species + Species:Treatment + (1|Block) + (1|Timepoint), REML = T, df) # + (1|Timepoint)
# m1<-lmer(x ~ Treatment + Section + Section:Treatment + Species + Species:Treatment + (1|Block), REML = T, df) # + (1|Timepoint)
anova(m1, ddf = "Kenward-Roger")
r.squaredGLMM(m1)
c1<-cld(emmeans(m1, ~"Section:Treatment"))
c1
