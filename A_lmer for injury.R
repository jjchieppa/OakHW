# Bring in data ####

library(car); library(sjPlot); library(lme4)
library(emmeans); library(lmerTest); library(multcomp)

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
# df<-subset(df, spp != "mich")
# df$Tldiff<-df$Tleaf-df$Tair
# df$Tcdiff<-df$Tcan-df$Tair
# df$Timepoint<-as.factor(df$Timepoint)

# model ####

m1<-lmer(InBin ~ Section + Species + Section:Treatment + Species:Treatment + Treatment + (1|Block) + (1|Timepoint), REML = T, df)
anova(m1, ddf = "Kenward-Roger")
em1<-data.frame(cld(emmeans(m1, ~ Section)))
em1
r.squaredGLMM(m1)
