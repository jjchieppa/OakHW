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

# Start ####

tiff(file = "V+A_OakHW_Injury+Traits ~ Climate (red).tiff", height = 10, width = 8, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(9,4), mar = c(0.7,0.7,0.7,0.7), omi = c(1.1,0.6,0.1,0.1))

# lims ####
rs.ll<-as.numeric(50000)
rs.ul<-as.numeric(1200000)
rs.int<-as.numeric(200000)
m1.ll<-as.numeric(-5)
m1.ul<-as.numeric(2)
m1.int<-as.numeric(1)
r1.ll<-as.numeric(2)
r1.ul<-as.numeric(8)
q1.int<-as.numeric(0.5)
m2.ll<-as.numeric(-0.6)
m2.ul<-as.numeric(0.6)
m2.int<-as.numeric(0.2)
r2.ll<-as.numeric(2)
r2.ul<-as.numeric(10)
q2.int<-as.numeric(2)

# a) LR ~ med1 ####

df$reg<-df$med1
df$yy<-df$LR
plot(df$yy ~ df$reg, xlim = c(m1.ll, m1.ul), ylim = c(0,1.2), pch = NA, axes = F, xlab = "", ylab = "")

dum<-subset(df, Treatment != "Control")
mod<-lm(yy ~ reg + reg:Treatment, dum); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg","Treatment"))
x<-as.data.frame(x$data)
# d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", d, col = "dodgerblue4")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.24'))
legend("topleft", "a)", bty = "n", cex = 1, adj = 1.25)

axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0, 1, 0.2))
mtext(side = 2, "LR", cex = 1.2, padj = -3)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
box()

# b) LR ~ r1 ####

df$reg<-df$r1
df$yy<-df$LR
plot(df$yy ~ df$reg, xlim = c(r1.ll, r1.ul), ylim = c(0,1.2), pch = NA, axes = F, xlab = "", ylab = "")

dum<-subset(df, Treatment != "Control")
mod<-lm(yy ~ reg + reg:Treatment, dum); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg","Treatment"))
x<-as.data.frame(x$data)
# d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", d, col = "dodgerblue4")
d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.24,'~italic(P)~'= 0.09'))
legend("topleft", "b)", bty = "n", cex = 1, adj = 1.25)

axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0, 1, 0.2), labels = F)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
box()

# c) LR ~ m2 ####

df$reg<-df$med2
df$yy<-df$LR
plot(df$yy ~ df$reg, xlim = c(m2.ll, m2.ul), ylim = c(0,1.2), pch = NA, axes = F, xlab = "", ylab = "")

dum<-subset(df, Treatment != "Control")
mod<-lm(yy ~ reg + reg:Treatment, dum); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg","Treatment"))
x<-as.data.frame(x$data)
# d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", d, col = "dodgerblue4")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.59'))
legend("topleft", "c)", bty = "n", cex = 1, adj = 1.25)

axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0, 1, 0.2), labels = F)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
box()

# d) LR ~ r2 ####

df$reg<-df$r2
df$yy<-df$LR
plot(df$yy ~ df$reg, xlim = c(r2.ll, r2.ul), ylim = c(0,1.2), pch = NA, axes = F, xlab = "", ylab = "")

dum<-subset(df, Treatment != "Control")
mod<-lm(yy ~ reg + reg:Treatment, dum); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg","Treatment"))
x<-as.data.frame(x$data)
# d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", d, col = "dodgerblue4")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.11'))
legend("topleft", "d)", bty = "n", cex = 1, adj = 1.25)

axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0, 1, 0.2), labels = F)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
box()

# e) LS ~ med1 ####

df$reg<-df$med1
df$yy<-df$LS
plot(df$yy ~ df$reg, xlim = c(m1.ll, m1.ul), ylim = c(0,80), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0, 80, 20))

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
# d<-subset(x, group == "Control"); 
points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.60,'~italic(P)~'< 0.001'))
legend("topleft", "e)", bty = "n", cex = 1, adj = 1.25)
mtext(side = 2, "LS", cex = 1.2, padj = -3)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# f) LS ~ r1 ####

df$reg<-df$r1
df$yy<-df$LS
plot(df$yy ~ df$reg, xlim = c(r1.ll, r1.ul), ylim = c(0,80), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0, 80, 20), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.47,'~italic(P)~'< 0.001'))
legend("topleft", "f)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# g) LS ~ m2 ####

df$reg<-df$med2
df$yy<-df$LS
plot(df$yy ~ df$reg, xlim = c(m2.ll, m2.ul), ylim = c(0,80), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0, 80, 20), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.12,'~italic(P)~'= 0.09'))
legend("topleft", "g)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# h) LS ~ m2 ####

df$reg<-df$r2
df$yy<-df$LS
plot(df$yy ~ df$reg, xlim = c(r2.ll, r2.ul), ylim = c(0,80), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0, 80, 20), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
# d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.44'))
legend("topleft", "h)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# i) LMA ~ med1 ####

df$reg<-df$med1
df$yy<-df$LMA
plot(df$yy ~ df$reg, xlim = c(m1.ll, m1.ul), ylim = c(50,200), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(50, 200, 25))

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.35,'~italic(P)~'< 0.01'))
legend("topleft", "i)", bty = "n", cex = 1, adj = 1.25)
mtext(side = 2, "LMA", cex = 1.2, padj = -3)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# j) LMA ~ r1 ####

df$reg<-df$r1
df$yy<-df$LMA
plot(df$yy ~ df$reg, xlim = c(r1.ll, r1.ul), ylim = c(50,200), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(50, 200, 25), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
# d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.16'))
legend("topleft", "j)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# k) LMA ~ m2 ####

df$reg<-df$med2
df$yy<-df$LMA
plot(df$yy ~ df$reg, xlim = c(m2.ll, m2.ul), ylim = c(50,200), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(50, 200, 25), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.18,'~italic(P)~'< 0.05'))
legend("topleft", "k)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# l) LMA ~ m2 ####

df$reg<-df$r2
df$yy<-df$LMA
plot(df$yy ~ df$reg, xlim = c(r2.ll, r2.ul), ylim = c(50,200), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(50, 200, 25), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
# d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.81'))
legend("topleft", "l)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()






# m) LDMC ~ med1 ####

df$reg<-df$med1
df$yy<-df$LDMC
plot(df$yy ~ df$reg, xlim = c(m1.ll, m1.ul), ylim = c(0.35,0.6), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0.35,0.6,0.05))

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
# d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.61'))
legend("topleft", "m)", bty = "n", cex = 1, adj = 1.25)
mtext(side = 2, "LDMC", cex = 1.2, padj = -3)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# n) LDMC ~ r1 ####

df$reg<-df$r1
df$yy<-df$LDMC
plot(df$yy ~ df$reg, xlim = c(r1.ll, r1.ul), ylim = c(0.35,0.6), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0.35,0.6,0.05), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg","Treatment"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", d, col = "dodgerblue4")
d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.27,'~italic(P)~'< 0.05'))
legend("topleft", "n)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# o) LDMC ~ m2 ####

df$reg<-df$med2
df$yy<-df$LDMC
plot(df$yy ~ df$reg, xlim = c(m2.ll, m2.ul), ylim = c(0.35,0.6), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0.35,0.6,0.05), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.11'))
legend("topleft", "o)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# p) LDMC ~ m2 ####

df$reg<-df$r2
df$yy<-df$LDMC
plot(df$yy ~ df$reg, xlim = c(r2.ll, r2.ul), ylim = c(0.35,0.6), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0.35,0.6,0.05), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg","Treatment"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", d, col = "dodgerblue4")
d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.27,'~italic(P)~'< 0.05'))
legend("topleft", "p)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()



# q) PA ~ med1 ####

df$reg<-df$med1
df$yy<-df$PA
plot(df$yy ~ df$reg, xlim = c(m1.ll, m1.ul), ylim = c(0.5,4.5), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0.5,4.5,1))

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.44,'~italic(P)~'< 0.01'))
legend("topleft", "q)", bty = "n", cex = 1, adj = 1.25)
mtext(side = 2, "PA", cex = 1.2, padj = -3)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# r) PA ~ r1 ####

df$reg<-df$r1
df$yy<-df$PA
plot(df$yy ~ df$reg, xlim = c(r1.ll, r1.ul), ylim = c(0.5,4.5), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0.5,4.5,1), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.21,'~italic(P)~'< 0.05'))
legend("topleft", "r)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# s) PA ~ m2 ####

df$reg<-df$med2
df$yy<-df$PA
plot(df$yy ~ df$reg, xlim = c(m2.ll, m2.ul), ylim = c(0.5,4.5), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0.5,4.5,1), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.12,'~italic(P)~'= 0.05'))
legend("topleft", "s)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# t) PA ~ m2 ####

df$reg<-df$r2
df$yy<-df$PA
plot(df$yy ~ df$reg, xlim = c(r2.ll, r2.ul), ylim = c(0.5,4.5), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0.5,4.5,1), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
# d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.75'))
legend("topleft", "t)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()



# u) LW ~ med1 ####

df$reg<-df$med1
df$yy<-df$LW
plot(df$yy ~ df$reg, xlim = c(m1.ll, m1.ul), ylim = c(1,8), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0,8,1))

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.55,'~italic(P)~'< 0.001'))
legend("topleft", "u)", bty = "n", cex = 1, adj = 1.25)
mtext(side = 2, "LW", cex = 1.2, padj = -3)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# v) LW ~ r1 ####

df$reg<-df$r1
df$yy<-df$LW
plot(df$yy ~ df$reg, xlim = c(r1.ll, r1.ul), ylim = c(1,8), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0,8,1), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.40,'~italic(P)~'< 0.001'))
legend("topleft", "v)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# w) LW ~ m2 ####

df$reg<-df$med2
df$yy<-df$LW
plot(df$yy ~ df$reg, xlim = c(m2.ll, m2.ul), ylim = c(1,8), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0,8,1), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.30,'~italic(P)~'= 0.01'))
legend("topleft", "w)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# x) LW ~ m2 ####

df$reg<-df$r2
df$yy<-df$LW
plot(df$yy ~ df$reg, xlim = c(r2.ll, r2.ul), ylim = c(1,8), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(0,8,1), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
# d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.28'))
legend("topleft", "x)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()



# y) SD ~ med1 ####

df$reg<-df$med1
df$yy<-df$SD
plot(df$yy ~ df$reg, xlim = c(m1.ll, m1.ul), ylim = c(50,175), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(50,175,25))

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.23,'~italic(P)~'< 0.05'))
legend("topleft", "y)", bty = "n", cex = 1, adj = 1.25)
mtext(side = 2, "SD", cex = 1.2, padj = -3)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# z) SD ~ r1 ####

df$reg<-df$r1
df$yy<-df$SD
plot(df$yy ~ df$reg, xlim = c(r1.ll, r1.ul), ylim = c(50,175), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(50,175,25), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.17,'~italic(P)~'= 0.05'))
legend("topleft", "z)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# A) SD ~ m2 ####

df$reg<-df$med2
df$yy<-df$SD
plot(df$yy ~ df$reg, xlim = c(m2.ll, m2.ul), ylim = c(50,175), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(50,175,25), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.55'))
legend("topleft", "A)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# B) SD ~ m2 ####

df$reg<-df$r2
df$yy<-df$SD
plot(df$yy ~ df$reg, xlim = c(r2.ll, r2.ul), ylim = c(50,175), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(50,175,25), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
# d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.16'))
legend("topleft", "B)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()



# C) SS ~ med1 ####

df$reg<-df$med1
df$yy<-df$SS
plot(df$yy ~ df$reg, xlim = c(m1.ll, m1.ul), ylim = c(50,175), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(50,175,25))

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.23,'~italic(P)~'< 0.05'))
legend("topleft", "C)", bty = "n", cex = 1, adj = 1.25)
mtext(side = 2, "SS", cex = 1.2, padj = -3)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# D) SS ~ r1 ####

df$reg<-df$r1
df$yy<-df$SS
plot(df$yy ~ df$reg, xlim = c(r1.ll, r1.ul), ylim = c(50,175), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(50,175,25), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
# d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.21'))
legend("topleft", "D)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# E) SS ~ m2 ####

df$reg<-df$med2
df$yy<-df$SS
plot(df$yy ~ df$reg, xlim = c(m2.ll, m2.ul), ylim = c(50,175), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(50,175,25), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.32,'~italic(P)~'< 0.01'))
legend("topleft", "E)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# F) SS ~ m2 ####

df$reg<-df$r2
df$yy<-df$SS
plot(df$yy ~ df$reg, xlim = c(r2.ll, r2.ul), ylim = c(50,175), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, labels = F)
axis(2, cex.axis = 1.1, las = 2, at = seq(50,175,25), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
# d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.75'))
legend("topleft", "F)", bty = "n", cex = 1, adj = 1.25)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()



# G) Chlor ~ med1 ####

df$reg<-df$med1
df$yy<-df$Chlor
plot(df$yy ~ df$reg, xlim = c(m1.ll, m1.ul), ylim = c(5,17), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, at = seq(-5,2,1),
     labels = c("Warmer -5","-4","-3","-2","-1","0",
                                            "1","Cooler 2"))
axis(2, cex.axis = 1.1, las = 2, at = seq(5,17,2))

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.27'))
legend("topleft", "G)", bty = "n", cex = 1, adj = 1.25)
mtext(side = 2, "Chlor", cex = 1.2, padj = -3)

mtext(side = 1, expression(PC1[tilde(x)]), padj = 5.5)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# H) Chlor ~ r1 ####

df$reg<-df$r1
df$yy<-df$Chlor
plot(df$yy ~ df$reg, xlim = c(r1.ll, r1.ul), ylim = c(5,17), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, at = seq(2,8,1))
axis(2, cex.axis = 1.1, las = 2, at = seq(5,17,2), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.79'))
legend("topleft", "H)", bty = "n", cex = 1, adj = 1.25)

mtext(side = 1, expression(PC1[R]), padj = 5.5)
i
dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# I) Chlor ~ m2 ####

df$reg<-df$med2
df$yy<-df$Chlor
plot(df$yy ~ df$reg, xlim = c(m2.ll, m2.ul), ylim = c(5,17), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, at = seq(-0.6,0.6,0.2), 
     labels = c("Wetter -0.6","-0.4","-0.2","0","0.2","0.4","Drier 0.6"))
axis(2, cex.axis = 1.1, las = 2, at = seq(5,17,2), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(P)~'= 0.50'))
legend("topleft", "I)", bty = "n", cex = 1, adj = 1.25)

mtext(side = 1, expression(PC2[tilde(x)]), padj = 5.5)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()

# J) Chlor ~ m2 ####

df$reg<-df$r2
df$yy<-df$Chlor
plot(df$yy ~ df$reg, xlim = c(r2.ll, r2.ul), ylim = c(5,17), pch = NA, axes = F, xlab = "", ylab = "")
axis(1, cex.axis = 1.2, las = 2, at = seq(2,10,2))
axis(2, cex.axis = 1.1, las = 2, at = seq(5,17,2), labels = F)

mod<-lm(yy ~ reg + reg:Treatment, df); Anova(mod); ms<-summary(mod); ms$r.squared
x<-plot_model(mod, type = "pred", terms = c("reg"))
x<-as.data.frame(x$data)
d<-subset(x, group == "Control"); points(predicted ~ x, type = "l", x, col = "black")
# d<-subset(x, group == "Heatwave"); points(predicted ~ x, type = "l", d, col = "firebrick")
# d<-subset(x, group == "Warm+Heat"); points(predicted ~ x, type = "l", d, col = "orange2")

legend("top", bty = "n", expression(italic(R)^2~'= 0.14,'~italic(P)~'= 0.09'))
legend("topleft", "J)", bty = "n", cex = 1, adj = 1.25)

mtext(side = 1, expression(PC2[R]), padj = 5.5)

dat<-subset(df, Treatment == "Heatwave" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Heatwave" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "firebrick", cex = 1.1)
dat<-subset(df, Treatment == "Warm+Heat" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "orange2")
dat<-subset(df, Treatment == "Warm+Heat" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "orange2")
dat<-subset(df, Treatment == "Control" & section == "quercus"); points(yy ~ reg, dat, pch = 15, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "lobatae"); points(yy ~ reg, dat, pch = 16, col = "dodgerblue4")
dat<-subset(df, Treatment == "Control" & section == "virentes"); points(yy ~ reg, dat, pch = 17, col = "dodgerblue4")
box()



# off ####

dev.off()