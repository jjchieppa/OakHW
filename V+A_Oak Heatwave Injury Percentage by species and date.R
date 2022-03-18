# start ####
library(effects); library(doBy); library(plotrix)
library(RVAideMemoire)

setwd("C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Oak Heatwaves")
setwd("~/Git/OakHW/")
rm(list=ls()) # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
dev.off()     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~
cat("\f")     # ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ ~

# read-in #####

# df<-read.csv("Oak_Heat injury surveys.csv")
# trt<-read.csv("Oak treatment assignments.csv")
# df<-merge(df, trt, by = c("PlantID"))
# write.csv(df, "Oak_Heat injury surveys w trts.csv", row.names = F)
# df<-read.csv("Oak_Heat injury surveys w trts.csv")
# setwd("C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Oak Heatwaves/Analysis")
df<-read.csv("Oak_Heat injury surveys w trts.csv")
mod<-lm(Injury_0to2 ~ Species * Treatment * Date, df)
anova(mod)
# plot(allEffects(mod))

df$x<-df$Injury_0to2
# df<-summaryBy(x ~ Species * Treatment * Date, FUN = c(mean, std.error), na.rm = T, df)

# beginning ####

# setwd("C:/Users/Jeff/OneDrive - Auburn University/Other Projects/Oak Heatwaves/Analysis")

tiff(file = "V+A_Oak Heatwave Injury Percentage by species and date.tiff", height = 10, width = 12, res = 600, units = "in", compression = "zip+p")

par(mfrow = c(5,3), omi = c(0.8,0.7,0.1,0.01), mar = c(1.75,1.3,1.5,1))

# Start plot ####

# start hemi ####

dum<-subset(df, Species == "hemi")

plot(1 ~ 1, pch = NA, xlim = c(0,16), ylim = c(0,1.2), xaxt="n",yaxt="n", xlab="",ylab="")
axis(2, at = seq(0,2,0.25), cex.axis = 1.2, las = 2, labels = seq(0,200,25))
axis(1, at = c(0.5, 1.5, 2.5, 4.5, 5.5, 6.5,
               9.5, 10.5, 11.5, 13.5, 14.5, 15.5), labels = F)
mtext(side = 3, expression(italic(Q.~hemi)), cex = 1.3)

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(00, 00, 01, no)
rect(00, (no), 01, (no+mi), col= "grey")
rect(00, (no+mi), 01, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(01, 00, 02, no)
rect(01, (no), 02, (no+mi), col= "grey")
rect(01, (no+mi), 02, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(02, 00, 03, no)
rect(02, (no), 03, (no+mi), col= "grey")
rect(02, (no+mi), 03, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(04, 00, 05, no)
rect(04, (no), 05, (no+mi), col= "grey")
rect(04, (no+mi), 05, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(05, 00, 06, no)
rect(05, (no), 06, (no+mi), col= "grey")
rect(05, (no+mi), 06, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(06, 00, 07, no)
rect(06, (no), 07, (no+mi), col= "grey")
rect(06, (no+mi), 07, (no+mi+hi), col= "black")

text(1.5, 1.15, "Pre-HW1", cex = 0.8)
text(5.5, 1.15, "Post-HW1", cex = 0.8)

abline(v = 8, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(09, 00, 10, no)
rect(09, (no), 10, (no+mi), col= "grey")
rect(09, (no+mi), 10, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(10, 00, 11, no)
rect(10, (no), 11, (no+mi), col= "grey")
rect(10, (no+mi), 11, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(11, 00, 12, no)
rect(11, (no), 12, (no+mi), col= "grey")
rect(11, (no+mi), 12, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(13, 00, 14, no)
rect(13, (no), 14, (no+mi), col= "grey")
rect(13, (no+mi), 14, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(14, 00, 15, no)
rect(14, (no), 15, (no+mi), col= "grey")
rect(14, (no+mi), 15, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(15, 00, 16, no)
rect(15, (no), 16, (no+mi), col= "grey")
rect(15, (no+mi), 16, (no+mi+hi), col= "black")

text(10.5, 1.15, "Pre-HW2", cex = 0.8)
text(14.5, 1.15, "Post-HW2", cex = 0.8)

abline(v = 17, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

# end hemi ####

# start chap ####

dum<-subset(df, Species == "chap")

plot(1 ~ 1, pch = NA, xlim = c(0,16), ylim = c(0,1.2), xaxt="n",yaxt="n", xlab="",ylab="")
axis(2, at = seq(0,2,0.25), labels = F)
axis(1, at = c(0.5, 1.5, 2.5, 4.5, 5.5, 6.5,
               9.5, 10.5, 11.5, 13.5, 14.5, 15.5), labels = F)
mtext(side = 3, expression(italic(Q.~chap)), cex = 1.3)

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(00, 00, 01, no)
rect(00, (no), 01, (no+mi), col= "grey")
rect(00, (no+mi), 01, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(01, 00, 02, no)
rect(01, (no), 02, (no+mi), col= "grey")
rect(01, (no+mi), 02, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(02, 00, 03, no)
rect(02, (no), 03, (no+mi), col= "grey")
rect(02, (no+mi), 03, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(04, 00, 05, no)
rect(04, (no), 05, (no+mi), col= "grey")
rect(04, (no+mi), 05, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(05, 00, 06, no)
rect(05, (no), 06, (no+mi), col= "grey")
rect(05, (no+mi), 06, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(06, 00, 07, no)
rect(06, (no), 07, (no+mi), col= "grey")
rect(06, (no+mi), 07, (no+mi+hi), col= "black")

text(1.5, 1.15, "Pre-HW1", cex = 0.8)
text(5.5, 1.15, "Post-HW1", cex = 0.8)

abline(v = 8, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(09, 00, 10, no)
rect(09, (no), 10, (no+mi), col= "grey")
rect(09, (no+mi), 10, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(10, 00, 11, no)
rect(10, (no), 11, (no+mi), col= "grey")
rect(10, (no+mi), 11, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(11, 00, 12, no)
rect(11, (no), 12, (no+mi), col= "grey")
rect(11, (no+mi), 12, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(13, 00, 14, no)
rect(13, (no), 14, (no+mi), col= "grey")
rect(13, (no+mi), 14, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(14, 00, 15, no)
rect(14, (no), 15, (no+mi), col= "grey")
rect(14, (no+mi), 15, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(15, 00, 16, no)
rect(15, (no), 16, (no+mi), col= "grey")
rect(15, (no+mi), 16, (no+mi+hi), col= "black")

text(10.5, 1.15, "Pre-HW2", cex = 0.8)
text(14.5, 1.15, "Post-HW2", cex = 0.8)

abline(v = 17, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

# end chap ####

# start gemi ####

dum<-subset(df, Species == "gemi")

plot(1 ~ 1, pch = NA, xlim = c(0,16), ylim = c(0,1.2), xaxt="n",yaxt="n", xlab="",ylab="")
axis(2, at = seq(0,2,0.25), labels = F)
axis(1, at = c(0.5, 1.5, 2.5, 4.5, 5.5, 6.5,
               9.5, 10.5, 11.5, 13.5, 14.5, 15.5), labels = F)
mtext(side = 3, expression(italic(Q.~gemi)), cex = 1.3)

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(00, 00, 01, no)
rect(00, (no), 01, (no+mi), col= "grey")
rect(00, (no+mi), 01, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(01, 00, 02, no)
rect(01, (no), 02, (no+mi), col= "grey")
rect(01, (no+mi), 02, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(02, 00, 03, no)
rect(02, (no), 03, (no+mi), col= "grey")
rect(02, (no+mi), 03, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(04, 00, 05, no)
rect(04, (no), 05, (no+mi), col= "grey")
rect(04, (no+mi), 05, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(05, 00, 06, no)
rect(05, (no), 06, (no+mi), col= "grey")
rect(05, (no+mi), 06, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(06, 00, 07, no)
rect(06, (no), 07, (no+mi), col= "grey")
rect(06, (no+mi), 07, (no+mi+hi), col= "black")

text(1.5, 1.15, "Pre-HW1", cex = 0.8)
text(5.5, 1.15, "Post-HW1", cex = 0.8)

abline(v = 8, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(09, 00, 10, no)
rect(09, (no), 10, (no+mi), col= "grey")
rect(09, (no+mi), 10, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(10, 00, 11, no)
rect(10, (no), 11, (no+mi), col= "grey")
rect(10, (no+mi), 11, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(11, 00, 12, no)
rect(11, (no), 12, (no+mi), col= "grey")
rect(11, (no+mi), 12, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(13, 00, 14, no)
rect(13, (no), 14, (no+mi), col= "grey")
rect(13, (no+mi), 14, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(14, 00, 15, no)
rect(14, (no), 15, (no+mi), col= "grey")
rect(14, (no+mi), 15, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(15, 00, 16, no)
rect(15, (no), 16, (no+mi), col= "grey")
rect(15, (no+mi), 16, (no+mi+hi), col= "black")

text(10.5, 1.15, "Pre-HW2", cex = 0.8)
text(14.5, 1.15, "Post-HW2", cex = 0.8)

abline(v = 17, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

# end gemi ####

# start phel ####

dum<-subset(df, Species == "phel")

plot(1 ~ 1, pch = NA, xlim = c(0,16), ylim = c(0,1.2), xaxt="n",yaxt="n", xlab="",ylab="")
axis(2, at = seq(0,2,0.25), cex.axis = 1.2, las = 2, labels = seq(0,200,25))
axis(1, at = c(0.5, 1.5, 2.5, 4.5, 5.5, 6.5,
               9.5, 10.5, 11.5, 13.5, 14.5, 15.5), labels = F)
mtext(side = 3, expression(italic(Q.~phel)), cex = 1.3)

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(00, 00, 01, no)
rect(00, (no), 01, (no+mi), col= "grey")
rect(00, (no+mi), 01, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(01, 00, 02, no)
rect(01, (no), 02, (no+mi), col= "grey")
rect(01, (no+mi), 02, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(02, 00, 03, no)
rect(02, (no), 03, (no+mi), col= "grey")
rect(02, (no+mi), 03, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(04, 00, 05, no)
rect(04, (no), 05, (no+mi), col= "grey")
rect(04, (no+mi), 05, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(05, 00, 06, no)
rect(05, (no), 06, (no+mi), col= "grey")
rect(05, (no+mi), 06, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(06, 00, 07, no)
rect(06, (no), 07, (no+mi), col= "grey")
rect(06, (no+mi), 07, (no+mi+hi), col= "black")

text(1.5, 1.15, "Pre-HW1", cex = 0.8)
text(5.5, 1.15, "Post-HW1", cex = 0.8)

abline(v = 8, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(09, 00, 10, no)
rect(09, (no), 10, (no+mi), col= "grey")
rect(09, (no+mi), 10, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(10, 00, 11, no)
rect(10, (no), 11, (no+mi), col= "grey")
rect(10, (no+mi), 11, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(11, 00, 12, no)
rect(11, (no), 12, (no+mi), col= "grey")
rect(11, (no+mi), 12, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(13, 00, 14, no)
rect(13, (no), 14, (no+mi), col= "grey")
rect(13, (no+mi), 14, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(14, 00, 15, no)
rect(14, (no), 15, (no+mi), col= "grey")
rect(14, (no+mi), 15, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(15, 00, 16, no)
rect(15, (no), 16, (no+mi), col= "grey")
rect(15, (no+mi), 16, (no+mi+hi), col= "black")

text(10.5, 1.15, "Pre-HW2", cex = 0.8)
text(14.5, 1.15, "Post-HW2", cex = 0.8)

abline(v = 17, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

# end phel ####

# start marg ####

dum<-subset(df, Species == "marg")

plot(1 ~ 1, pch = NA, xlim = c(0,16), ylim = c(0,1.2), xaxt="n",yaxt="n", xlab="",ylab="")
axis(2, at = seq(0,2,0.25), labels = F)
axis(1, at = c(0.5, 1.5, 2.5, 4.5, 5.5, 6.5,
               9.5, 10.5, 11.5, 13.5, 14.5, 15.5), labels = F)
mtext(side = 3, expression(italic(Q.~marg)), cex = 1.3)

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(00, 00, 01, no)
rect(00, (no), 01, (no+mi), col= "grey")
rect(00, (no+mi), 01, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(01, 00, 02, no)
rect(01, (no), 02, (no+mi), col= "grey")
rect(01, (no+mi), 02, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(02, 00, 03, no)
rect(02, (no), 03, (no+mi), col= "grey")
rect(02, (no+mi), 03, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(04, 00, 05, no)
rect(04, (no), 05, (no+mi), col= "grey")
rect(04, (no+mi), 05, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(05, 00, 06, no)
rect(05, (no), 06, (no+mi), col= "grey")
rect(05, (no+mi), 06, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(06, 00, 07, no)
rect(06, (no), 07, (no+mi), col= "grey")
rect(06, (no+mi), 07, (no+mi+hi), col= "black")

text(1.5, 1.15, "Pre-HW1", cex = 0.8)
text(5.5, 1.15, "Post-HW1", cex = 0.8)

abline(v = 8, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(09, 00, 10, no)
rect(09, (no), 10, (no+mi), col= "grey")
rect(09, (no+mi), 10, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(10, 00, 11, no)
rect(10, (no), 11, (no+mi), col= "grey")
rect(10, (no+mi), 11, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(11, 00, 12, no)
rect(11, (no), 12, (no+mi), col= "grey")
rect(11, (no+mi), 12, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(13, 00, 14, no)
rect(13, (no), 14, (no+mi), col= "grey")
rect(13, (no+mi), 14, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(14, 00, 15, no)
rect(14, (no), 15, (no+mi), col= "grey")
rect(14, (no+mi), 15, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(15, 00, 16, no)
rect(15, (no), 16, (no+mi), col= "grey")
rect(15, (no+mi), 16, (no+mi+hi), col= "black")

text(10.5, 1.15, "Pre-HW2", cex = 0.8)
text(14.5, 1.15, "Post-HW2", cex = 0.8)

abline(v = 17, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

# end marg ####

# start virg ####

dum<-subset(df, Species == "virg")

plot(1 ~ 1, pch = NA, xlim = c(0,16), ylim = c(0,1.2), xaxt="n",yaxt="n", xlab="",ylab="")
axis(2, at = seq(0,2,0.25), labels = F)
axis(1, at = c(0.5, 1.5, 2.5, 4.5, 5.5, 6.5,
               9.5, 10.5, 11.5, 13.5, 14.5, 15.5), labels = F)
mtext(side = 3, expression(italic(Q.~virg)), cex = 1.3)

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(00, 00, 01, no)
rect(00, (no), 01, (no+mi), col= "grey")
rect(00, (no+mi), 01, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(01, 00, 02, no)
rect(01, (no), 02, (no+mi), col= "grey")
rect(01, (no+mi), 02, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(02, 00, 03, no)
rect(02, (no), 03, (no+mi), col= "grey")
rect(02, (no+mi), 03, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(04, 00, 05, no)
rect(04, (no), 05, (no+mi), col= "grey")
rect(04, (no+mi), 05, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(05, 00, 06, no)
rect(05, (no), 06, (no+mi), col= "grey")
rect(05, (no+mi), 06, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(06, 00, 07, no)
rect(06, (no), 07, (no+mi), col= "grey")
rect(06, (no+mi), 07, (no+mi+hi), col= "black")

text(1.5, 1.15, "Pre-HW1", cex = 0.8)
text(5.5, 1.15, "Post-HW1", cex = 0.8)

abline(v = 8, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(09, 00, 10, no)
rect(09, (no), 10, (no+mi), col= "grey")
rect(09, (no+mi), 10, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(10, 00, 11, no)
rect(10, (no), 11, (no+mi), col= "grey")
rect(10, (no+mi), 11, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(11, 00, 12, no)
rect(11, (no), 12, (no+mi), col= "grey")
rect(11, (no+mi), 12, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(13, 00, 14, no)
rect(13, (no), 14, (no+mi), col= "grey")
rect(13, (no+mi), 14, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(14, 00, 15, no)
rect(14, (no), 15, (no+mi), col= "grey")
rect(14, (no+mi), 15, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(15, 00, 16, no)
rect(15, (no), 16, (no+mi), col= "grey")
rect(15, (no+mi), 16, (no+mi+hi), col= "black")

text(10.5, 1.15, "Pre-HW2", cex = 0.8)
text(14.5, 1.15, "Post-HW2", cex = 0.8)

abline(v = 17, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

# end virg ####

# start shum ####

dum<-subset(df, Species == "shum")

plot(1 ~ 1, pch = NA, xlim = c(0,16), ylim = c(0,1.2), xaxt="n",yaxt="n", xlab="",ylab="")
axis(2, at = seq(0,2,0.25), cex.axis = 1.2, las = 2, labels = seq(0,200,25))
axis(1, at = c(0.5, 1.5, 2.5, 4.5, 5.5, 6.5,
               9.5, 10.5, 11.5, 13.5, 14.5, 15.5), labels = F)
mtext(side = 3, expression(italic(Q.~shum)), cex = 1.3)

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(00, 00, 01, no)
rect(00, (no), 01, (no+mi), col= "grey")
rect(00, (no+mi), 01, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(01, 00, 02, no)
rect(01, (no), 02, (no+mi), col= "grey")
rect(01, (no+mi), 02, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(02, 00, 03, no)
rect(02, (no), 03, (no+mi), col= "grey")
rect(02, (no+mi), 03, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(04, 00, 05, no)
rect(04, (no), 05, (no+mi), col= "grey")
rect(04, (no+mi), 05, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(05, 00, 06, no)
rect(05, (no), 06, (no+mi), col= "grey")
rect(05, (no+mi), 06, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(06, 00, 07, no)
rect(06, (no), 07, (no+mi), col= "grey")
rect(06, (no+mi), 07, (no+mi+hi), col= "black")

text(1.5, 1.15, "Pre-HW1", cex = 0.8)
text(5.5, 1.15, "Post-HW1", cex = 0.8)

abline(v = 8, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(09, 00, 10, no)
rect(09, (no), 10, (no+mi), col= "grey")
rect(09, (no+mi), 10, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(10, 00, 11, no)
rect(10, (no), 11, (no+mi), col= "grey")
rect(10, (no+mi), 11, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(11, 00, 12, no)
rect(11, (no), 12, (no+mi), col= "grey")
rect(11, (no+mi), 12, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(13, 00, 14, no)
rect(13, (no), 14, (no+mi), col= "grey")
rect(13, (no+mi), 14, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(14, 00, 15, no)
rect(14, (no), 15, (no+mi), col= "grey")
rect(14, (no+mi), 15, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(15, 00, 16, no)
rect(15, (no), 16, (no+mi), col= "grey")
rect(15, (no+mi), 16, (no+mi+hi), col= "black")

text(10.5, 1.15, "Pre-HW2", cex = 0.8)
text(14.5, 1.15, "Post-HW2", cex = 0.8)

abline(v = 17, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

# end shum ####

# start stel ####

dum<-subset(df, Species == "stel")

plot(1 ~ 1, pch = NA, xlim = c(0,16), ylim = c(0,1.2), xaxt="n",yaxt="n", xlab="",ylab="")
axis(2, at = seq(0,2,0.25), labels = F)
axis(1, at = c(0.5, 1.5, 2.5, 4.5, 5.5, 6.5,
               9.5, 10.5, 11.5, 13.5, 14.5, 15.5), labels = F)
mtext(side = 3, expression(italic(Q.~stel)), cex = 1.3)

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(00, 00, 01, no)
rect(00, (no), 01, (no+mi), col= "grey")
rect(00, (no+mi), 01, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(01, 00, 02, no)
rect(01, (no), 02, (no+mi), col= "grey")
rect(01, (no+mi), 02, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(02, 00, 03, no)
rect(02, (no), 03, (no+mi), col= "grey")
rect(02, (no+mi), 03, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(04, 00, 05, no)
rect(04, (no), 05, (no+mi), col= "grey")
rect(04, (no+mi), 05, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(05, 00, 06, no)
rect(05, (no), 06, (no+mi), col= "grey")
rect(05, (no+mi), 06, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(06, 00, 07, no)
rect(06, (no), 07, (no+mi), col= "grey")
rect(06, (no+mi), 07, (no+mi+hi), col= "black")

text(1.5, 1.15, "Pre-HW1", cex = 0.8)
text(5.5, 1.15, "Post-HW1", cex = 0.8)

abline(v = 8, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(09, 00, 10, no)
rect(09, (no), 10, (no+mi), col= "grey")
rect(09, (no+mi), 10, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(10, 00, 11, no)
rect(10, (no), 11, (no+mi), col= "grey")
rect(10, (no+mi), 11, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(11, 00, 12, no)
rect(11, (no), 12, (no+mi), col= "grey")
rect(11, (no+mi), 12, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(13, 00, 14, no)
rect(13, (no), 14, (no+mi), col= "grey")
rect(13, (no+mi), 14, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(14, 00, 15, no)
rect(14, (no), 15, (no+mi), col= "grey")
rect(14, (no+mi), 15, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(15, 00, 16, no)
rect(15, (no), 16, (no+mi), col= "grey")
rect(15, (no+mi), 16, (no+mi+hi), col= "black")

text(10.5, 1.15, "Pre-HW2", cex = 0.8)
text(14.5, 1.15, "Post-HW2", cex = 0.8)

abline(v = 17, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

# end stel ####

# blank1 ####
plot(1 ~ 1, pch = NA, xaxt = "n", yaxt = "n")
# end blank1 ####

# start velu ####

dum<-subset(df, Species == "velu")

plot(1 ~ 1, pch = NA, xlim = c(0,16), ylim = c(0,1.2), xaxt="n",yaxt="n", xlab="",ylab="")
axis(2, at = seq(0,2,0.25), cex.axis = 1.2, las = 2, labels = seq(0,200,25))
axis(1, at = c(0.5, 1.5, 2.5, 4.5, 5.5, 6.5,
               9.5, 10.5, 11.5, 13.5, 14.5, 15.5), labels = F)
mtext(side = 3, expression(italic(Q.~velu)), cex = 1.3)

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(00, 00, 01, no)
rect(00, (no), 01, (no+mi), col= "grey")
rect(00, (no+mi), 01, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(01, 00, 02, no)
rect(01, (no), 02, (no+mi), col= "grey")
rect(01, (no+mi), 02, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(02, 00, 03, no)
rect(02, (no), 03, (no+mi), col= "grey")
rect(02, (no+mi), 03, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(04, 00, 05, no)
rect(04, (no), 05, (no+mi), col= "grey")
rect(04, (no+mi), 05, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(05, 00, 06, no)
rect(05, (no), 06, (no+mi), col= "grey")
rect(05, (no+mi), 06, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(06, 00, 07, no)
rect(06, (no), 07, (no+mi), col= "grey")
rect(06, (no+mi), 07, (no+mi+hi), col= "black")

text(1.5, 1.15, "Pre-HW1", cex = 0.8)
text(5.5, 1.15, "Post-HW1", cex = 0.8)

abline(v = 8, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(09, 00, 10, no)
rect(09, (no), 10, (no+mi), col= "grey")
rect(09, (no+mi), 10, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(10, 00, 11, no)
rect(10, (no), 11, (no+mi), col= "grey")
rect(10, (no+mi), 11, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(11, 00, 12, no)
rect(11, (no), 12, (no+mi), col= "grey")
rect(11, (no+mi), 12, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(13, 00, 14, no)
rect(13, (no), 14, (no+mi), col= "grey")
rect(13, (no+mi), 14, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(14, 00, 15, no)
rect(14, (no), 15, (no+mi), col= "grey")
rect(14, (no+mi), 15, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(15, 00, 16, no)
rect(15, (no), 16, (no+mi), col= "grey")
rect(15, (no+mi), 16, (no+mi+hi), col= "black")

text(10.5, 1.15, "Pre-HW2", cex = 0.8)
text(14.5, 1.15, "Post-HW2", cex = 0.8)

abline(v = 17, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

# end velu ####

# blank2 ####
plot(1 ~ 1, pch = NA, xaxt = "n", yaxt = "n")
# end blank2 ####

# blank3 ####
plot(1 ~ 1, pch = NA, xaxt = "n", yaxt = "n")
# end blank3 ####

# start lobatae ####

dum<-subset(df, section == "lobatae")

plot(1 ~ 1, pch = NA, xlim = c(0,16), ylim = c(0,1.2), xaxt="n",yaxt="n", xlab="",ylab="")
axis(2, at = seq(0,2,0.25), cex.axis = 1.2, las = 2, labels = seq(0,200,25))
axis(1, at = c(0.5, 1.5, 2.5, 4.5, 5.5, 6.5,
               9.5, 10.5, 11.5, 13.5, 14.5, 15.5),
     labels = c("Ambient","A+HW","W+HW", "Ambient", "A+HW", "W+HW",
                "Ambient","A+HW","W+HW", "Ambient", "A+HW", "W+HW"),
     las = 2)
mtext(side = 3, expression(Lobatae), cex = 1.3)

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(00, 00, 01, no)
rect(00, (no), 01, (no+mi), col= "grey")
rect(00, (no+mi), 01, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(01, 00, 02, no)
rect(01, (no), 02, (no+mi), col= "grey")
rect(01, (no+mi), 02, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(02, 00, 03, no)
rect(02, (no), 03, (no+mi), col= "grey")
rect(02, (no+mi), 03, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(04, 00, 05, no)
rect(04, (no), 05, (no+mi), col= "grey")
rect(04, (no+mi), 05, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(05, 00, 06, no)
rect(05, (no), 06, (no+mi), col= "grey")
rect(05, (no+mi), 06, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(06, 00, 07, no)
rect(06, (no), 07, (no+mi), col= "grey")
rect(06, (no+mi), 07, (no+mi+hi), col= "black")

text(1.5, 1.15, "Pre-HW1", cex = 0.8)
text(5.5, 1.15, "Post-HW1", cex = 0.8)

abline(v = 8, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(09, 00, 10, no)
rect(09, (no), 10, (no+mi), col= "grey")
rect(09, (no+mi), 10, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(10, 00, 11, no)
rect(10, (no), 11, (no+mi), col= "grey")
rect(10, (no+mi), 11, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(11, 00, 12, no)
rect(11, (no), 12, (no+mi), col= "grey")
rect(11, (no+mi), 12, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(13, 00, 14, no)
rect(13, (no), 14, (no+mi), col= "grey")
rect(13, (no+mi), 14, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(14, 00, 15, no)
rect(14, (no), 15, (no+mi), col= "grey")
rect(14, (no+mi), 15, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(15, 00, 16, no)
rect(15, (no), 16, (no+mi), col= "grey")
rect(15, (no+mi), 16, (no+mi+hi), col= "black")

text(10.5, 1.15, "Pre-HW2", cex = 0.8)
text(14.5, 1.15, "Post-HW2", cex = 0.8)

abline(v = 17, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

# end lobatae ####

# start quercus ####

dum<-subset(df, section == "quercus")

plot(1 ~ 1, pch = NA, xlim = c(0,16), ylim = c(0,1.2), xaxt="n",yaxt="n", xlab="",ylab="")
axis(2, at = seq(0,2,0.25), labels = F)
axis(1, at = c(0.5, 1.5, 2.5, 4.5, 5.5, 6.5,
               9.5, 10.5, 11.5, 13.5, 14.5, 15.5),
     labels = c("Ambient","A+HW","W+HW", "Ambient", "A+HW", "W+HW",
                "Ambient","A+HW","W+HW", "Ambient", "A+HW", "W+HW"),
     las = 2)
mtext(side = 3, expression(Quercus), cex = 1.3)

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(00, 00, 01, no)
rect(00, (no), 01, (no+mi), col= "grey")
rect(00, (no+mi), 01, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(01, 00, 02, no)
rect(01, (no), 02, (no+mi), col= "grey")
rect(01, (no+mi), 02, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(02, 00, 03, no)
rect(02, (no), 03, (no+mi), col= "grey")
rect(02, (no+mi), 03, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(04, 00, 05, no)
rect(04, (no), 05, (no+mi), col= "grey")
rect(04, (no+mi), 05, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(05, 00, 06, no)
rect(05, (no), 06, (no+mi), col= "grey")
rect(05, (no+mi), 06, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(06, 00, 07, no)
rect(06, (no), 07, (no+mi), col= "grey")
rect(06, (no+mi), 07, (no+mi+hi), col= "black")

text(1.5, 1.15, "Pre-HW1", cex = 0.8)
text(5.5, 1.15, "Post-HW1", cex = 0.8)

abline(v = 8, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(09, 00, 10, no)
rect(09, (no), 10, (no+mi), col= "grey")
rect(09, (no+mi), 10, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(10, 00, 11, no)
rect(10, (no), 11, (no+mi), col= "grey")
rect(10, (no+mi), 11, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(11, 00, 12, no)
rect(11, (no), 12, (no+mi), col= "grey")
rect(11, (no+mi), 12, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(13, 00, 14, no)
rect(13, (no), 14, (no+mi), col= "grey")
rect(13, (no+mi), 14, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(14, 00, 15, no)
rect(14, (no), 15, (no+mi), col= "grey")
rect(14, (no+mi), 15, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(15, 00, 16, no)
rect(15, (no), 16, (no+mi), col= "grey")
rect(15, (no+mi), 16, (no+mi+hi), col= "black")

text(10.5, 1.15, "Pre-HW2", cex = 0.8)
text(14.5, 1.15, "Post-HW2", cex = 0.8)

abline(v = 17, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

# end quercus ####

# start virentes ####

dum<-subset(df, section == "virentes")

plot(1 ~ 1, pch = NA, xlim = c(0,16), ylim = c(0,1.2), xaxt="n",yaxt="n", xlab="",ylab="")
axis(2, at = seq(0,2,0.25), labels = F)
axis(1, at = c(0.5, 1.5, 2.5, 4.5, 5.5, 6.5,
               9.5, 10.5, 11.5, 13.5, 14.5, 15.5),
     labels = c("Ambient","A+HW","W+HW", "Ambient", "A+HW", "W+HW",
                "Ambient","A+HW","W+HW", "Ambient", "A+HW", "W+HW"),
     las = 2)
mtext(side = 3, expression(Virentes), cex = 1.3)

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(00, 00, 01, no)
rect(00, (no), 01, (no+mi), col= "grey")
rect(00, (no+mi), 01, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(01, 00, 02, no)
rect(01, (no), 02, (no+mi), col= "grey")
rect(01, (no+mi), 02, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/19/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(02, 00, 03, no)
rect(02, (no), 03, (no+mi), col= "grey")
rect(02, (no+mi), 03, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(04, 00, 05, no)
rect(04, (no), 05, (no+mi), col= "grey")
rect(04, (no+mi), 05, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(05, 00, 06, no)
rect(05, (no), 06, (no+mi), col= "grey")
rect(05, (no+mi), 06, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "5/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(06, 00, 07, no)
rect(06, (no), 07, (no+mi), col= "grey")
rect(06, (no+mi), 07, (no+mi+hi), col= "black")

text(1.5, 1.15, "Pre-HW1", cex = 0.8)
text(5.5, 1.15, "Post-HW1", cex = 0.8)

abline(v = 8, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(09, 00, 10, no)
rect(09, (no), 10, (no+mi), col= "grey")
rect(09, (no+mi), 10, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(10, 00, 11, no)
rect(10, (no), 11, (no+mi), col= "grey")
rect(10, (no+mi), 11, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "6/29/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(11, 00, 12, no)
rect(11, (no), 12, (no+mi), col= "grey")
rect(11, (no+mi), 12, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Control")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(13, 00, 14, no)
rect(13, (no), 14, (no+mi), col= "grey")
rect(13, (no+mi), 14, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Heatwave")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(14, 00, 15, no)
rect(14, (no), 15, (no+mi), col= "grey")
rect(14, (no+mi), 15, (no+mi+hi), col= "black")

dat<-subset(dum, Date == "7/4/2020" & Treatment == "Warm+Heat")
dat<-dat[!is.na(dat$Injury_0to2),]; count<-nrow(dat)
no<-subset(dat, Injury_0to2 == "0"); no<-nrow(no)
mi<-subset(dat, Injury_0to2 == "1"); mi<-nrow(mi)
hi<-subset(dat, Injury_0to2 == "2"); hi<-nrow(hi)
no<-no/count; mi<-mi/count; hi<-hi/count
rect(15, 00, 16, no)
rect(15, (no), 16, (no+mi), col= "grey")
rect(15, (no+mi), 16, (no+mi+hi), col= "black")

text(10.5, 1.15, "Pre-HW2", cex = 0.8)
text(14.5, 1.15, "Post-HW2", cex = 0.8)

abline(v = 17, lty = 3) #~~~~~~~~~~~~~~~~~~~~~~~~~~~

# end virentes ####

# turn-off ####
mtext(side = 1, "Treatment", outer = T, cex = 1.5, padj = 3)
mtext(side = 2, "Percent of Individuals with Injury (%)", outer = T, cex = 1.5, padj = -2)

dev.off()
1+1
