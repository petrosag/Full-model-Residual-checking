## Adewuya Gbemisola
## Lab 7
## Problem 1. Effects of Cutting Speed

Speed <- read.csv(file.choose(), header = T)
head(Speed)
block <- rep(1:3, times = 8)
block
data <- data.frame(Speed, block)


attach(data)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
data$Cutting.Speed <- factor(data$Cutting.Speed)
data$Tool.Geometry <- factor(data$Tool.Geometry)
data$Cutting.Angle <- factor(data$Cutting.Angle)
data$block <- factor(data$block)
plot.design(data)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
head(data)
## build linear model
data.lm <- lm(Life.Hours ~ Cutting.Speed*Tool.Geometry*Cutting.Angle + block)
anova(data.lm)
data.aov <- aov(Life.Hours ~ Cutting.Speed*Tool.Geometry*Cutting.Angle + Error(block) )
summary(data.aov)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

#residual plots

plot(fitted(data.lm),residuals(data.lm))
qqnorm(residuals(data.lm))
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

library(FrF2)
#one - half fractional designs
#2^8-5 fractional factorial design
des.x <- FrF2(8, 5, res3 = T, factor.names = c('Cutting.Speed', 'Tool.Geometry', 'Cutting.Angle', 'Life.Hours', 'block'), randomize = F)
summary(des.x) 
design.info(des.x)$catlg.entry


#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## plotting : normal plot of effects




#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

## Problem 2 Cutting locating notches
Nothches <- read.csv(file.choose(), header = T)
head(Nothches)
Bl <- rep(1:4, times = 4)
Bl

notches.data <- data.frame(Nothches, Bl)
attach(notches.data)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
notches.data 
#Bit.Size Cutting.Speed Vibration Bl
notches.data$Bit.Size <- factor(notches.data$Bit.Size)
notches.data$Cutting.Speed <- factor(notches.data$Cutting.Speed)
notches.data$Bl <- factor(notches.data$Bl)
plot.design(notches.data)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

## build linear model
notches.data.lm <- lm(Vibration ~ Bit.Size*Cutting.Speed+Bl)
anova(notches.data.lm)
notches.aov <- aov(Vibration ~ Bit.Size*Cutting.Speed+Error(Bl))
summary(notches.aov)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

plot(fitted(notches.data.lm), residuals(notches.data.lm))
qqnorm(residuals(notches.data.lm))
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##one - half fractional designs
des.n <- FrF2(8, 4, res3 = T, factor.names = c('Vibration','Bit.Size', 'Cutting.Speed', 'Bl'), randomize = F)
summary(des.n)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

## Problem 3 Create a design of 16 runs
gen.des <- FrF2(16, 8, generators = c('ABC','ABD', 'ACD', 'BCD'), randomize = F)
y1 <- c(5.75, 6.7, 11.2, 10.67, 4.92, 5.35, 2.81, 10.83, 6.08, 7.27, 9.68, 4.2, 3.9, 3.78, 11.57, 7.39)
## to add the response of y1
gen.data <- add.response(gen.des, y1)
gen.data

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#build the linear model
gen.data.aov <- aov(y1~ (.)^2, data = gen.data)
summary(gen.data.aov)
library(daewr)
gen.lm <- lm(y1~ A*B*C*D, data = gen.data)
LGB(coef(gen.lm)[-1], rpt = FALSE)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#3-way interaction plot
MEPlot(gen.data, abbrev = 5, cex.xax = 1.6, cex.main = 2)
IAPlot(gen.data, abbrev = 5, show.alias = TRUE, lwd = 2, cex = 2, cex.xax = 1.2, cex.lab = 1.5)
summary(lm(gen.data))
IAPlot(gen.data.aov , select=c(1,2,3,4)) 
IAPlot(gen.data.aov , select=c(5,6,7,8))
DanielPlot(gen.data.aov)
library(BsMD)
LenthPlot(gen.data.aov)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
generators(gen.des)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## Problem 4 one half fraction

Treatment <- read.csv(file.choose(), header = T)

tbl <- rep(1:2, times = 16)
tbl
trtdata <- data.frame(Treatment, tbl)
attach(trtdata)
head(trtdata)

#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
trtdata.aov<-aov(Crack.Length~(.)^2,data=Treatment)  
summary(trtdata.aov)
library(FrF2)
destrt <- FrF2(16, 4)
summary(destrt)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
trt.lm <- lm(Crack.Length ~(.)^2, data=Treatment)
summary(trt.lm)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
#Normal plot of effect
runs <- 2^(8-4)
expt <- c("A", "B","C","D")
expt.des <- FrF2(runs, factor.names = expt, default.levels = c("0", "1"))
summary(expt.des)

#Pour.Temperature, Titanium.Content, Heat.Treat.Method, Grain.Refiner
#with acuall names

expt2 <- c("Pour.Temperature", "Titanium.Content","Heat.Treat.Method","Grain.Refiner")
expt.des2 <- FrF2(runs, factor.names = expt2, default.levels = c("0", "1"))
summary(expt.des2)

exper.trt <- data.frame(expt.des2)
aliasprint(expt.des2)
#<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
par(c(2, 2))
plot(fitted(trt.lm), residuals(trt.lm))
qqnorm(residuals(trt.lm))
LenthPlot(trtdata.aov)
