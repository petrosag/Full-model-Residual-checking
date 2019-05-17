## Problem 1 Chemical Process:

#Extractig table from Excel:

yield <- read.csv(file.choose(), header = TRUE)
yield
length(Pressure)
attach(yield)
Pressure <- factor(Pressure)
Temperature <- factor(Temperature)
tapply(Yield,list(Pressure, Temperature),mean)

means <-tapply(Yield,list(Pressure, Temperature),mean)
sdv <- tapply(Yield,list(Pressure, Temperature),sd)
vars <- tapply(Yield,list(Pressure, Temperature),var)


## b. Building Linear Model
ydata.aov <- aov(Yield ~ Temperature+Pressure)
summary.aov(ydata.aov)
ydata.aov <- aov(Yield ~ Temperature*Pressure, data=yield )
summary.aov(ydata.aov)
## at degree of freedom 13 the residuals have both positive mean square.

tukey <- TukeyHSD(ydata.aov)
tukey
plot(tukey)

## there is a significant different, the mean levels of pressure went below 0 to negative at 230 -215
## the temp mean isnt significant
## Check for interaction; if it is significant

interaction(Pressure, Temperature)
interaction.plot(Pressure,Temperature, Yield)
## The interation occured between the temperature of 15 to 17 which yields at about 90.37 


## c.

plot(Yield~ Temperature)
plot(Yield ~ Pressure)
plot(Yield~Temperature+Pressure)

## The lowest yield of (90.0 - 90.2) occurred at the temperature 180
## Compared to pressure the lowest yield at pressure(230) yield(90.0 - 90.2)

## d.Residual Check
residualz <- residuals(ydata.aov)
qqnorm(residualz)
qqline(residualz)
plot(residualz~Pressure)
plot(residualz~Temperature)

model.tables(ydata.aov, type = "mean")
model.tables(ydata.aov, type = "effects")





## Problem 2 Investigate Warping

copperplate <- read.csv(file.choose(), header = TRUE)
copperplate
attach(copperplate)
length(Warping)
Content <- factor(Content)
Temp <- factor(Temp)

tapply(Warping, list(Content, Temp),mean )
tapply(Warping, list(Content, Temp),sd )
tapply(Warping, list(Content, Temp),var )

## a.. Building Linear Model

warp.aov <- aov(Warping~Content+Temp)
warp.aov <- aov(Warping~Content*Temp, data = copperplate)
summary.aov(warp.aov)

# residual at df 25 has a mean sq of 8.89

## check for interaction
interaction.plot(Content,Temp,Warping)
interaction.plot(Content, Temp, Warping, pch=19)

## b. Build a RSM with warpage as response
library(rsm)

warpage <- rsm(Warping ~ FO(Content, Temp), data = copperplate)
summary(warpage)

## c.plots of 

par(mfrow=c(2,2))
# warpage vs Temp
# warpage vs Content
# warpage vs Temp and Content
plot(warpage)

# these plots didnt work i dont know why 
plot(warpage~ Temp)
plot(warpage~ Content)
plot(warpage~ Temp+Content)