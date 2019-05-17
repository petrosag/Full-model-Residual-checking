## Lab 6
## Problem 1
# reading the data.
Router <- read.csv(file.choose(), header = T)
head(Router)
## "Bit.Size"      "Cutting.Speed" "Vibration" 

attach(Router)
names(Router)
##a. Analyzisng the experiment

router.mod <- aov(Vibration ~ Cutting.Speed*Bit.Size)# check interaction
router.mod1 <- aov(Vibration ~ Cutting.Speed+Bit.Size)
model.tables(router.mod)
model.tables(router.mod1)


#there is no interaction

summary(router.mod)
summary(router.mod1)

##linear model
router.lm = lm(Vibration ~ Cutting.Speed*Bit.Size, data = Router)
router.stdres <- rstandard(router.lm)

library(FrF2)
#library(lsmeans)
#router.mod2 <- lm(Vibration ~ Cutting.Speed*Bit.Size, data = Router, contrasts = list(Cutting.Speed=contr.FrF2, Bit.Size=contr.FrF2))
#summary(router.mod2)

## normal probability plot
qqnorm(router.stdres, 
       ylab = "Standard Residuals",
       xlab = "Vibration Level",
       main = "Router")
qqline(router.stdres)

DanielPlot(router.mod)
fullnormal(coef(router.mod)[-1])
anova(router.lm)
##c.) interaction plot
IAPlot(router.mod, select = c(1,2))

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

# Problem 2.
##Elimination of factor effects

# reading data:

life <- read.csv(file.choose(), header = T)
head(life)
attach(life)
names(life)
life.mod <-aov(Life.Hours ~ Cutting.Speed*Tool.Geometry+Cutting.Angle) #
life.mod1<-aov(Life.Hours ~ Cutting.Speed*Tool.Geometry*Cutting.Angle) #check interaction
life.mod2 <- aov(Life.Hours ~ Tool.Geometry*Cutting.Angle)
model.tables(life.mod)


## a) estimate for the factor effects
summary.aov(life.mod, type ="effects")
summary(life.mod1)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

##b) analysis of variance
anova(life.mod1, life.mod2)# compare to values: 
# H0 : M1 = M2
# HA : M1 != M2
model.matrix(life.mod)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

##c) regression model for predicting tool life
life.lm <- lm(Life.Hours ~ Cutting.Speed*Tool.Geometry*Cutting.Angle)
life.lm1 <- lm(Life.Hours ~ Cutting.Speed*Tool.Geometry+Cutting.Angle)
summary(life.lm)
summary(life.lm1)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
## d) analyse the residuals
#check <- life$residual


##e) analysis of main effect and interaction plots
## plots
DanielPlot(life.mod)
fullnormal(coef(life.mod)[-1])
MEPlot(life.mod)
##interaction plot
IAPlot(life.mod, select = c(1,2,3))



##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<



#problem 3 : Yield of chemical process

Process <- read.csv(file.choose(),header = T)
head(Process)
attach(Process)
names(Process)

## a) estimate for the factor effects
process.aov <- aov(Yield ~ A*B*C*D, data = Process)
summary.aov(process.aov, type ="effects")
model.tables(process.aov)
##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

##b) analysis of variance
process.mod <- aov(Yield ~ A*B*C*D)
process.mod1 <- aov(Yield ~ A*B*C+D)
anova(process.mod, process.mod1)

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<

##c)  regression model for predicting

process.lm <- lm(Yield ~ A*B*C*D, data = Process)

process.lm1 <- lm(Yield ~ A*B*C+D, data = Process)
summary(process.lm)
summary(process.lm1)

##<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
##d) the residuals versus the predicted yeild and a normal probability
processresiduals <- process.mod$residuals
qqnorm(processresiduals)
qqline(processresiduals)
processfitted <- process.mod1$fitted.values
plot(processfitted, processresiduals)
abline(h=0)
## normal probability plot
DanielPlot(process.mod1)
fullnormal(coef(process.mod1)[-1])
MEPlot(process.mod1)
process.stdres <- rstandard(process.lm1)
qqnorm(process.stdres, 
       ylab = "Standard Residuals",
       xlab = "Predicted Yield",
       main = "Process")
qqline(process.stdres)


##e) three factors of interaction
##interaction plot
IAPlot(process.mod1, select = c(1,2,3))
IAPlot(process.mod1, select = c(1,3,4))
