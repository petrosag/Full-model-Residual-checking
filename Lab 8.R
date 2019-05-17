#Gbemisola Adewuya
#Lab 8: STAT 5309
#Problem:1 Tensile Strength

Strength <- c(160, 171, 175, 182, 184, 181,188,193, 195, 200)
Percent.H <- c(10, 15, 15, 20, 20, 20,25,25,28,30)
Tensil <- data.frame( Percent.H,Strength)
Tensil
Percent.H <- factor(Percent.H)
attach(Tensil)
#a.) multiple linear regression model: using strength as response
tensil.mod <-lm(Strength ~ Percent.H)
summary(tensil.mod)
tensil.aov <- aov(Strength ~ Percent.H)
summary(tensil.aov)

#A p-value of less than 0.05 is considered to be statistically significant
#-------------------------------------------------------------------------
#b.) the model matrix X: calculate coefficients vector, using beta=(X'X)^-1Xy
summary(matmod <- lm(Strength ~ Percent.H, data = Tensil))
model.matrix(matmod)
model.matrix(~Percent.H)
model.matrix(formula(~ Percent.H))
coef(matmod)
#y-hat <- ( 143.824385 + 1.878635xi )
#-------------------------------------------------------------------------
#c. the residuals, e = (I-H)y; H=X'(X'X)^-1Xy
# Yi = Beta0 + Beta1*xi + Ei
# Ei ~ N(0, sd^2)
#xi = Strength
#xi2 = Percent.H
#(xi1, xi2, yi)

#Fitted model
Betahat <- 143.824385
Beta1hat <- 1.878635
summary(matmod)$sigma
Id <- diag(10)
Id
#-------------------------------------------------------------------------
#d.) 95% - CI for B1 percent hardwood

confint(matmod, level = .95)

#-------------------------------------------------------------------------
#-------------------------------------------------------------------------

#Problem 2 Oxygen by liquidifying air

Oxygen <- read.csv(file.choose(), header = T)
head(Oxygen)
str(Oxygen)
Pressure <- factor(Pressure)
Temperature <- factor(Temperature)
attach(Oxygen)
#-------------------------------------------------------------------------
# First order RSM/FO RSM
oxygen.mod <- lm(Purity ~ Pressure+Temperature, data = Oxygen)
oxygen.mod

oxygen.aov <- aov(Purity ~ Pressure*Temperature)
oxygen.aov
fitted <- oxygen.aov$fitted.values
max(fitted)
min(fitted)

Oxygen.rsm <- rsm(Purity ~ FO(Pressure, Temperature)+ TWI(Pressure, Temperature), data = Oxygen)
oxysummary <- summary(Oxygen.rsm)
oxysummary



#-------------------------------------------------------------------------
#stationary points: canonical

canonical(Oxygen.rsm)

#contour
liquidify <- canonical(Oxygen.rsm)$xs
liquidify
contour(Oxygen.rsm ~ Pressure+Temperature, image=TRUE, at=liquidify )
#-------------------------------------------------------------------------
#path of steepest ascent

steep <- steepest (Oxygen.rsm, dist = seq(0, 5, by = .5), descent = FALSE)
plot(steep)
#-------------------------------------------------------------------------

class(Pressure)
press <- c((Pressure -1.2)/.1)
press
temp <- c((Temperature-(-220))/5)
temp
purity.orth <- data.frame(press, temp, Purity)


#-------------------------------------------------------------------------
#-------------------------------------------------------------------------

#Problem 3 Crystal Growth
library(rsm)
condition <- read.csv(file.choose(), header = T)

condition
str(condition)
attach(condition)
x1 <- factor(x1)
x2 <- factor(x2)
x3 <- factor(x3)

#-------------------------------------------------------------------------
#a - b. First order RSM/SO RSM
crystal.rsm <- rsm(Yield ~ SO(x1,x2,x3), data = condition)
summary(crystal.rsm)

#-------------------------------------------------------------------------
#c. stationary points: canonical

canonical(crystal.rsm)
crystal <- canonical(crystal.rsm)$xs
crystal

contour(crystal.rsm, ~x1+x2+x3, image = TRUE, at=crystal )

#-------------------------------------------------------------------------
#d. Viscosity: canonical
contour(crystal.rsm, ~x1+x2+x3, image = TRUE, at=crystal )





#-------------------------------------------------------------------------
#-------------------------------------------------------------------------

#Problem 4 reactant and the operating temperature

Yield <- c(81,89,83,91,79,87,84,90)
Concentration <- rep(seq(1.00, 2.00), each = 2,times=2)
Temperature <- c(150,180,150,180,150,180,150,180)
Reactant <- data.frame(Concentration, Temperature, Yield)
attach(Reactant)
Reactant
#--------------------------------------------------------------------
#main effects model: X'X matrix

mod.react <- lm(Yield ~ ., data=Reactant)
model.matrix(mod.react)
mod.aov <- aov(Yield ~ ., data = Reactant)
model.tables(mod.aov, type = "effects")


#b. X'X------------------
summary(mod.react)$sigma
coef(mod.react)

#c. checking for data diagonal
conc <- (Concentration-1.5)/.5
temp <- (Temperature-165)/15
data <- data.frame(conc, temp, Yield)
react.mod2 <- lm(Yield ~., data = data)
X <- model.matrix(react.mod2)
xtx <- t(X)%*% X
xtx
#Diagonal = Octagonal = TRUE

# d. X'X----------------------new set of variables
conc2 <- (Concentration-1.0)/1.0
temp2 <- (Temperature-150)/30
data2 <- data.frame(conc2,temp2, Yield)
react.mod3 <- lm(Yield ~., data = data2)
X2 <- model.matrix(react.mod3)
xtx2 <- t(X2)%*% X2
xtx2
#dii.
#Diagonal = FALSE

