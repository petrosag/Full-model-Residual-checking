## settig up the dat frame
Chemical <- read.csv(file.choose(), header = TRUE)
Chemical

Day <- factor(Day)
Batch <- factor(Batch)
means <- tapply(Time, Day, mean)
means
Vars <- tapply(Time, Day, var)
Vars
Sdv <- tapply(Time, Day, sd)
Sdv
mean <- tapply(Time, Batch, mean)
## Building Linear Model
ingre.aov <- aov(Time ~ Day+Batch)
summary.aov(ingre.aov)

## the ingredients affect reaction time
model.tables(ingre.aov, type="effects")
## Day1 against Batch1 has a significant different of -0.68 and batch 0.72
model.tables(ingre.aov, type = "means")
tukey <- TukeyHSD(ingre.aov)
plot(tukey)

## check for interaction
interaction.plot(Day, Batch, Time)
res <- residuals(ingre.aov)
qqnorm(res)
qqline(res)

## No. 2 Engineering

Engineering <- read.csv(file.choose(), header = TRUE)
Engineering

Operator <- factor(Operator)
Assembly <- factor(Assembly)
tapply(time, Operator, mean)
tapply(time, Operator, var)
tapply(time, Operator, sd)

## linear model, using aov()
engineering.aov <- aov(time ~ Operator+Assembly)
summary(engineering.aov)

## the effects taken for the operator, assembly time  
model.tables(engineering.aov, type = "effects")
model.tables(engineering.aov,, type = "means")

## check for interaction
interaction.plot(Operator, Assembly, time)

tuk <- TukeyHSD(engineering.aov)
plot(tuk)


## Residual check:

residu <- residuals(engineering.aov)
residu
qqnorm(residu)
qqline(residu)
plot(residu,time)
plot(residu, Operator)
plot(residu,time)

plot(time~Assembly)
plot(time~ Operator)