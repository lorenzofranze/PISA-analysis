# libraries
library(MVN) 
library(mvnormtest)
library(ggplot2)
library(GGally)

# load data
df <- get(load("new_features_targets.RData"))
head(df)
n <- dim(df)[1]
p <- dim(df)[2]

# jitter
for(j in 1:p)
  df[,j] <- df[,j] +  + cbind(rnorm(n, sd=0.025))

### EDA

## PVMATH
x11()
par(mfrow=c(2,4))
for(j in 1:7)
  plot(df[,j], df[,8])
# not encouraging
## PVREAD
x11()
par(mfrow=c(2,4))
for(j in 1:7)
  plot(df[,j], df[,9], sample=30)
# still not encouraging

### try to fit linear model (target: PVMATH)
mod1 <- lm(df$PV1MATH ~ df$MASTGOAL + df$RESILIENCE + df$WORKMAST +
           df$EMOSUPS + df$PERCOOP + df$PERCOMP + df$COMPETE)
summary(mod1)

x11()
par(mfcol=c(2,2))
plot(mod1)

#################### variable selection (manual)

# - resilience
mod2 <- lm(df$PV1MATH ~ df$MASTGOAL + df$WORKMAST + df$EMOSUPS +
          df$PERCOOP + df$PERCOMP + df$COMPETE)
summary(mod2)

# - mastgoal
mod3 <- lm(df$PV1MATH ~ df$WORKMAST + df$EMOSUPS + df$PERCOOP + df$PERCOMP + df$COMPETE)
summary(mod3)

# - percomp
mod4 <- lm(df$PV1MATH ~ df$WORKMAST + df$EMOSUPS + df$PERCOOP + df$COMPETE)
summary(mod4)

x11()
par(mfcol=c(2,2))
plot(mod4)

# From diagnostic I can't see any pattern: I don't know what relationship has 
# been left out, idk how to improve the R^2

# Try to build a linear using PCAs
effects <- df[,1:7]
effects.sd <- scale(effects)
effects.sd <- data.frame(effects.sd)

shapiro.test(sample(df$PV1MATH, 2000))
# Histograms fo the target
x11()
par(mfcol=c(1,2))
hist(df$PV1MATH, prob=T)
xx <- seq(min(df$PV1MATH), max(df$PV1MATH))
lines(xx, dnorm(xx,mean(df$PV1MATH), sd(df$PV1MATH)), col='blue', lty=2)
qqnorm(df$PV1MATH)
qqline(df$PV1MATH)
# I assume the target to be normal.
pc <- princomp(effects.sd, scores=T)
summary(pc)
# I select the first three PCs since are the most interpretable, explaining the
# 62% of the total variance.
scores_red <- pc$scores[,1:3]
# Build the dataset of PCs and the target variable PV1MATH
df_3PCA <- data.frame(scores_red, df$PV1MATH)
df_3PCA
# Build the first linear model with the new PCs' dataset
pca_mod1 <- lm(df_3PCA$df.PV1MATH ~ df_3PCA$Comp.1 + df_3PCA$Comp.2 + df_3PCA$Comp.3)
summary(pca_mod1) # R squared is 0.017

# Since the R squared is to low, i try to build a linear model using all PCs.
df_PCA <- data.frame(pc$scores, df$PV1MATH)
pca_mod2 <- lm(df_PCA$df.PV1MATH ~ df_PCA$Comp.1 + df_PCA$Comp.2 + df_PCA$Comp.3
               + df_PCA$Comp.4 + df_PCA$Comp.5 + df_PCA$Comp.6)
summary(pca_mod2) # R squared is 0.0248
# I remove the insignificant components:
pca_mod3 <- lm(df_PCA$df.PV1MATH ~ df_PCA$Comp.1 + df_PCA$Comp.2 + df_PCA$Comp.4
               + df_PCA$Comp.5)
summary(pca_mod3) # R squared is 0.0241
# FAILED
