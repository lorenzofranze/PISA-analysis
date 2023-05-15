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
  plot(df[,j], df[,9])
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

# from diagnostic I can't see any pattern: I don't know what relationship has 
# been left out, idk how to improve the R^2