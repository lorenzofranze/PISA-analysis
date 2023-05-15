df <- get(load("new_features_targets.RData"))
head(df)
n <- dim(df)[1]
p <- dim(df)[2]

# jitter
for(j in 1:p)
  df[,j] <- df[,j] +  + cbind(rnorm(n, sd=0.025))

#################################### PCA ################################
effects <- df[,1:7]
effects.sd <- scale(effects)
effects.sd <- data.frame(effects.sd)
pc <- princomp(effects.sd, scores=T)
summary(pc)

# explained variance (proportion blablabla...)
x11()
layout(matrix(c(2,3,1,3), 2, byrow=T))
barplot(pc$sdev^2, las=2, main='Principal Components', ylim=c(0,4), ylab='Variances')
abline(h=1, col='blue')
barplot(sapply(effects.sd,sd)^2, las=2, main='Original Variables', ylim=c(0,4), ylab='Variances')
plot(cumsum(pc$sdev^2)/sum(pc$sde^2), type='b', axes=F, xlab='number of components',
     ylab='contribution to the total variance', ylim=c(0,1))
box()
axis(2, at=0:10/10, labels=0:10/10)
axis(1, at=1:ncol(effects.sd), labels=1:ncol(effects.sd), las=2)

scores_red <- pc$scores[,1:3]
loadings_red <- pc$loadings[,1:3]

# visualize loadings of first 3 PCs
x11()
par(mfrow=c(3,1))
for(i in 1:3) barplot(pc$loadings[,i], ylim=c(-1,1), main=paste('loadings on PC',i))


#################################### MANOVA ##############################
# group based on performance
qa <- quantile(df$PV1MATH, 1/3)
qb <- quantile(df$PV1MATH, 2/3)

performance_levels <- NULL
for (i in 1:n)
{
  if(df$PV1MATH[i] <= qa)
    performance_levels[i] = 'l'
  else if (df$PV1MATH[i] > qa & df$PV1MATH[i] <= qb)
    performance_levels[i] = 'm'
  else
    performance_levels[i] = 'h'
}

performance_levels <- factor(performance_levels)
df["factor_PV1MATH"] <- performance_levels

effects <- df[,1:7]
fit <- manova(as.matrix(effects) ~ df$factor_PV1MATH)
summary.manova(fit)

# divide populations
dfl = df[df$factor_PV1MATH=='l',1:7]
dfm = df[df$factor_PV1MATH=='m',1:7]
dfh = df[df$factor_PV1MATH=='h',1:7]

###################### VERIFY HYPOTHESIS ########################

### univariate normality
p <- 7

x11()
par(mfrow=c(2,4))
for(j in 1:p)
  plot(density(dfl[,j]))
#