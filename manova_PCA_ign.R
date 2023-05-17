library(MVN)
library(car)
library(foreign)
library(bestNormalize)
library("ggplot2")                     
library("GGally")

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

### create new dataframe: features=scores from PCA + target (PVMATH1 for now)
df_PCA <- data.frame(scores_red, df$PV1MATH)
head(df_PCA)
p <- 3
colnames(df_PCA) <- c("PC1","PC2","PC3","PV1MATH")
  
#################################### MANOVA (on scores) ##############################
# group based on performance
qa <- quantile(df_PCA$PV1MATH, 1/3)
qb <- quantile(df_PCA$PV1MATH, 2/3)

performance_levels <- NULL
for (i in 1:n)
{
  if(df_PCA$PV1MATH[i] <= qa)
    performance_levels[i] = 'l'
  else if (df_PCA$PV1MATH[i] > qa & df_PCA$PV1MATH[i] <= qb)
    performance_levels[i] = 'm'
  else
    performance_levels[i] = 'h'
}

performance_levels <- factor(performance_levels)
df_PCA["factor_PV1MATH"] <- performance_levels

effects <- df_PCA[,1:3]
fit <- manova(as.matrix(effects) ~ df_PCA$factor_PV1MATH)
summary.manova(fit)

summary.aov(fit)

# divide populations
dfl = df_PCA[df_PCA$factor_PV1MATH=='l',1:3]
dfm = df_PCA[df_PCA$factor_PV1MATH=='m',1:3]
dfh = df_PCA[df_PCA$factor_PV1MATH=='h',1:3]

### inference
# Via Bonferroni
g <- 3
alpha <- 0.05
k <- p*g*(g-1)/2
qT <- qt(1-alpha/(2*k), n-g)
W <- summary.manova(fit)$SS$Residuals  # within variability

m  <- sapply(df_PCA, mean)         # estimates mu
m1 <- sapply(dfl, mean)    # estimates mu.1 = mu + tau.1
m2 <- sapply(dfm, mean)    # estimates mu.2 = mu + tau.2
m3 <- sapply(dfh, mean)    # estimates mu.3 = mu + tau.3

# for all components at once
inf12 <- m1 - m2 - qT * sqrt(diag(W)/(n-g) * (1/n1+1/n2))
sup12 <- m1 - m2 + qT * sqrt(diag(W)/(n-g) * (1/n1+1/n2))
inf13 <- m1 - m3 - qT * sqrt(diag(W)/(n-g) * (1/n1+1/n3))
sup13 <- m1 - m3 + qT * sqrt(diag(W)/(n-g) * (1/n1+1/n3))
inf23 <- m2 - m3 - qT * sqrt(diag(W)/(n-g) * (1/n2+1/n3))
sup23 <- m2 - m3 + qT * sqrt(diag(W)/(n-g) * (1/n2+1/n3))

CI <- list(setosa_versicolor    = cbind(inf12, sup12),
           setosa_virginica     = cbind(inf13, sup13),
           versicolor_virginica = cbind(inf23, sup23))
CI


### VERIFY HYPOTHESIS
graphics.off()
## univariate normality
p <- 3

# low
x11()
par(mfrow=c(1,3))
for(j in 1:p)
  plot(density(dfl[,j]))

x11()
par(mfrow=c(2,3))

for(j in 1:p)   # each feature
{
  boxplot(dfl[,j], main=(paste("(before) group: low, feature: ",j)))
}
pvalues_l <- NULL

for(j in 1:p) 
{
  qqnorm(dfl[,j], main=(paste("(before) group: low, feature: ",j)))
  qqline(dfl[,j])
  pvalues_l[j] = shapiro.test(dfl[,j])$p.value
}
pvalues_l
# identify and remove outliers:
M <- colMeans(dfl)
S <- cov(dfl)
# compute M distance (for each point, from sample mean)
d2 <- matrix(mahalanobis(dfl, M, S))
quantile(d2, 0.95)
# consider not-outliers d2<=6.5    ######## SCELTA A CAZZO, DA DISCUTERE #################
dfl_wo_outliers <- dfl[which(d2 <= 8), ]
dim(dfl_wo_outliers)

pvalues_l <- NULL
for(j in 1:p) 
{
  pvalues_l[j] = shapiro.test(dfl_wo_outliers[,j])$p.value
}
pvalues_l

# Box-cox transformations
lambdas <- NULL
# Box-Cox transformations can only be used on positive data ==> I use this extension
for(j in 1:p)
{
  lambdas[j] = as.numeric(yeojohnson(dfl_wo_outliers[,j])$lambda)
  dfl_wo_outliers[,j] = yjPower(dfl_wo_outliers[,j], lambdas[j])
}

x11()
par(mfrow=c(2,3))

for(j in 1:p)   # each feature
{
  boxplot(dfl_wo_outliers[,j], main=(paste("(after) group: low, feature: ",j)))
}
pvalues_l <- NULL

for(j in 1:p) 
{
  qqnorm(dfl_wo_outliers[,j], main=(paste("(after) group: low, feature: ",j)))
  qqline(dfl_wo_outliers[,j])
  pvalues_l[j] = shapiro.test(dfl_wo_outliers[,j])$p.value
}
pvalues_l

## medium
x11()
par(mfrow=c(2,3))

for(j in 1:p)   # each feature
{
  boxplot(dfm[,j], main=(paste("(before) group: medium, feature: ",j)))
}
pvalues_m <- NULL

for(j in 1:p) 
{
  qqnorm(dfm[,j], main=(paste("(before) group: medium, feature: ",j)))
  qqline(dfm[,j])
  pvalues_m[j] = shapiro.test(dfm[,j])$p.value
}
pvalues_m

# identify and remove outliers:
M <- colMeans(dfm)
S <- cov(dfm)
# compute M distance (for each point, from sample mean)
d2 <- matrix(mahalanobis(dfm, M, S))
quantile(d2, 0.9)
# consider not-outliers d2<=13    ######## SCELTA A CAZZO, DA DISCUTERE #################
dfm_wo_outliers <- dfm[which(d2 <= 6.5), ]
dim(dfm_wo_outliers)

pvalues_m <- NULL
for(j in 1:p) 
{
  pvalues_m[j] = shapiro.test(dfm_wo_outliers[,j])$p.value
}
pvalues_m

# Box-cox transformations
lambdas <- NULL
# Box-Cox transformations can only be used on positive data ==> I use this extension
for(j in 1:p)
{
  lambdas[j] = as.numeric(yeojohnson(dfm_wo_outliers[,j])$lambda)
  dfm_wo_outliers[,j] = yjPower(dfm_wo_outliers[,j], lambdas[j])
}

x11()
par(mfrow=c(2,3))

for(j in 1:p)   # each feature
{
  boxplot(dfm_wo_outliers[,j], main=(paste("(after) group: medium, feature: ",j)))
}
pvalues_m <- NULL

for(j in 1:p) 
{
  qqnorm(dfm_wo_outliers[,j], main=(paste("(after) group: medium, feature: ",j)))
  qqline(dfm_wo_outliers[,j])
  pvalues_m[j] = shapiro.test(dfm_wo_outliers[,j])$p.value
}
pvalues_m

## high
x11()
par(mfrow=c(2,3))

for(j in 1:p)   # each feature
{
  boxplot(dfh[,j], main=(paste("(before) group: high, feature: ",j)))
}
pvalues_h <- NULL

for(j in 1:p) 
{
  qqnorm(dfh[,j], main=(paste("(before) group: high, feature: ",j)))
  qqline(dfh[,j])
  pvalues_h[j] = shapiro.test(dfh[,j])$p.value
}
pvalues_h

# identify and remove outliers:
M <- colMeans(dfh)
S <- cov(dfh)
# compute M distance (for each point, from sample mean)
d2 <- matrix(mahalanobis(dfh, M, S))
quantile(d2, 0.9)
# consider not-outliers d2<=13    ######## SCELTA A CAZZO, DA DISCUTERE #################
dfh_wo_outliers <- dfh[which(d2 <= 12), ]
dim(dfh_wo_outliers)








