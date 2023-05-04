### PRELIMINARY STUFF
library(MVN)
library(car)
library(foreign)
library(bestNormalize)
library("ggplot2")                     
library("GGally")

# import dataframes (already difficult)
# schools = read.spss("CY07_MSU_SCH_QQQ.sav", to.data.frame=TRUE)
students = read.spss("CY07_MSU_STU_QQQ.sav", to.data.frame=TRUE)
# teachers = read.spss("CY07_MSU_tch_QQQ.sav", to.data.frame=TRUE) 
students=CY07_MSU_STU_QQQ
# dataframe about students
head(students)

# select interesting variables
df = students[,c(1,863,887,895,896,898,900,903,904,1027,1037)]
head(df)
dim(df)
save(df, file='students.RData')
D<-get(load("students.RData"))
# remove NA's
df = na.omit(df)
dim(df)

# problem: it's all factors ==> get right format
for(j in 2:9)
  df[,j] = as.numeric(as.character(df[,j]))

# change column names
#colnames(df) <- c('country', 'exp_occupation', 'parents_support', 'compet_school',
                  #'coop_school', 'compet', 'fear_fail', 'resilience',
                  #'master_orient', 'math', 'reading')
# filter: italians
dfITA = df[df$CNTRYID == 'Italy', ]
dim(dfITA)
n = dim(dfITA)[1]    # = ml+nm+nh

head(dfITA)

############################# EDA ###########################################
# ben
###################################################################################
# MANOVA ATTEMPT

# create variable factor: divide performance into 3 levels: low, medium, high

performance_levels <- NULL
for (i in 1:n)
{
  if(dfITA$PV1MATH[i] <= 300.0)
    performance_levels[i] = 'l'
  else if (dfITA$PV1MATH[i] > 300.0 & dfITA$PV1MATH[i] <= 600.0)
    performance_levels[i] = 'm'
  else
    performance_levels[i] = 'h'
}

performance_levels <- factor(performance_levels)
dfITA["factor_PV1MATH"] <- performance_levels

save(dfITA, file='dfITA_processed.RData')

#dfl = dfITA[dfITA$factor_PV1MATH == 'l',2:8]
#dfm = dfITA[dfITA$factor_PV1MATH == 'm',2:8]
#dfh = dfITA[dfITA$factor_PV1MATH == 'h',2:8]

head(dfITA)
features_ITA = dfITA[,2:8]

# plot densities
x11()
par(mfrow=c(2,4))
for(j in 1:p)
  plot(density(features_ITA[,j]))
# I can see multiple modes: try PCA
features_ITA.sd <- scale(features_ITA)
features_ITA.sd <- data.frame(features_ITA.sd)
pc.features_ITA <- princomp(features_ITA.sd, scores=T)

x11()
par(mfrow=c(2,4))
for(j in 1:p)
  plot(density(pc.features_ITA$scores[,j]))

# recover info about PV
for(j in 2:8)
  dfITA[,j] = pc.features_ITA$scores[,j-1]

# divide populations
dfl = dfITA[dfITA$factor_PV1MATH == 'l',2:8]
dfm = dfITA[dfITA$factor_PV1MATH == 'm',2:8]
dfh = dfITA[dfITA$factor_PV1MATH == 'h',2:8]

# set params
nl = dim(dfl)[1]
nm = dim(dfm)[1]
nh = dim(dfh)[1]
p = dim(dfl)[2]   # = with m, h

### check (and force) normality in each population
## univariate: 7 variable
## boxplot and qqnorm

# low
x11()
par(mfrow=c(2,4))

for(j in 1:p)   # each feature
{
  boxplot(dfl[,j], main=(paste("after group: low, feature: ",j)))
}
pvalues_l <- NULL

x11()
par(mfrow=c(2,4))
for(j in 1:p) 
{
  qqnorm(dfl[,j], main=(paste("after group: low, feature: ",j)))
  qqline(dfl[,j])
  pvalues_l[j] = shapiro.test(dfl[,j])$p.value
}
pvalues_l

# medium: still not okay (outliers)
x11()
par(mfrow=c(2,4))

for(j in 1:p)   # each feature
{
  boxplot(dfm[,j], main=paste("group: medium, feature: ",j))
}
# some outliers in j=3 and j=7, very asymmetric j=2

# identify and remove outliers:
M <- colMeans(dfm)
S <- cov(dfm)
# compute M distance (for each point, from sample mean)
d2 <- matrix(mahalanobis(dfm, M, S))
quantile(d2, 0.9)
# consider not-outliers d2<=13    ######## SCELTA A CAZZO, DA DISCUTERE #################
dfm_wo_outliers <- dfm[which(d2 <= 12), ]
dim(dfm_wo_outliers)

# Box-cox transformations
lambdas <- NULL
# Box-Cox transformations can only be used on positive data ==> I use this extension
for(j in 1:p)
{
  lambdas[j] = as.numeric(yeojohnson(dfl_wo_outliers[,j])$lambda)
  dfl_wo_outliers[,j] = yjPower(dfl_wo_outliers[,j], lambdas[j])
}

x11()
par(mfrow=c(2,4))
for(j in 1:p)   # each feature
{
  boxplot(dfm_wo_outliers[,j], main=paste("group: medium, feature: ",j))
}

x11()
par(mfrow=c(2,4))
pvalues_m <- NULL
for(j in 1:p) 
{
  qqnorm(dfm_wo_outliers[,j], main=paste("group: medium, feature: ",j))
  qqline(dfm_wo_outliers[,j])
  pvalues_m[j] = shapiro.test(dfm_wo_outliers[,j])$p.value
}
pvalues_m

# high
x11()
par(mfrow=c(2,4))

for(j in 1:p)   # each feature
{
  boxplot(dfh[,j])
}
# some outliers, mostly for j=1
# identify and remove outliers:
M <- colMeans(dfh)
S <- cov(dfh)
# compute M distance (for each point, from sample mean)
d2 <- matrix(mahalanobis(dfh, M, S))
# consider not-outliers d2<=15    ######## SCELTA A CAZZO, DA DISCUTERE #################
quantile(d2, 0.9)
dfh_wo_outliers <- dfh[which(d2 <= 12), ]

x11()
par(mfrow=c(2,4))
for(j in 1:p)   # each feature
{
  boxplot(dfh_wo_outliers[,j])
}

x11()
par(mfrow=c(2,4))
pvalues_h = NULL
for(j in 1:p) 
{
  qqnorm(dfh[,j], main=paste("group: high, feature: ",j))
  qqline(dfh[,j])
  pvalues_h[j] = shapiro.test(dfh[,j])$p.value
}
pvalues_h

graphics.off()

# multivariate

resultl <- mvn(data = dfl)
resultm <- mvn(data = dfm)
resulth <- mvn(data = dfh)
### check omoschedasticity: rule of thumb

### MANOVA

### further analysis







###########################  for 2-ways MANOVA: PVread ########################
performance_levels <- NULL
for (i in 1:dim(df)[1]){
  if(PV1READ[i] <= 300.0)
    performance_levels[i] = 'l'
  else if (PV1READ[i] > 300.0 & PV1READ[i] <= 600.0)
    performance_levels[i] = 'm'
  else
    performance_levels[i] = 'h'
}
performance_levels <- factor(performance_levels)
df["factor_PV1READ"] <- performance_levels
####################  failed normalization attempts   ########################

# identify and remove outliers:
M <- colMeans(dfl)
S <- cov(dfl)
# compute M distance (for each point, from sample mean)
d2 <- matrix(mahalanobis(dfl, M, S))
# consider not-outliers d2<=15    ######## SCELTA A CAZZO, DA DISCUTERE #################
dfl_wo_outliers <- dfl[which(d2 <= 15), ]

# jitter data
for(j in 1:p)
  dfl_wo_outliers[,j] <- dfl_wo_outliers[,j] + cbind(rnorm(nl, sd=0.025))

# Box-cox transformations
lambdas <- NULL
lambdas[1] <- powerTransform(dfl_wo_outliers[,1])$lambda
dfl_wo_outliers[,1] <- bcPower(dfl_wo_outliers[,1], lambdas[1])
# Box-Cox transformations can only be used on positive data ==> I use this extension

for(j in 2:p)
{
  lambdas[j] = as.numeric(yeojohnson(dfl_wo_outliers[,j])$lambda)
  dfl_wo_outliers[,j] = yjPower(dfl_wo_outliers[,j], lambdas[j])
}


graphics.off()

