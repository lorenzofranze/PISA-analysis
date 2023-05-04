### import dataset: All countries, no NA, not jittered 
library(MVN)
library(car)
library(foreign)
library(bestNormalize)
library("ggplot2")                     
library("GGally")

set.seed(040523)
df<-get(load("df_processed.RData"))
p=9
n=dim(df)[1]
## jitter the data ##

for(j in 2:p)
  df[,j] <- df[,j] + cbind(rnorm(n, sd=0.1))
head(df)

### BY COUNTRY ###
dfITA = df[df$CNTRYID == 'Italy', ]
dim(dfITA)
n = dim(dfITA)[1]  

features_ITA = dfITA[,2:p]

# plot densities
quartz()
par(mfrow=c(2,4))
for(j in 1:p)
  plot(density(features_ITA[,j]))
dev.off()

pairs(features_ITA)
ggpairs(features_ITA)

## test for bimodality on single variables 
library(multimode)
#emosup
modetest(features_ITA[,2], mod0=2, B=100)
locmodes(features_ITA[,2],mod0=2,display=TRUE)
# percoop
locmodes(features_ITA[,4],mod0=2,display=TRUE)

quartz()
par(mfrow=c(2,4))
for (i in 1:8) {
  locmodes(features_ITA[,i],mod0=2,display=TRUE)
  
}

# kernel density estimation
density(features_ITA[,2], bw = "nrd0")


# DBSCAN
library(dbscan)
features_ITA<-scale(features_ITA)
df_sample<-features_ITA[sample(nrow(features_ITA),size=200,replace=F),]
plot(df_sample)

minPts = 9# Dimensionality + 1
quartz()
kNNdistplot(features_ITA, k =minPts )
abline(h = 2, col = "red", lty = 2)

eps <- 2
dbs <- dbscan(features_ITA, eps, minPts)

# plot one feature at a time
quartz()
par(mfrow=c(2,4))
for (j in 1:8){
  plot(features_ITA[,j],col=dbs$cluster+1L,pch=19)
}
# conclusion: seems like also DBSCAN is fallimentary--> classify almost everything as a single group
# only identify "outliers"