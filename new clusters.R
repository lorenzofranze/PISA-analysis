library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)
library(ggplot2)

set.seed(040523)

# import dataset with features selected with factor analysis
dfITA <- get(load('datenere.RData'))
dim(dfITA)
head(dfITA)
n <- dim(dfITA)[1]
p <- dim(dfITA)[2]

x11()
plot(dfITA)

# jitter data
for(j in 1:p)
  dfITA[,j] <- dfITA[,j] + cbind(rnorm(n, sd=0.0025))

# standardize data
features <- scale(dfITA)

### EUCLIDEAN DISTANCE

# compute dissimilarity matrix
Dmat_e <- dist(features, method='euclidean')

# compute clustering structure
eucl_single <- hclust(Dmat_e, method='single')
eucl_avg <- hclust(Dmat_e, method='average')
eucl_compl <- hclust(Dmat_e, method='complete')
eucl_ward <- hclust(Dmat_e, method='ward.D')

# plot dendograms
x11()
par(mfrow=c(1,4))
plot(eucl_single, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(eucl_avg, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(eucl_compl, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(eucl_ward, main='euclidean-ward', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

## ward linkage

# cut at 2
rect.hclust(eucl_ward, k=2)
cluster.ew2 <- cutree(eucl_ward, k=2) 

# see if it makes sense (it doesn't)
x11()
par(mfrow=c(2,4))
for(j in 1:p)
  plot(features[,j], col = cluster.ew2+1)

############# evaluate
# compute the cophenetic matrices 
coph.es <- cophenetic(eucl_single)
coph.ea <- cophenetic(eucl_avg)
coph.ec <- cophenetic(eucl_compl)
coph.ew <- cophenetic(eucl_ward)

# compute cophenetic coefficients
es <- cor(Dmat_e, coph.es)
ea <- cor(Dmat_e, coph.ea)
ec <- cor(Dmat_e, coph.ec)
ew <- cor(Dmat_e, coph.ew)


