library(mvtnorm)
library(MVN)
library(rgl)
library(car)
library(dbscan)
library(cluster)
library(fields)
library(ggplot2)

# import dataframe (just Italy)
dfITA<-get(load('dfITA_processed.RData'))

# extract actual features (unsupervised learning)
features <- dfITA[,2:9]

dim(features)
p = dim(features)[2]
n = dim(features)[1]

# set seed
set.seed(040523)

# jitter data
for(j in 1:p)
  features[,j] <- features[,j] + cbind(rnorm(n, sd=0.025))

# standardize data
features <- scale(features)
############################  hierarchical clustering  ##########################
# some plots
to_plot <- features[sample(n, size=50, replace=F),]

x11()
pairs(to_plot, main='pair')

### EUCLIDEAN DISTANCE

# compute dissimilarity matrix
Dmat_e <- dist(features, method='euclidean')

# compute clustering structure
eucl_single <- hclust(Dmat_e, method='single')
eucl_avg <- hclust(Dmat_e, method='average')
eucl_compl <- hclust(Dmat_e, method='complete')

# plot dendograms
x11()
par(mfrow=c(1,3))
plot(eucl_single, main='euclidean-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(eucl_avg, main='euclidean-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(eucl_compl, main='euclidean-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
# single sucks, complete is acceptable, average also sucks

## complete linkage

# cut at 2
rect.hclust(eucl_compl, k=2)
cluster.ec2 <- cutree(eucl_compl, k=2) 

# see if it makes sense (it doesn't)
x11()
par(mfrow=c(2,4))
for(j in 1:p)
  plot(features[,j], col = cluster.ec3+1)


# see if it makes sense
x11()
par(mfrow=c(2,4))
for(j in 1:p)
  plot(features[,j], col = cluster.mc2+1)

### manhattan distance

# compute dissimilarity matrix
Dmat_m <- dist(features, method='manhattan')

# compute clustering structure
manh_single <- hclust(Dmat_m, method='single')
manh_avg <- hclust(Dmat_m, method='average')
manh_compl <- hclust(Dmat_m, method='complete')

# plot dendograms
x11()
par(mfrow=c(1,3))
plot(manh_single, main='manhattan-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(manh_avg, main='manhattan-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(manh_compl, main='manhattan-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')

# sucks

### maximum distance

# dissimilarity matrix
Dmat_mx <- dist(features, method='maximum')

# clustering structure
max_single <- hclust(Dmat_mx, method='single')
max_avg <- hclust(Dmat_mx, method='average')
max_compl <- hclust(Dmat_mx, method='complete')

# plot dendograms
x11()
par(mfrow=c(1,3))
plot(max_single, main='manhattan-single', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(max_avg, main='manhattan-average', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')
plot(max_compl, main='manhattan-complete', hang=-0.1, xlab='', labels=F, cex=0.6, sub='')


### max dist
rect.hclust(max_compl, k=2)
cluster.mc2 <- cutree(max_compl, k=2) 


#####################