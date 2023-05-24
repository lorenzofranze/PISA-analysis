##################### IMPORT DATASET
library(MVN)
library(car)
library(foreign)
library(bestNormalize)
library("ggplot2")                     
library("GGally")
students = read.spss("CY6_MS_CMB_STU_QQQ.sav", to.data.frame=TRUE)

#################### FIRST LINEAR MODEL

df1 <- students[,c(1,678,810,820)]
df1 <- na.omit(df1)
for(i in 2:dim(students)[2])
  df1[,i] = as.numeric(as.character(df1[,i]))
lm1 <- lm(PV1MATH ~ CNTRYID + MOTIVAT + CNTRYID:MOTIVAT, data = df1[,-4])
summary(lm1)
# notiamo che le interationi motivazione-paese sono molto significative
# per molti paesi
# ==> decidiamo di dividere l'analisi in più paesi

################# CLUSTERING

Countries <- c("Italy", "Japan", "Qatar", "Sweden", "Thailand", "Mexico")
## hierarchical --> non funziona (troppi dati)
df2 <- df1[df1$CNTRYID %in% Countries,]
df2.e <- dist(df2[,3:4], method = 'euclidean')
df1.es <- hclust(df2.e, method='single')
df2.ea <- hclust(df2.e, method='average')
df3.ec <- hclust(df2.e, method='complete')
## kmeans
kmea <- kmeans(df2[,3:4], centers = 3)
quartz()
plot(df2[,2:4], col = kmea$cluster+2)
# funziona molto bene ==> procediamo con la MANOVA

################## MANOVA

TAU <- c()
for(i in dim(Countries)){
  dfm <- df2[df2$CNTRYID == Countries[i],]
  attach(dfm)
  fitmath <- manova(as.matrix(dfm[,3:4]) ~ MOTIVAT)
  W <- summary.manova(fitmath)$SS$Residuals
  m <- 
  TAU[i] <- fitmath$
}
# poi la finiamo

################# NAIVE LINEAR MODELS


