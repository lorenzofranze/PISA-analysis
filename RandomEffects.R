library(missMDA)
library(ISLR)
library(leaps)
library(psych)
library(ggplot2)
library(insight)
library(lattice)
library(lme4)
library(assist)
library(fabricatr)

for_factor <- students[,c(1,3,863,887,895,896,898,900,903,904,872,873,874,875,879,880,883,884,888,889,890,891,892,899,905,908,935,1027)]
df <- students[,c(1,3,863,887,895,896,898,900,903,904,1027)]

for_factor$CNTSCHID <- as.factor(for_factor$CNTSCHID)

for(j in 4:dim(for_factor)[2])
  for_factor[,j] = as.numeric(as.character(for_factor[,j]))

for_factor <- for_factor[for_factor$CNTRYID %in% c("Italy"),]
for_factor2 <- as.data.frame(imputePCA(X = for_factor[,c(3:26,28)], ncp = 3)$completeObs)
for_factor2$CNTRYID <- for_factor$CNTRYID
for_factor2$CNTSCHID <- for_factor$CNTSCHID
for_factor2$CNTRYID <- droplevels(for_factor2$CNTRYID)
for_factor2$CNTSCHID <- droplevels(for_factor2$CNTSCHID)

lmm1 = lmer(PV1MATH ~ . + (1|CNTSCHID), 
            data = for_factor[for_factor$CNTRYID == "Japan",-3])
summary(lmm1)

jpn <- for_factor[for_factor$CNTRYID == "Japan",]
lmm1 = lmer(PV1MATH ~ . + (1 + EMOSUPS + PERCOMP + PERCOOP + COMPETE + GFOFAIL + RESILIENCE + MASTGOAL|CNTSCHID), 
            data = jpn[,-c(1,3)])
summary(lmm1)
# decidiamo di tenere solo COMPETE

lmm1 = lmer(PV1MATH ~ . + (1 + COMPETE|CNTSCHID), 
            data = jpn[,-c(1,3)])
summary(lmm1)

lmm1 = slm(PV1MATH ~ .,
           data = for_factor2[,-1])

jpn$BSMJ <-  
model1 <- lm(PV1MATH ~ )

