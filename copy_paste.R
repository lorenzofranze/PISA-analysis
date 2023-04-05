### PRELIMINARY STUFF


# import dataframes (already difficult)
library(foreign)
# schools = read.spss("CY07_MSU_SCH_QQQ.sav", to.data.frame=TRUE)
students = read.spss("CY07_MSU_STU_QQQ.sav", to.data.frame=TRUE)
# teachers = read.spss("CY07_MSU_tch_QQQ.sav", to.data.frame=TRUE) 

# dataframe about students
n_stud = dim(students)[1]
p_stud = dim(students)[2]
# 1119 variables: way too many
head(students)

# select interesting variables
df = students[,c(1,863,887,895,896,898,900,903,904,1027,1037)]
head(df)
dim(df)

# remove NA's
df = na.omit(df)
dim(df)

# problem: it's all factors ==> get right format
attach(df)
class(BSMJ)
df$BSMJ <- as.numeric(as.character(BSMJ))
class(EMOSUPS)
df$EMOSUPS <- as.numeric(as.character(EMOSUPS))
df$PERCOMP <- as.numeric(as.character(PERCOMP))
df$PERCOOP <- as.numeric(as.character(PERCOOP))
df$COMPETE <- as.numeric(as.character(COMPETE))
df$GFOFAIL <- as.numeric(as.character(GFOFAIL))
df$RESILIENCE <- as.numeric(as.character(RESILIENCE))
df$MASTGOAL <- as.numeric(as.character(MASTGOAL))
detach(df)

# change column names
#colnames(df) <- c('country', 'exp_occupation', 'parents_support', 'compet_school',
                  #'coop_school', 'compet', 'fear_fail', 'resilience',
                  #'master_orient', 'math', 'reading')
# filter: italians
dfITA = df[df$CNTRYID == 'Italy',]

head(df)

attach(dfITA)

###################################################################################
# MANOVA ATTEMPT

# create variable factor: divide performance into 3 levels: low, medium, high
performance_levels <- NULL
for (i in 1:dim(df)[1])
{
  if(PV1MATH[i] <= 300.0)
    performance_levels[i] = 'l'
  else if (PV1MATH[i] > 300.0 & PV1MATH[i] <= 600.0)
    performance_levels[i] = 'm'
  else
    performance_levels[i] = 'h'
}

performance_levels <- factor(performance_levels)
dfITA["factor_PV1MATH"] <- performance_levels

dfl = dfITA[dfITA$factor_PV1MATH == 'l',2:8]
dfm = dfITA[dfITA$factor_PV1MATH == 'm',2:8]
dfh = dfITA[dfITA$factor_PV1MATH == 'h',2:8]

### check normality in each population

# univariate: 7 variable
## box- and qqnorm
# low
x11()
par(mfrow=c(2,4))

for(j in 1:dim(dfl)[2])   # each feature
{
  boxplot(dfl[,j])
}

x11()
par(mfrow=c(2,4))
for(j in 1:dim(dfl)[2]) 
{
  qqnorm(dfl[,j])
  qqline(dfl[,j])
}

# no outliers

# medium
x11()
par(mfrow=c(2,4))

for(j in 1:dim(dfm)[2])   # each feature
{
  boxplot(dfm[,j])
}
# some outliers in j=3 and j=7, very asymmetric j=2

# high
x11()
par(mfrow=c(2,4))

for(j in 1:dim(dfh)[2])   # each feature
{
  boxplot(dfh[,j])
}
# some outliers, mostly for j=1

graphics.off()

# multivariate
library(MVN)
library(car)

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
############################