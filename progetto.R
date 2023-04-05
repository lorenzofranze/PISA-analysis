# import dataframes (already difficult)
library(foreign)
schools = read.spss("CY07_MSU_SCH_QQQ.sav", to.data.frame=TRUE)
students = read.spss("CY07_MSU_STU_QQQ.sav", to.data.frame=TRUE)
teachers = read.spss("CY07_MSU_tch_QQQ.sav", to.data.frame=TRUE) 

# dataframe about students
n_stud = dim(students)[1]
p_stud = dim(students)[2]
# 1119 variables: way too many
head(students)

###################################################################################
students_career = students[,c(1:14,430:511)]
dim(students_career)

# prova con aggregati
# select interesting columns
prova = students[,c(892, 900, 901, 903)]

dim(prova)
summary(prova)

# remove NA's
prova = na.omit(prova)
dim(prova)

# some plots
x11()
boxplot(prova)

x11()
plot(prova)

mean = sapply(prova,mean)


# MOTIVATION
# select interesting variables
df = students[,c(1,863,887,895,896,897,898,900,903,904,1027,1037)]
head(df)

# remove NA's
df = na.omit(df)
dim(df)

# select studenti italiani
df = df[df$CNTRYID=='Italy',]
dim(df)

# plots
x11()
boxplot(df)

x11()
pairs(df)

###################################################################################
# MANOVA ATTEMPT

# select interesting variables
df = students[,c(1,863,887,895,896,898,900,903,904,1027,1037)]
head(df)
dim(df)

# remove NA's
df = na.omit(df)
dim(df)

# change column names
colnames(df) <- c('country', 'exp_occupation', 'parents_support', 'compet_school',
                  'coop_school', 'compet', 'fear_fail', 'resilience',
                  'master_orient', 'math', 'reading')

head(df)

library(MVN)
library(car)
library(heplots)

### factors: countries --> too many (?)

### check normality in each population

# univariate

# multivariate

### check omoschedasticity: rule of thumb

### MANOVA

### further analysis

dfITA = df[df$CNTRYID == 'Italy',]
detach(df)
attach(dfITA)
performance_levels <- NULL
for (i in 1:dim(df)[1]){
  if(PV1MATH[i] <= 300.0)
    performance_levels[i] = 'l'
  else if (PV1MATH[i] > 300.0 & PV1MATH[i] <= 600.0)
    performance_levels[i] = 'm'
  else
    performance_levels[i] = 'h'
}
performance_levels <- factor(performance_levels)
dfITA["factor_PV1MATH"] <- performance_levels




###########################
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


#######################################################################################################
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

model1 = lm(PV1MATH ~ BSMJ + 
              EMOSUPS + 
              PERCOMP + 
              PERCOOP + 
              ATTLNACT + 
              COMPETE + 
              GFOFAIL + 
              RESILIENCE + 
              MASTGOAL, data = df)
summary(model1)

model2 = lm(PV1MATH ~ BSMJ + 
              EMOSUPS + 
              PERCOMP + 
              PERCOOP + 
              ATTLNACT + 
              COMPETE + 
              GFOFAIL + 
              RESILIENCE, data = df)
summary(model2)

model3 = lm(PV1MATH ~ BSMJ + 
              EMOSUPS + 
              PERCOMP + 
              PERCOOP + 
              ATTLNACT + 
              COMPETE + 
              GFOFAIL, data = df)
summary(model3)

model4 = lm(PV1MATH ~ BSMJ + 
              EMOSUPS + 
              PERCOMP + 
              PERCOOP +
              COMPETE + 
              GFOFAIL, data = df)
summary(model4)

model5 = lm(PV1MATH ~ BSMJ + 
              EMOSUPS + 
              PERCOOP + 
              COMPETE + 
              GFOFAIL, data = df)
summary(model5)

####################
## Dopo aver selezionato le domande rimuoviamo i dati mancanti
## Verifico che CNTRYID è un factor ed estraggo i livelli
## Salvo i livelli in una lista e rimuovo quelli che non hanno neanche
## una coppia valida
## Creo una lista [key,value] = [paese,R^2_adj] vuota
## estraggo da df un singolo paese e lo salvo in df2
## creo il modello lineare con le variabili salvate come as.numeric()
## inserisco nella lista il valore

df <- na.omit(df)
class(CNTRYID)
levels(CNTRYID)

dfl = dfITA[dfITA$factor_PV1MATH == 'l',2:8]
dfm = dfITA[dfITA$factor_PV1MATH == 'm',2:8]
dfh = dfITA[dfITA$factor_PV1MATH == 'h',2:8]
resultl <- mvn(data = dfl)
resultm <- mvn(data = dfm)
resulth <- mvn(data = dfh)



countries <- levels(df$CNTRYID)
countries <- countries[-c(6,12,18,33,40,53,66,75,80)]
R_adj.values <- list()
Hyp_check <- list()
for (c in countries){
  df2l = dfl[dfl$CNTRYID==c,]
  df2m = dfm[dfm$CNTRYID==c,]
  df2h = dfh[dfh$CNTRYID==c,]
  Hyp_check[[c]] <- [mshapiro.test(t(df2l)),mshapiro.test(t(df2m))]
  
  
  
  model1 = lm(PV1MATH ~ BSMJ + 
                EMOSUPS + 
                PERCOMP + 
                PERCOOP + 
                ATTLNACT + 
                COMPETE + 
                GFOFAIL + 
                RESILIENCE + 
                MASTGOAL, data = df2)
  R_adj.values[[c]] <- summary(model1)$adj.r.squared
}
R_adj.values
df2mITA = dfm[dfm$]
