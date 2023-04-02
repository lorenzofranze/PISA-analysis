# import dataframes (already difficult)
library(foreign)
schools = read.spss("CY07_MSU_SCH_QQQ.sav", to.data.frame=TRUE)
students = read.spss("CY07_MSU_STU_QQQ.sav", to.data.frame=TRUE)
teachers = read.spss("CY07_MSU_TCH_QQQ.sav", to.data.frame=TRUE) 

# dataframe about students
n_stud = dim(students)[1]
p_stud = dim(students)[2]
# 1119 variables: way too many
head(students)

students_career = students[,c(1:14,430:511)]
dim(students_career)
head(students_career)

# prova con aggregati
# select interesting columns
prova = students[,c(892, 900, 901, 903)]

dim(prova)
summary(prova)

# remove NA's
prova = na.omit(prova)
dim(prova)

# some plots
quartz()
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

# change column names

# plots
x11()
boxplot(df)

x11()
pairs(df)

attach(df)
class(BSMJ)
df$BSMJ <- as.numeric(BSMJ)
class(EMOSUPS)
df$EMOSUPS <- as.numeric(EMOSUPS)
df$PERCOMP <- as.numeric(PERCOMP)
df$PERCOOP <- as.numeric(PERCOOP)
df$ATTLNACT <- as.numeric(ATTLNACT)
df$COMPETE <- as.numeric(COMPETE)
df$GFOFAIL <- as.numeric(GFOFAIL)
df$RESILIENCE <- as.numeric(RESILIENCE)
df$MASTGOAL <- as.numeric(MASTGOAL)

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


countries <- levels(df$CNTRYID)
countries <- countries[-c(6,12,18,33,40,53,66,75,80)]
R_adj.values <- list()
for (c in countries){
  df2 = df[df$CNTRYID==c,]
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

