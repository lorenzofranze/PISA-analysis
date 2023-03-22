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

# change column names

# plots
x11()
boxplot(df)

x11()
pairs(df)