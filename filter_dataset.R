library(foreign)

# import dataset
students = read.spss("CY07_MSU_STU_QQQ.sav", to.data.frame=TRUE)

# only Italy
df <- students[students$CNTRYID == 'Italy', ]

# select variables: identified by factor analysis + targets
df <- df[,c(904,903,899,887,896,895,898,1027,1037)]

# remove na's
df = na.omit(df)
dim(df)

# problem: it's all factors ==> get right format
for(j in 1:dim(df)[2])
  df[,j] = as.numeric(as.character(df[,j]))

save(df, file="new_features_targets.RData")
