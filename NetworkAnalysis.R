library(corrr)

for_factor <- students[,c(1,863,887,895,896,898,900,903,904,872,873,874,875,879,880,883,884,888,889,890,891,892,899,905,908,935)]
#for_factor <- students[,c(1,863:943,1027,1037)]
#for_factor <- for_factor[,-c(6:10,41)]

for_factor <- na.omit(for_factor)
ffITA = for_factor[for_factor$CNTRYID == 'Italy', ]
ffITA <- na.omit(ffITA)
ffITA <- ffITA[,-c(1,7,15,26,16,24)]
for(j in 1:20)
  ffITA[,j] = as.numeric(as.character(ffITA[,j]))


quartz()
network_plot(cor(ffITA), curved = FALSE, min_cor = 0.1)

fac <- factanal(scale(ffITA), factors = 3)
fac

dfITA <- dfITA[,-c(1,12)]
fac <- factanal(dfITA, factors = 3)
fac

quartz()
heatmap(cor(ffITA))

da.tenere <- ffITA[,c(7,6,19,2,4,3,5)]
