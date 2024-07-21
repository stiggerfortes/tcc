setwd("C:/Users/mariana/Desktop/TCC/UK")

#### gets out of sample y and computes random walk forecasts ###
library(roll)
load("uk_data.rda")

nwindows = 132

data = uk_data

y = data[,"GBRCPALTT01IXNBM"]
y = cbind(y,roll_prod(1+y,3)-1,roll_prod(1+y,6)-1,roll_prod(1+y,12)-1)
yout = tail(y,nwindows)

rw = matrix(NA,nwindows,12)
for(i in 1:12){
  aux=data[(nrow(data)-nwindows-i+1):(nrow(data)-i),"GBRCPALTT01IXNBM"]
  rw[,i]=aux;
}

save(yout,file = "forecasts/yout.rda")
save(rw,file = "forecasts/rw.rda")
