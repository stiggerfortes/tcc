setwd("C:/Users/mariana/Desktop/TCC/Germany")

#### gets out of sample y and computes random walk forecasts ###
library(roll)
load("germany_data.rda")

nwindows = 132

data = germany_data

y = data[,"DEUCPIALLMINMEI"]
y = cbind(y,roll_prod(1+y,3)-1,roll_prod(1+y,6)-1,roll_prod(1+y,12)-1)
yout = tail(y,nwindows)

rw = matrix(NA,nwindows,12)
for(i in 1:12){
  aux=data[(nrow(data)-nwindows-i+1):(nrow(data)-i),"DEUCPIALLMINMEI"]
  rw[,i]=aux;
}


save(yout,file = "forecasts/yout.rda")
save(rw,file = "forecasts/rw.rda")
