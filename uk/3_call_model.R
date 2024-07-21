setwd("C:/Users/mariana/Desktop/TCC/UK")
rm(list = ls())
### must add package for specific models ###
# library(devtools)
# install_github("gabrielrvsc/HDeconometrics")
library(HDeconometrics)
library(glmnet)
library(dplyr)

source("functions/rolling_window.R")
source("functions/functions.R")

#####
## The file with the forecasts will be saved with model_name
model_name = "l2"
## The function called to run models is model_function, which is a function from functions.R
model_function = runl2boost
#####


load("uk_data.rda")
data = uk_data

####### run rolling window ##########
nwindows = 132
model_list = list()
for(i in 1:12){
  model = rolling_window(model_function,data,nwindows+i-1,i,"GBRCPALTT01IXNBM")
  model_list[[i]] = model
  cat(i,"\n")
}

forecasts = Reduce(cbind,lapply(model_list, function(x)head(x$forecast,nwindows)))

save(forecasts, file = paste("forecasts/",model_name,".rda",sep = ""))

par(mar = c(4, 4, 1, 1) + 0.1)
plot(tail(data[,"GBRCPALTT01IXNBM"], nwindows),type = "l", xlab = "Janela móvel", ylab = "Valor", cex.lab = 0.8, cex.axis = 0.8)
lines(forecasts[,1],col = 3)
