setwd("C:/Users/mariana/Desktop/TCC/Germany")

library(tidyverse)
library(matrixStats)
rm(list=ls())
load("forecasts/yout.rda")
load("forecasts/rw.rda")

rw_180 = rw
yout_180 = yout

model_files = setdiff(list.files("forecasts/"),c("rw.rda","yout.rda"))

models_list = list()
for(i in 1:length(model_files)){
  
  load(paste("forecasts/",model_files[i],sep = ""))
  models_list[[i]] = forecasts
  
}
names(models_list) = model_files


# RMSE ratio h = 1:12

rmse_models = lapply(models_list, function(x){
  sqrt(colMeans((x[, 1:12] - yout_180[, 1])^2))
})%>% Reduce(f = cbind)
colnames(rmse_models) = model_files

rmse_rw = sqrt(colMeans((rw_180[, 1:12] - yout_180[, 1])^2))

rmse_ratio = rmse_models / rmse_rw


# MAE ratio h = 1:12

mae_models = lapply(models_list, function(x){
  colMeans(abs(yout_180[, 1] - x[, 1:12]))
})%>% Reduce(f = cbind)
colnames(mae_models) = model_files

mae_rw = colMeans(abs(yout_180[, 1] - rw_180[, 1:12]))

mae_ratio = mae_models / mae_rw


# MAD ratio h = 1:12

mad_models = lapply(models_list, function(x){
  
  median_matrix_models = matrix(NA, nrow = nrow(rw), ncol = 12)
  
  for (hor in 1:12) {
    median_matrix_models[, hor] = median(yout_180[, 1] - x[, hor])
  }
  colnames(median_matrix_models) = colnames(rw)[1:12]
  
  mad_models = apply(
    abs((yout_180[, 1] - x[, 1:12]) - median_matrix_models),
    2, 
    median
  )
})%>% Reduce(f = cbind)
colnames(mad_models) = model_files


median_matrix_rw = matrix(NA, nrow = nrow(rw), ncol = 12)

for (hor in 1:12) {
  median_matrix_rw[, hor] = median(yout_180[, 1] - rw[, hor])
}
colnames(median_matrix_rw) = colnames(rw)[1:12]

mad_rw = apply(
  abs((yout_180[, 1] - rw[, 1:12]) - median_matrix_rw),
  2, 
  median
)

mad_ratio = mad_models / mad_rw

RMSE_TABLE = rbind(rmse_ratio)
MAE_TABLE = rbind(mae_ratio)
MAD_TABLE = rbind(mad_ratio)


# Create summary statistics h = 1:12

ave_rmse = rbind(as.matrix(colMeans(rmse_models)), mean(as.matrix(rmse_rw)))
rownames(ave_rmse)[nrow(ave_rmse)] = "RW_180"

max_rmse = rbind(as.matrix(colMaxs(rmse_models)), max(as.matrix(rmse_rw)))
rownames(max_rmse)[nrow(max_rmse)] = "RW_180"

min_rmse = rbind(as.matrix(colMins(rmse_models)), min(as.matrix(rmse_rw)))
rownames(min_rmse)[nrow(min_rmse)] = "RW_180"

SUMMARY_TABLE = cbind(ave_rmse, max_rmse, min_rmse)
colnames(SUMMARY_TABLE) = c("ave_rmse", "max_rmse", "min_rmse")

FINAL_TABLES = list(
  RMSE = RMSE_TABLE,
  MAE = MAE_TABLE,
  MAD = MAD_TABLE,
  SUMMARY = SUMMARY_TABLE
)

save(FINAL_TABLES, file = "final_tables.rda")
