setwd("C:/Users/mariana/Desktop/TCC/Germany")
library(readxl)
library(urca)
library(dplyr)

# Get data for United Kingdom

NEW_GERMANY = read_excel("C:/Users/mariana/Downloads/NEW_Germany.xlsx")

Germany_varlist = colnames(NEW_GERMANY)[-1]

Germany_dates = NEW_GERMANY$DATE
Germany_data = as.data.frame(select(NEW_GERMANY, -DATE))
rownames(Germany_data) = Germany_dates

Germany_mat = matrix(NA, nrow = 300, ncol = 19)

colsec = 19
typesec = "trend"

plot.ts(Germany_data[, colsec])

# Rodar teste ADF
Germany_adf_df0 = ur.df(
  y = Germany_data[, colsec],
  type = typesec,
  lags = 10, 
  selectlags = "BIC")
summary(Germany_adf_df0)     

Germany_adf_df1 = ur.df(
  y = diff(Germany_data[, colsec], differences = 1), 
  type = typesec, 
  lags = 10, 
  selectlags = "BIC")
summary(Germany_adf_df1)     


Germany_mat[1:300, colsec] = Germany_data[, colsec]     # se estacionário
Germany_mat[2:300, colsec] = Germany_adf_df1@y          # se raiz unitária

Germany_df = as.data.frame(Germany_mat)
rownames(Germany_df) = Germany_dates
colnames(Germany_df) = Germany_varlist

germany_data = Germany_df[2:300, 1:19]

save(germany_data, file = "germany_data.rda")
