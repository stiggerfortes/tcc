setwd("C:/Users/mariana/Desktop/TCC/UK")
library(readxl)
library(urca)
library(dplyr)

# Get data for United Kingdom

NEW_UK = read_excel("C:/Users/mariana/Downloads/NEW_UK.xlsx")

UK_varlist = colnames(NEW_UK)[-1]

UK_dates = NEW_UK$DATE
UK_data = as.data.frame(select(NEW_UK, -DATE))
rownames(UK_data) = UK_dates

UK_mat = matrix(NA, nrow = 312, ncol = 20)

colsec = 20
typesec = "trend"

plot.ts(UK_data[, colsec])

# Rodar teste ADF
UK_adf_df0 = ur.df(
  y = UK_data[, colsec],
  type = typesec,
  lags = 10, 
  selectlags = "BIC")
summary(UK_adf_df0)     

UK_adf_df1 = ur.df(
  y = diff(UK_data[, colsec], differences = 1), 
  type = typesec, 
  lags = 10, 
  selectlags = "BIC")
summary(UK_adf_df1)     


UK_mat[1:312, colsec] = UK_data[, colsec]     # se estacionário
UK_mat[2:312, colsec] = UK_adf_df1@y          # se raiz unitária

UK_df = as.data.frame(UK_mat)
rownames(UK_df) = UK_dates
colnames(UK_df) = UK_varlist

uk_data = UK_df[2:300, 1:20]

save(uk_data, file = "uk_data.rda")
