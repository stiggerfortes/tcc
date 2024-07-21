embed_matrix = function(x, num_lags){
  
  var_names = colnames(x)
  
  X = embed(as.matrix(x), num_lags)
  
  embed_col_names = c()
  for (i in 1:num_lags) {
    for (j in 1:length(var_names)) {
      lagged_var_name = paste(var_names[j], "lag", i, sep = "_")
      embed_col_names = c(embed_col_names, lagged_var_name)
    }
  }
  
  colnames(X) = embed_col_names
  
  return(X)
}


dataprep = function(ind, df, variable, horizon, lag_max = 12, add_dummy = FALSE, univar = FALSE, nofact = TRUE, factonly = FALSE)
{
  df = df[ind, ]
  y = df[, variable]
  
  if(nofact == TRUE) {
    if(univar == FALSE) {
      x = df
    }
    else {
      x = as.matrix(df[, variable])
    }
  }
  else{
    if(univar == FALSE) {
      factors = princomp(scale(df))$scores[, 1:4]
      if(factonly == TRUE) {
        x = cbind(df[, variable], factors)
      }
      else {
        x = cbind(df, factors)
      }
    }
    else {
      x = as.matrix(df[, variable])
    }
  }
  
  # X = embed(x, 12)          # para AR apenas
  X = embed_matrix(x, 12)   # para demais modelos
  
  Xin = X[-c((nrow(X) - horizon + 1):nrow(X)), ]
  Xout = X[nrow(X), ]
  Xout = t(as.vector(Xout))
  colnames(Xout) = colnames(Xin)
  yin = tail(y, nrow(Xin))
  
  return(list(Xin = Xin, Xout = Xout, yin = yin))
  
}

rungradboost = function(x, y, num_ite = 200, eta = 0.1) {
  require(mboost)
  
  x = as.matrix(x)
  
  modelest = glmboost(x, y, offset = 0, center = TRUE, control = boost_control(mstop = num_ite, nu = eta))
  
  bic = AIC(modelest, k = log(nrow(x)))
  all_attributes = attributes(bic)
  bic_seq = all_attributes$AIC
  
  m_opt = min(
    c(which(diff(bic_seq) > 0)[1],
      which.min(bic_seq)
    ),
    na.rm = TRUE
  )
  modelest[m_opt]
  
  df_opt = all_attributes$df[m_opt]
  
  coef_opt_aux = coef(modelest[m_opt])
  coef_opt = rep(0, ncol(x))
  names(coef_opt) = colnames(x)
  coef_opt[names(coef_opt_aux)] = coef_opt_aux
  
  return(
    list(
    reg_opt = modelest[m_opt], 
    coef_opt = coef_opt, 
    df_opt = df_opt, 
    m_opt = m_opt, 
    bic_opt = min(bic_seq)
    )
  )
}

runl2boost = function(ind, df, variable, horizon, num_ite = 200) {
  prep_data = dataprep(ind, df, variable, horizon)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  # gradient boost
  
  modelest = rungradboost(x = Xin, y = yin, num_ite = num_ite)
 
  # l2 boost

  Xin_mean = as.vector(apply(Xin, 2, mean))
  yin_mean = mean(yin)

  coef_train = modelest$coef_opt %>% as.vector()

  yout_hat = sum((Xout - Xin_mean) * coef_train) + yin_mean

  var_imp = varimp(modelest$reg_opt)
  
  outputs = list(regression = modelest, var_imp = var_imp)

  return(list(forecast = yout_hat, outputs = outputs))

}


runl2splines = function(ind, df, variable, horizon, num_ite = 70, eta = 0.2, degree = 2, dfsplines = 4, differences = NULL) {
  prep_data = dataprep(ind, df, variable, horizon, add_dummy = FALSE)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  
  require(mboost)

  bbs_names = c()
  for (col in 1:ncol(Xin)) {
    bbs_names = c(bbs_names, paste0("bbs(Xin[, ", col, "], df = ", dfsplines, ", degree = ", degree, ", differences = ", differences,")"))
  }
  
  fmla = as.formula(paste("yin ~", paste(bbs_names, collapse = "+")))
  
  # gradient boost
  
  modelest = gamboost(fmla,
                      family=Gaussian(),
                      control = boost_control(mstop = num_ite, nu = eta))
  
  bic = AIC(modelest, k = log(nrow(x)))
  all_attributes = attributes(bic)
  bic_seq = all_attributes$AIC
  
  m_opt = min(
    c(which(diff(bic_seq) > 0)[1],
      which.min(bic_seq)
    ),
    na.rm = TRUE
  )
  
  df_opt = all_attributes$df[m_opt]
  
  coef_opt_aux = coef(modelest[m_opt])
  
  coef_opt = matrix(0, nrow = length(coef_opt_aux[[1]]), ncol = ncol(Xout))
  colnames(coef_opt) = names(varimp(modelest[m_opt]))
  
  all_vars = names(coef_opt_aux)
  
  for (var in all_vars) {
    coef_opt[, var] = coef_opt_aux[[var]]
  }
  colnames(coef_opt) = colnames(Xout)
  
  # l2 boost
  
  Xin_mean = as.vector(apply(Xin, 2, mean))
  yin_mean = mean(yin)
  
  coef_train = t(colMeans(coef_opt))
  
  yout_hat = sum((Xout - Xin_mean) * coef_train) + yin_mean
  
  var_imp = varimp(modelest[m_opt])
  names(var_imp) = colnames(Xin)
  
  outputs = list(regression = modelest[m_opt], var_imp = var_imp)
  
  return(list(forecast = yout_hat, outputs = outputs))
  
}


rolling_window=function(fn,df,nwindow=1,horizon,variable,...){
  ind=1:nrow(df)
  window_size=nrow(df)-nwindow
  indmat=matrix(NA,window_size,nwindow)
  indmat[1,]=1:ncol(indmat)
  for(i in 2:nrow(indmat)){
    indmat[i,]=indmat[i-1,]+1
  }
  rw=apply(indmat,2,fn,df=df,horizon=horizon,variable=variable,...)
  forecast=unlist(lapply(rw,function(x)x$forecast))
  outputs=lapply(rw,function(x)x$outputs)
  return(list(forecast=forecast, outputs=outputs))
  
}


runarima = function(ind,df,variable,horizon) {
  require(forecast)
  
  prep_data = dataprep(ind,df,variable,horizon, add_dummy = FALSE)
  yin = prep_data$yin
  
  modelest = auto.arima(y = yin, ic = "bic", stationary = TRUE) 
  
  arima_forecast = forecast(modelest, h = horizon)
  yout_arima = as.vector(tail(arima_forecast$mean, 1))
  
  forecast = yout_arima
  
  return(list(yin = yin, forecast = forecast))
}

runar=function(ind,df,variable,horizon, type = "bic"){
  prep_data = dataprep(ind,df,variable,horizon, univar = TRUE, add_dummy = FALSE)
  Xin = prep_data$Xin
  yin = prep_data$yin
  Xout = prep_data$Xout
  # dummy = prep_data$dummy
  
  if(type=="fixed"){
    modelest=lm(yin~Xin+dummy)
    best = ncol(Xin)
  }
  
  if(type=="bic"){
    bb=Inf
    best = 1
    for(i in seq(1,ncol(Xin),1)){
      # m=lm(yin~Xin[,1:i]+dummy)
      m=lm(yin~Xin[,1:i])
      crit=BIC(m)
      if(crit<bb){
        bb=crit
        modelest=m
        best = i
      }
    }
  }
  coef=coef(modelest)
  coef[is.na(coef)] = 0
  forecast=c(1,Xout[,1:best])%*%coef
  
  return(list(forecast=forecast))
}
