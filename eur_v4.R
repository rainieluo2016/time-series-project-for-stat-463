EUR = function (date){
  # setting
  library(quantmod)
  library(fGarch)
  library(tseries)
  library(forecast)
  getSymbols("USD/EUR",src="oanda")
  library(simts)
  EUR_TS=gts(USDEUR,start=2019,freq = 365)
  diff_eur=gts(diff(EUR_TS))
  
  datelist = seq(as.Date(rownames(data.frame(USDEUR)[1])[1]), as.Date(date)+9, by = "day")
  datelist = datelist [-c(1:179)]
  length = length(datelist)
  final_matrix = matrix (NA, nrow = length, ncol = 4)
  for (i in 1:length){
    final_matrix[i,1] = as.character(datelist[i])
  }
  
  # comparing the smallest ar 
  select_aic = select(AR(20), diff_eur, criterion = "aic")
  select_bic = select(AR(20), diff_eur, criterion = "bic")
  up = select_aic[["arma"]][1]
  down = select_bic[["arma"]][1]
  up
  down
  
  # find the smallest MAPE for ar
  MAPE_ar = 1000
  best_ar = NA
  for (i in down : up){
    fit = arima(EUR_TS, order=c(i,1,0))
    test1 = median(abs(fit$residuals))
    if (MAPE_ar > test1) {
      MAPE_ar = test1
      best_ar = c(i,1,0)
    }
  }
  
  MAPE_ar
  best_ar
  
  # best mape for arima
  MAPE_arima = 1000
  best_arima = NA
  for(i in 0:3){
    for(j in 0:3){
      fit <- arima(diff_eur, order=c(i,1,j))
      test1 = median(abs(fit$residuals))
      if (MAPE_arima > test1) {
        MAPE_arima = test1
        best_arima = c(i,1,j)
      }
    }
  }
  MAPE_arima
  best_arima
  
  if (MAPE_ar > MAPE_arima) {final_model = arima(EUR_TS, order = best_arima)
  }else {final_model = arima(EUR_TS, order = best_ar)}
  final_model
  
  res = residuals(final_model)
  res_garch = garchFit(~garch(1,0), data = res, cond.dist = "sstd",trace = FALSE)
  CI = predict(res_garch, n.ahead = length)[,1]*1.96
  
  #prediction
  pre = forecast(final_model, h =length,level = 0.95)
  for (i in 1:length){
    final_matrix[i,2] = round(pre$mean[i],digits = 6)
    final_matrix[i,3] = round(pre$mean[i]-CI[i],digits = 6)
    final_matrix[i,4] = round(pre$mean[i]+CI[i],digits = 6)
  }
  colnames(final_matrix) <- c('date', 'point prediction','lower bound','upper bound')
  # return only 10 result
  return(tail(final_matrix, n = 10L))
}
EUR("2019-12-10")
