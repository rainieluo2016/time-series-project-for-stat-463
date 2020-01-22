JPY = function (date2){
  
  # setting
  library(quantmod)
  library(fGarch)
  library(forecast)
  library(tseries)
  getSymbols("USD/JPY",src="oanda")
  library(simts)
  JPY_TS=gts(USDJPY,start=2019,freq = 365)
  diff_jpy=gts(diff(JPY_TS))
  
  datelist2 = seq(as.Date(rownames(data.frame(USDJPY)[1])[1]), as.Date(date2)+9, by = "day")
  datelist2 = datelist2 [-c(1:179)]
  length2 = length(datelist2)
  final_matrix2 = matrix (NA, nrow = length2, ncol = 4)
  for (i in 1:length2){
    final_matrix2[i,1] = as.character(datelist2[i])
  }
  
  # comparing the smallest ar 
  select_aic2 = select(AR(20), diff_jpy, criterion = "aic")
  select_bic2 = select(AR(20), diff_jpy, criterion = "bic")
  up2 = select_aic2[["arma"]][1]
  down2 = select_bic2[["arma"]][1]
  up2
  down2
  
  # find the smallest MAPE for ar
  MAPE_ar2 = 1000
  best_ar2 = NA
  for (i in down2 : up2){
    fit2 = arima(JPY_TS, order=c(i,1,0))
    test2_1 = median(abs(fit2$residuals))
    if (MAPE_ar2 > test2_1) {
      MAPE_ar2 = test2_1
      best_ar2 = c(i,1,0)
    }
  }
  
  MAPE_ar2
  best_ar2
  
  # best mape for arima
  MAPE_arima2 = 1000
  best_arima2 = NA
  for(i in 0:3){
    for(j in 0:3){
      fit2 <- arima(diff_jpy, order=c(i,1,j))
      test2_1 = median(abs(fit2$residuals))
      if (MAPE_arima2 > test2_1) {
        MAPE_arima2 = test2_1
        best_arima2 = c(i,1,j)
      }
    }
  }
  MAPE_arima2
  best_arima2
  
  if (MAPE_ar2 > MAPE_arima2) {
    final_model2 = arima(JPY_TS, order = best_arima2)
    }
  else {
    final_model2 = arima(JPY_TS, order = best_ar2)
  }
  
  final_model2
  
  res2 = residuals(final_model2)
  res2_garch = garchFit(~garch(1,0), data = res2, cond.dist = "sstd",trace = FALSE)
  CI2 = predict(res2_garch, n.ahead = length2)[,1]*1.96
  
  #prediction
  pre2 = forecast(final_model2, h =length2,level = 0.95)
  for (i in 1:length2){
    final_matrix2[i,2] = round(pre2$mean[i],digits = 6)
    final_matrix2[i,3] = round(pre2$mean[i]-CI2[i],digits = 6)
    final_matrix2[i,4] = round(pre2$mean[i]+CI2[i],digits = 6)
  }
  colnames(final_matrix2) <- c('date', 'point prediction','lower bound','upper bound')
  # return only 10 result
  return(tail(final_matrix2, n = 10L))
  
}


JPY("2019-12-20")
