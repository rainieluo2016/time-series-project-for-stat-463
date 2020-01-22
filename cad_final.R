CAD = function (date3){
  # setting
  library(quantmod)
  library(fGarch)
  library(tseries)
  library(forecast)
  getSymbols("USD/CAD",src="oanda")
  library(simts)
  CAD_TS=gts(USDCAD,start=2019,freq = 365)
  diff_cad=gts(diff(CAD_TS))
  
  datelist3 = seq(as.Date(rownames(data.frame(USDCAD)[1])[1]), as.Date(date3)+9, by = "day")
  datelist3 = datelist3 [-c(1:179)]
  length3 = length(datelist3)
  final_matrix3 = matrix (NA, nrow = length3, ncol = 4)
  for (i in 1:length3){
    final_matrix3[i,1] = as.character(datelist3[i])
  }
  
  # comparing the smallest ar 
  select_aic3 = select(AR(20), diff_cad, criterion = "aic")
  select_bic3 = select(AR(20), diff_cad, criterion = "bic")
  up3 = select_aic3[["arma"]][1]
  down3 = select_bic3[["arma"]][1]
  up3
  down3
  
  # find the smallest MAPE for ar
  MAPE_ar3 = 1000
  best_ar3 = NA
  for (i in down3 : up3){
    fit3 = arima(CAD_TS, order=c(i,1,0))
    test3 = median(abs(fit3$residuals))
    if (MAPE_ar3 > test3) {
      MAPE_ar3 = test3
      best_ar3 = c(i,1,0)
    }
  }
  
  MAPE_ar3
  best_ar3
  
  # best mape for arima
  MAPE_arima3 = 1000
  best_arima3 = NA
  for(i in 0:3){
    for(j in 0:3){
      fit3 <- arima(diff_cad, order=c(i,1,j))
      test3 = median(abs(fit3$residuals))
      if (MAPE_arima3 > test3) {
        MAPE_arima3 = test3
        best_arima3 = c(i,1,j)
      }
    }
  }
  MAPE_arima3
  best_arima3
  
  if (MAPE_ar3 > MAPE_arima3) {final_model3 = arima(CAD_TS, order = best_arima)
  }else {final_model3 = arima(CAD_TS, order = best_ar)}
  final_model3
  
  #prediction
  pre3 = forecast(final_model3, h =length3,level = 0.95)
  for (i in 1:length3){
    final_matrix3[i,2] = round(pre3$mean[i],digits = 6)
    final_matrix3[i,3] = round(pre3$lower[i],digits = 6)
    final_matrix3[i,4] = round(pre3$upper[i],digits = 6)
  }
  colnames(final_matrix3) <- c('date', 'point prediction','lower bound','upper bound')
  # return only 10 result
  return(tail(final_matrix3, n = 10L))
}
CAD("2019-12-10")
