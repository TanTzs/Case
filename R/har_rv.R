#' har_rv
#' @import forecast
#' @import dplyr
#'
#' @param data 数据框，每一列代表一天的价格数据
#' @param fore_L 选取data数据框的尾部fore_L列作为预测对象
#' @param tau 计算VaR时的选择的分位数
#'
#' @return 返回一个数据框，每一列依次为:收益率预测、波动率预测、基于HAR-RV方法得到的VaR预测以及
#' 基于历史模拟法得到的VaR的预测
#' @export

har_rv<-function(data,fore_L,tau){
  return_data = apply(log(data),2,diff) # 构造收益率数据
  RV_data = apply(return_data^2,2,sum) # 计算日度RV

  RV_all_data = matrix(NA,ncol = 4, nrow = length(RV_data)) # 用于保存HAR-RV所需数据
  colnames(RV_all_data) = c('RV','RV_t-1','RV_w','RV_m')

  # 保存数据
  RV_all_data[,'RV'] = RV_data
  RV_all_data[,'RV_t-1'] = c(NA,RV_data[1:(length(RV_data)-1)])
  for(i in 6:nrow(RV_all_data)){
    RV_all_data[i,'RV_w'] = mean(RV_all_data[(i-5):(i-1),'RV'])
  }
  for(i in 23:nrow(RV_all_data)){
    RV_all_data[i,'RV_m'] = mean(RV_all_data[(i-22):(i-1),'RV'])
  }
  RV_all_data_new = RV_all_data[23:nrow(RV_all_data),] # 去除缺失数据

  # 参数设置
  window_width = nrow(RV_all_data_new) - fore_L # 用于模型估计的窗宽

  RV_harrv_est = rep(NA, fore_L) # 保存预测值
  RV_real = tail(RV_all_data_new[,'RV'],fore_L) # 真实值

  return_data_day = apply(return_data,2,sum)
  return_harrv_est = rep(NA, fore_L) # 保存预测值
  return_real = tail(return_data_day,fore_L) # 真实值

  res_qt_est_hs = rep(NA,fore_L)
  res_qt_est_harrv = rep(NA,fore_L)

  for( i in 1:fore_L){
    print(paste0(i,'/',fore_L))
    # 波动率
    data_fit = as.data.frame(RV_all_data_new[i:(i+window_width-1),]) # 用于模型估计的数据
    data_4pre = RV_all_data_new[(i+window_width),-1]|>t()|>as.data.frame()
    HAR_md = lm(RV ~ `RV_t-1` + `RV_w` + `RV_m`, data = data_fit) # 模型估计
    RV_hat = HAR_md$fitted.values # 模型拟合值
    RV_harrv_est[i] = predict(HAR_md, newdata = data_4pre) # 模型预测

    # 收益率
    data_fit = return_data_day[(23+i):(23+(i+window_width-1))]
    ar_md = Arima(data_fit,c(1,0,1))
    AR_hat = ar_md$fitted # 模型拟合值
    return_harrv_est[i] = as.numeric(forecast(ar_md,1)$mean[[1]]) # 模型预测

    # 残差
    residuals_data = (data_fit - AR_hat)/sqrt(RV_hat)
    res_qt_est_hs[i] = quantile(data_fit,tau)
    res_qt_est_harrv[i] = quantile(residuals_data,tau)
  }

  # 计算VaR
  VaR_hs = -(res_qt_est_hs)
  VaR_harrv = -(return_harrv_est+res_qt_est_harrv*sqrt(RV_harrv_est))
  har_rv_result = data.frame(
    'return_est' = return_harrv_est,
    'rv_est' = RV_harrv_est,
    'VaR_est' = VaR_harrv,
    'VaR_hs' = VaR_hs
  )

  return(har_rv_result)
}
