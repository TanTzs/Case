#' GARCH
#' @description
#' 基于garch(1,1)模型和arma(1,1)模型得到的波动率、收益率以及VaR预测的函数
#'
#'
#' @import forecast
#' @import rugarch
#' @import dplyr
#'
#' @param data 数据框，每一列代表一天的价格数据
#' @param fore_L 选取data数据框的尾部fore_L列作为预测对象
#' @param tau 计算VaR时的选择的分位数
#'
#' @return 返回一个数据框，每一列依次为:收益率预测、波动率预测、基于garch(1,1)得到的VaR预测
#' @export

garch<-function(data,fore_L,tau){
  data = as.matrix(data)
  logreturn_day = apply(data,2,as.numeric)|>apply(2,function(x) sum(diff(log(x))))
  RV_day = apply(data,2,as.numeric)|>apply(2,function(x) sum(diff(log(x))^2))
  spec_garch = ugarchspec(variance.model = list(model = 'sGARCH',garchOrder = c(1,1)),
                          mean.model = list(armaOrder = c(1,1), include.mean = TRUE))
  RV_garch_est = rep(NA,fore_L)
  return_garch_est = rep(NA,fore_L)
  res_qt_est_garch = rep(NA,fore_L)
  window_width = length(logreturn_day) - fore_L
  for( i in 1:fore_L){
    print(paste0(i,'/',fore_L))
    # 波动率
    data_fit = logreturn_day[i:(i+window_width-1)] # 用于模型估计的数据

    spec_garch_fitted = ugarchfit(spec_garch,data = data_fit,solver = 'hybrid')
    forc = ugarchforecast(spec_garch_fitted,n.ahead = 1)

    RV_garch_est[i] = (forc@forecast$sigmaFor)^2
    return_garch_est[i] = forc@forecast$seriesFor

    # 残差
    res_qt_est_garch[i] = quantile(residuals(spec_garch_fitted, standardize = T),tau)
  }

  VaR_garch = -(return_garch_est+res_qt_est_garch*sqrt(RV_garch_est))

  result_garch = data.frame(
    'return_est' = return_garch_est,
    'rv_est' = RV_garch_est,
    'VaR_est' = VaR_garch
  )
  return(result_garch)
}

