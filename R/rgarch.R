#' rgarch
#'
#' 基于rgarch模型和arma模型得到的波动率、收益率以及VaR预测的函数
#'
#' @import forecast
#' @import rugarch
#' @import dplyr
#' @importFrom xts xts as.xts
#'
#' @param data 数据框，每一列代表一天的价格数据
#' @param fore_L 选取data数据框的尾部fore_L列作为预测对象
#' @param tau 计算VaR时的选择的分位数
#'
#' @return 返回一个数据框，每一列依次为:收益率预测、波动率预测、基于rgarch得到的VaR预测
#' @export
rgarch<-function(data,fore_L,tau){
  data = as.matrix(data)
  logreturn_day = apply(data,2,as.numeric)|>apply(2,function(x) sum(diff(log(x))))
  RV_day = apply(data,2,as.numeric)|>apply(2,function(x) sum(diff(log(x))^2))
  data=data[,-which(RV_day==0)]
  logreturn_day = apply(data,2,as.numeric)|>apply(2,function(x) sum(diff(log(x))))
  RV_day = apply(data,2,as.numeric)|>apply(2,function(x) sum(diff(log(x))^2))

  spec_rgarch = ugarchspec(variance.model = list(model = 'realGARCH',garchOrder = c(1,1)),
                           mean.model = list(armaOrder = c(1,1), include.mean = TRUE))

  RV_rgarch_est = rep(NA,fore_L)
  return_rgarch_est = rep(NA,fore_L)
  res_qt_est_rgarch = rep(NA,fore_L)
  window_width = length(logreturn_day) - fore_L

  for( i in 1:fore_L){
    print(paste0(i,'/',fore_L))
    # 波动率
    data_fit = (logreturn_day[i:(i+window_width-1)]) # 用于模型估计的数据
    RVdata_fit = (RV_day[i:(i+window_width-1)])

    data_fit = as.xts(data_fit)
    RVdata_fit = as.xts(RVdata_fit)

    spec_rgarch_fitted = rugarch::ugarchfit(spec_rgarch,data = (data_fit),
                                            realizedVol = (RVdata_fit),
                                   solver = 'hybrid')
    forc = rugarch::ugarchforecast(spec_rgarch_fitted,n.ahead = 1)
    RV_rgarch_est[i] = (forc@forecast$sigmaFor)^2
    return_rgarch_est[i] = forc@forecast$seriesFor

    # 残差
    res_qt_est_rgarch[i] = quantile(residuals(spec_rgarch_fitted, standardize = T)[is.finite(residuals(spec_rgarch_fitted, standardize = T))],tau)
  }


  VaR_rgarch = -(return_rgarch_est+res_qt_est_rgarch*sqrt(RV_rgarch_est))

  result_rgarch = data.frame(
    'return_est' = return_rgarch_est,
    'rv_est' = RV_rgarch_est,
    'VaR_est' = VaR_rgarch
  )
  return(result_rgarch)
}
