llf_heavy<-function(param,data,data_RV){#data是日度数据
  T<-length(data_RV);
  omega<-param[1];alpha<-param[2];beta<-param[3];
  rt<-data
  ht<-numeric(T)
  ht[1]<-T^(-1/2)*sum(data[1:floor(T^(1/2))]^2)
  lt<-numeric(T)#用来保存似然值的
  for(t in 2:T){#对天数循环
    ht[t]<-omega+alpha*data_RV[t-1]+beta*ht[t-1]
    lt[t]<-(-1/2*(log(ht[t])+rt[t]^2/ht[t]))
  }
  return(-sum(lt))
}

iter_heavy<-function(param,data,data_RV){
  T<-length(data_RV)
  omega<-param[1];alpha<-param[2];beta<-param[3];
  ht<-numeric(T+1)
  ht[1]<-T^(-1/2)*sum(data[1:floor(T^(1/2))]^2)
  for(t in 1:T){
    ht[t+1]<-omega+alpha*data_RV[t]+beta*ht[t]
  }
  return(ht)
}

ini_heavy<- c(1,1,0.5)#omega,alpha,beta
lower_heavy<-c(0,0,0)
upper_heavy<-c(Inf,Inf,1)

#' heavy
#'
#' 基于HEAVY模型和arma模型得到的波动率、收益率以及VaR预测的函数
#'
#' @import forecast
#' @import rugarch
#' @import dplyr
#'
#' @param data 数据框，每一列代表一天的价格数据
#' @param fore_L 选取data数据框的尾部fore_L列作为预测对象
#' @param tau 计算VaR时的选择的分位数
#'
#' @return 返回一个数据框，每一列依次为:收益率预测、波动率预测、基于HEAVY得到的VaR预测
#' @export
heavy<-function(data,fore_L,tau){
  data = as.matrix(data)
  logreturn_day = apply(data,2,as.numeric)|>apply(2,function(x) sum(diff(log(x))))
  RV_day = apply(data,2,as.numeric)|>apply(2,function(x) sum(diff(log(x))^2))

  RV_heavy_est = rep(NA,fore_L)
  return_heavy_est = rep(NA,fore_L)
  res_qt_est_heavy = rep(NA,fore_L)
  window_width = length(logreturn_day) - fore_L # 用于模型估计的窗宽
  #针对每一被预测天来看
  for (day_id in 1:fore_L){
    #获取数据
    data_est=logreturn_day[(day_id):(day_id-1+window_width)]
    data_RV=RV_day[(day_id):(day_id+window_width-1)]

    #针对模型来看
    fit_RV=nlminb(ini_heavy, llf_heavy, data=data_est,
                  lower=lower_heavy, upper=upper_heavy,data_RV=data_RV)
    theta_result=fit_RV[["par"]]

    #波动率
    a = iter_heavy(theta_result,data_est,data_RV)
    RV_heavy_est[day_id]=tail(a,1)
    RV_hat = a[-length(a)] # 模型拟合值

    #收益率
    data_fit = logreturn_day[day_id:(day_id+window_width-1)]
    ar_md = Arima(data_fit,c(1,0,1))
    AR_hat = ar_md$fitted # 模型拟合值
    return_heavy_est[day_id] = as.numeric(forecast(ar_md,1)$mean[[1]]) # 模型预测

    # 残差
    residuals_data = (data_fit - AR_hat)/sqrt(RV_hat)
    res_qt_est_heavy[day_id] = quantile(residuals_data,tau)

    ######################
    #####输出循环信息#####
    print(paste0(day_id,'/',fore_L))
  }

  # 计算VaR
  VaR_heavy = -(return_heavy_est+res_qt_est_heavy*sqrt(RV_heavy_est))

  result_heavy = data.frame(
    'return_est' = return_heavy_est,
    'rv_est' = RV_heavy_est,
    'VaR_est' = VaR_heavy
  )
  return(result_heavy)
}
