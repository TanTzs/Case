#' backtesting
#'@description
#' 不考虑交易的各种手续费的简单策略回测函数。
#' 当signal信号大于buy_signal时，买入amout_per金额的股票，
#' 当signal信号小于sell_signal时，卖出上一次交易买入的股票数。
#' 最终该函数将返回一个数据框，数据框包括整个回测过程的日期、信号交易以及全仓买入股票两种策略所持有的现金和股票的总价值。
#'
#' @param cash 交易开始时持有的现金
#' @param amount_per  每次执行策略所买入的金额
#' @param date 交易向量
#' @param signal 交易信号
#' @param stock 交易标的
#' @param buy_signal 买入信号
#' @param sell_signal 卖出信号
#'
#' @return 返回一个数据框，date列表示回测过程的日期，strategy表示执行策略收获的总资产，
#' baseline表示开始全仓买入收获的总资产
#' @export
#'
backtesting <- function(cash, amount_per,
                        date,
                        signal, stock,
                        buy_signal, sell_signal) {
  Asset <- rep(NA, length(signal))
  Asset_baseline <- rep(NA, length(signal))
  price <- stock
  gross_share <- 0

  for (i in 1:length(signal)) {
    num_share <- amount_per / price[i] # 可买入股票的份数

    # 基本策略
    if (i == 1) { # 全部买入资产
      baseline_share <- cash / price[i]
      Asset_baseline[i] <- baseline_share * price[i]
    } else {
      Asset_baseline[i] <- baseline_share * price[i]
    }

    # 信号策略
    if (signal[i] >= buy_signal & cash >= amount_per) { # 买入资产
      cash <- cash - amount_per # 花掉 amout_per 元
      gross_share <- gross_share + num_share # 增加 num_share 份股票
    } else if (signal[i] <= sell_signal & gross_share >= num_share) { # 卖出资产
      cash <- cash + num_share * price[i] # 卖出num_share份资产获得现金
      gross_share <- gross_share - num_share
    } else {
      NULL
    }
    Asset[i] <- cash + gross_share * price[i]
  }
  result <- data.frame(
    "date" = date,
    "strategy" = Asset,
    "baseline" = Asset_baseline
  )
  return(result)
}
