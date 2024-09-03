#' iVXCalc
#' @description
#' 用于估计中国波指iVX
#'
#' @import dplyr
#' @import lubridate
#'
#' @param data 数据框，要求包含期权数据的交易日期、期权合约、类型、挂牌日期以及收盘价等数据
#' @param rate 与期权数据长度相等的利率向量或者常数，默认为利率为常数0.04
#'
#' @return 返回估计得到的iVX数据框，包括iVX的日期和值
#' @export

iVXCalc <- function(data, rate) {
  date <- unique(data$交易日期)
  if (length(rate) == 1) {
    rate_vec <- rep(rate, length(date))
  } else if (length(rate) == length(date)) {
    rate_vec <- rate
  } else {
    rate_vec <- rep(NA, length(date))
  }
  iVX <- rep(NA, length(date))
  for (i1 in 1:length(date)) {
    tryCatch(
      {
        print(i1)
        i <- date[i1]
        date_data <- filter(data, 交易日期 == i)
        rate <- rate_vec[i1]

        # 确定近月合约
        date_contact <- sort(ymd(unique(date_data$到期日期)))
        date_now <- ymd(i)

        date_1 <- date_contact[which((date_contact - date_now) >= 7)[1]] # 剩余到期日超过7天的最近到期合约
        date_data_1 <- filter(date_data, 到期日期 == date_1) # 近月合约数据

        data_call_1 <- filter(date_data_1, 类型 == "看涨")
        price_vec <- names(which(table(data_call_1$执行价) >= 2)) # 用于修正每天执行价重复出现的问题
        for (price_corrected in price_vec) {
          location <- which(data_call_1$执行价 == price_corrected)
          data_call_1 <- data_call_1[-location[-1], ]
        }

        symbol <- paste0(data_call_1$到期日期, ":", data_call_1$执行价)
        data_call_1 <- cbind(data_call_1, symbol)

        data_put_1 <- filter(date_data_1, 类型 == "看跌")
        price_vec <- names(which(table(data_put_1$执行价) >= 2)) # 用于修正每天执行价重复出现的问题
        for (price_corrected in price_vec) {
          location <- which(data_put_1$执行价 == price_corrected)
          data_put_1 <- data_put_1[-location[-1], ]
        }
        symbol <- paste0(data_put_1$到期日期, ":", data_put_1$执行价)
        data_put_1 <- cbind(data_put_1, symbol)

        # 确定S
        Smat_1 <- matrix(NA, nrow = 2, ncol = length(symbol))
        Smat_1[1, ] <- unique(date_data_1$执行价)
        for (i2 in 1:length(symbol)) {
          datacall <- filter(data_call_1, symbol == symbol[i2])
          dataput <- filter(data_put_1, symbol == symbol[i2])
          Smat_1[2, ][i2] <- (datacall$收盘价 - dataput$收盘价)
        }
        S_1 <- Smat_1[1, ][which.min(abs(Smat_1[2, ]))]

        # 计算NT和Tpara(T)
        NT_1 <- as.numeric(date_1 - date_now)
        Tpara_1 <- NT_1 / 365

        # 计算Fpara(F)
        Fpara_1 <- S_1 + exp(rate * Tpara_1) * (Smat_1[2, ][which(Smat_1[1, ] == S_1)])

        # 计算K0
        K_sorted <- sort(unique(date_data_1$执行价))
        K0 <- tail(K_sorted[which(K_sorted < Fpara_1)], 1)

        # 计算delta_K
        delta_K <- NULL
        for (i3 in 1:length(K_sorted)) {
          if (i3 == 1) {
            delta_K[i3] <- (K_sorted[2] - K0) / 2
          } else if (i3 == length(K_sorted)) {
            delta_K[i3] <- K_sorted[i3] - K_sorted[i3 - 1]
          } else {
            delta_K[i3] <- (K_sorted[i3 + 1] - K_sorted[i3 - 1]) / 2
          }
        }

        # 计算PK
        PK <- NULL
        PK_mat <- matrix(NA, nrow = 3, ncol = length(K_sorted))
        PK_mat[1, ] <- K_sorted

        for (i3 in 1:length(K_sorted)) {
          datacall <- filter(data_call_1, 执行价 == K_sorted[i3])
          dataput <- filter(data_put_1, 执行价 == K_sorted[i3])
          PK_mat[2, i3] <- datacall$收盘价 # call option的价格
          PK_mat[3, i3] <- dataput$收盘价 # call option的价格
        }

        for (i3 in 1:length(K_sorted)) {
          Ki <- K_sorted[i3]
          if (Ki < K0) {
            PK[i3] <- PK_mat[3, ][i3]
          } else if (Ki > K0) {
            PK[i3] <- PK_mat[2, ][i3]
          } else {
            PK[i3] <- (PK_mat[3, ][i3] + PK_mat[2, ][i3]) / 2
          }
        }

        # 计算近月波动率
        sigma_1 <- (2 / Tpara_1) * sum((delta_K / (K_sorted^2)) * exp(rate * Tpara_1) * PK) -
          (1 / Tpara_1) * ((Fpara_1 / K0) - 1)^2

        # 计算次近月波动率
        date_2 <- date_contact[which((date_contact - date_now) >= 7)[2]] # 剩余到期日超过7天的次最近到期合约
        date_data_2 <- filter(date_data, 到期日期 == date_2) # 近月合约数据

        data_call_2 <- filter(date_data_2, 类型 == "看涨")
        price_vec <- names(which(table(data_call_2$执行价) >= 2)) # 用于修正每天执行价重复出现的问题
        for (price_corrected in price_vec) {
          location <- which(data_call_2$执行价 == price_corrected)
          data_call_2 <- data_call_2[-location[-1], ]
        }

        symbol <- paste0(data_call_2$到期日期, ":", data_call_2$执行价)
        data_call_2 <- cbind(data_call_2, symbol)


        data_put_2 <- filter(date_data_2, 类型 == "看跌")
        price_vec <- names(which(table(data_put_2$执行价) >= 2)) # 用于修正每天执行价重复出现的问题
        for (price_corrected in price_vec) {
          location <- which(data_put_2$执行价 == price_corrected)
          data_put_2 <- data_put_2[-location[-1], ]
        }
        symbol <- paste0(data_put_2$到期日期, ":", data_put_2$执行价)
        data_put_2 <- cbind(data_put_2, symbol)

        # 确定S
        Smat_2 <- matrix(NA, nrow = 2, ncol = length(symbol))
        Smat_2[1, ] <- unique(date_data_2$执行价)
        for (i2 in 1:length(symbol)) {
          datacall <- filter(data_call_2, symbol == symbol[i2])
          dataput <- filter(data_put_2, symbol == symbol[i2])
          Smat_2[2, ][i2] <- (datacall$收盘价 - dataput$收盘价)
        }
        S_2 <- Smat_2[1, ][which.min(abs(Smat_2[2, ]))]

        # 计算NT和Tpara(T)
        NT_2 <- as.numeric(date_2 - date_now)
        Tpara_2 <- NT_2 / 365

        # 计算Fpara(F)
        Fpara_2 <- S_2 + exp(rate * Tpara_2) * (Smat_2[2, ][which(Smat_2[1, ] == S_2)])

        # 计算K0
        K_sorted <- sort(unique(date_data_2$执行价))
        K0 <- tail(K_sorted[which(K_sorted < Fpara_2)], 1)

        # 计算delta_K
        delta_K <- NULL
        for (i3 in 1:length(K_sorted)) {
          if (i3 == 1) {
            delta_K[i3] <- (K_sorted[2] - K0) / 2
          } else if (i3 == length(K_sorted)) {
            delta_K[i3] <- K_sorted[i3] - K_sorted[i3 - 1]
          } else {
            delta_K[i3] <- (K_sorted[i3 + 1] - K_sorted[i3 - 1]) / 2
          }
        }

        # 计算PK
        PK <- NULL
        PK_mat <- matrix(NA, nrow = 3, ncol = length(K_sorted))
        PK_mat[1, ] <- K_sorted

        for (i3 in 1:length(K_sorted)) {
          datacall <- filter(data_call_2, 执行价 == K_sorted[i3])
          dataput <- filter(data_put_2, 执行价 == K_sorted[i3])
          PK_mat[2, i3] <- datacall$收盘价 # call option的价格
          PK_mat[3, i3] <- dataput$收盘价 # call option的价格
        }

        for (i3 in 1:length(K_sorted)) {
          Ki <- K_sorted[i3]
          if (Ki < K0) {
            PK[i3] <- PK_mat[3, ][i3]
          } else if (Ki > K0) {
            PK[i3] <- PK_mat[2, ][i3]
          } else {
            PK[i3] <- (PK_mat[3, ][i3] + PK_mat[2, ][i3]) / 2
          }
        }
        # 计算次近月波动率
        sigma_2 <- (2 / Tpara_2) * sum((delta_K / (K_sorted^2)) * exp(rate * Tpara_2) * PK) -
          (1 / Tpara_2) * ((Fpara_2 / K0) - 1)^2

        # 计算iVX
        if (as.numeric(date_1 - date_now) >= 30) { # 若近月合约到期日天数不小于30天
          iVX[i1] <- 100 * sqrt(sigma_1)
        } else {
          iVX[i1] <- 100 * sqrt(
            (Tpara_1 * sigma_1 * ((NT_2 - 30) / (NT_2 - NT_1)) +
              Tpara_2 * sigma_2 * ((30 - NT_1) / (NT_2 - NT_1)))
            * (365 / 30)
          )
        }
      },
      warning = function(w) {
        message("警告信息: ", conditionMessage(w))
      },
      error = function(e) {
        message("警告信息: ", conditionMessage(e))
      }
    )
  }
  iVX_data_all <- data.frame(
    "date" = as.POSIXct.Date(ymd(date)),
    "iVX_est" = iVX
  )
  return(iVX_data_all)
}
