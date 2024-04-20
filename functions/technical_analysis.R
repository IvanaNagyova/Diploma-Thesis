################# Technical Analysis #################
add_indicators <- function(prices_xts, parameters){
  #### Simple Moving Averages ####
  prices$SMA_20 <<- SMA(prices$close, n = 20)
  prices$SMA_50 <<- SMA(prices$close, n = 50)
  prices$SMA_100 <<- SMA(prices$close, n = 100)
  #### Exponential Moving Averages ####
  prices$EMA_20 <<- EMA(prices$close, n = 20, na.rm=TRUE)
  prices$EMA_50 <<- EMA(prices$close, n = 50, na.rm=TRUE)
  prices$EMA_100 <<- EMA(prices$close, n = 100, na.rm=TRUE)
  #### Commodity Channel Index ####
  prices$CCI <<- as.numeric(CCI(prices_xts[, c("high", "low", "close")]))
  #### Relative Strength Index ####
  prices$RSI <<- RSI(price = prices$close, n = 14)
  ### Computing ATR
  compute_atr(parameters$atr_period, parameters$atr_multiplier)
  #### Computing Stochastic
  compute_stochastic()
  #### Computing MACD
  compute_MACD()
  ### Bollinger Bands
  compute_BBand()
  print("Indicators added")
}

## Average True Range ####
compute_atr <- function(atr_period, atr_multiplier) {
  prices$atr <<- ATR(HLC(prices), n = atr_period, maType = "SMA")[, "atr"]
  prices$ATRUpper_Band <<- prices$SMA_20 + atr_multiplier * prices$atr
  prices$ATRLower_Band <<- prices$SMA_20 - atr_multiplier * prices$atr
}

## Stochastic Oscillator ####
compute_stochastic <- function(){
  stoch_result <- stoch(prices$high, prices$low, prices$close, nFastK = 14, nFastD = 3, nSlowD = 3, maType = "SMA", bounded = TRUE, smooth = 1)
  prices$Stochastic_fK <<- stoch_result[, "fastK"]
  prices$Stochastic_fD <<- stoch_result[, "fastD"]
}

## Moving Average Convergence Divergence ####
compute_MACD <- function(){
  macd_result <- MACD(prices$close, nFast = 12, nSlow = 26, nSig = 9, maType = "EMA", percent = FALSE)
  prices$MACD <<- macd_result[, "macd"]
  prices$signal_line <<- macd_result[, "signal"]
  prices$macd_histogram <<- prices$MACD - prices$signal_line
}

## Bollinger Bands ####
compute_BBand <- function(){
  bbands_result  <- BBands( prices[,c("high","low","close")], n = 20, maType = "SMA", sd = 2 )
  prices$BB_UpperBand <<- bbands_result[, "up"]
  prices$BB_LowerBand <<- bbands_result[, "dn"]
}

#######################################################
##### Signal detection #####

add_signals <- function(){
  SMA_signals()
  EMA_signals()
  Stochastic_signals()
  ATR_signals()
  CCI_signals()
  MACD_signals()
  RSI_signals()
  BBand_signals()
  print("Signals added")
}

## Simple Moving Average ####
SMA_signals <- function() {
  SMA_signal_buy <- integer(nrow(prices))
  SMA_signal_sell <- integer(nrow(prices))
  
  for (i in 1:nrow(prices)) {
    if (
      (!is.na(prices$SMA_20[i]) && !is.na(prices$SMA_50[i])) && 
      (!is.na(prices$SMA_20[i-1]) && !is.na(prices$SMA_50[i-1]))
    ){
      if (prices$SMA_20[i] > prices$SMA_50[i] && 
          prices$SMA_20[i-1] < prices$SMA_50[i-1]) {
        SMA_signal_buy[i] <- 1
      } 
      if (prices$SMA_20[i] < prices$SMA_50[i] && 
          prices$SMA_20[i-1] > prices$SMA_50[i-1]) {
        SMA_signal_sell[i] <- 1
      } 
    } 
  }
  prices$SMA_signal_buy <<- SMA_signal_buy
  prices$SMA_signal_sell <<- SMA_signal_sell
}




## Exponential Moving Average ####
EMA_signals <- function() {
  EMA_signal_buy <- integer(nrow(prices))
  EMA_signal_sell <- integer(nrow(prices))
  
  for (i in 1:nrow(prices)) {
    if (
      (!is.na(prices$EMA_20[i]) && !is.na(prices$EMA_50[i])) && 
      (!is.na(prices$EMA_20[i-1]) && !is.na(prices$EMA_50[i-1]))
    ){
      if (prices$EMA_20[i] > prices$EMA_50[i] && 
          prices$EMA_20[i-1] < prices$EMA_50[i-1]) {
        EMA_signal_buy[i] <- 1
      } 
      if (prices$EMA_20[i] < prices$EMA_50[i] && 
          prices$EMA_20[i-1] > prices$EMA_50[i-1]) {
        EMA_signal_sell[i] <- 1
      } 
    } 
  }
  prices$EMA_signal_buy <<- EMA_signal_buy
  prices$EMA_signal_sell <<- EMA_signal_sell
}

## Stochastic Oscillator ####
Stochastic_signals <- function() {
  Stochastic_signal_buy <- integer(nrow(prices))
  Stochastic_signal_sell <- integer(nrow(prices))
  
  for (i in 1:nrow(prices)) {
    if (
      (!is.na(prices$Stochastic_fK[i]) && !is.na(prices$Stochastic_fD[i])) && 
      (!is.na(prices$Stochastic_fK[i-1]) && !is.na(prices$Stochastic_fD[i-1]))
    ){
      if (prices$Stochastic_fK[i] > prices$Stochastic_fD[i] && 
          prices$Stochastic_fK[i] < 0.2 &&
          prices$Stochastic_fK[i-1] < prices$Stochastic_fD[i-1]) {
        Stochastic_signal_buy[i] <- 1
      } 
      if (prices$Stochastic_fK[i] < prices$Stochastic_fD[i] && 
          prices$Stochastic_fK[i] > 0.8 &&
          prices$Stochastic_fK[i-1] > prices$Stochastic_fD[i-1]) {
        Stochastic_signal_sell[i] <- 1
      } 
    } 
  }
  prices$Stochastic_signal_buy <<- Stochastic_signal_buy
  prices$Stochastic_signal_sell <<- Stochastic_signal_sell
}

## Average True Range ####
ATR_signals <- function() {
  ATR_signal_buy <- integer(nrow(prices))
  ATR_signal_sell <- integer(nrow(prices))
  
  for (i in 1:nrow(prices)) {
    if (
      (!is.na(prices$ATRLower_Band[i]) && !is.na(prices$ATRUpper_Band[i])) && 
      (!is.na(prices$ATRLower_Band[i-1]) && !is.na(prices$ATRUpper_Band[i-1]))
    ){
      if (prices$low[i] < prices$ATRLower_Band[i] && 
          prices$low[i-1] > prices$ATRLower_Band[i-1]) {
        ATR_signal_buy[i] <- 1
      } 
      if (prices$high[i] > prices$ATRUpper_Band[i] && 
          prices$high[i-1] < prices$ATRUpper_Band[i-1]) {
        ATR_signal_sell[i] <- 1
      } 
    } 
  }
  prices$ATR_signal_buy <<- ATR_signal_buy
  prices$ATR_signal_sell <<- ATR_signal_sell
}

## Commodity Channel Index ####
CCI_signals <- function() {
  CCI_signal_buy <- integer(nrow(prices))
  CCI_signal_sell <- integer(nrow(prices))
  
  for (i in 1:nrow(prices)) {
    if (
      (!is.na(prices$CCI[i])) && 
      (!is.na(prices$CCI[i-1]))
    ){
      if (prices$CCI[i] > -100 && 
          prices$CCI[i] < 0 && 
          prices$CCI[i-1] < -100) {
        CCI_signal_buy[i] <- 1
      } 
      if (prices$CCI[i] < 100 && 
          prices$CCI[i] > 0 && 
          prices$CCI[i-1] > 100) {
        CCI_signal_sell[i] <- 1
      } 
    } 
  }
  prices$CCI_signal_buy <<- CCI_signal_buy
  prices$CCI_signal_sell <<- CCI_signal_sell
}

## Moving Average Convergence Divergence ####
MACD_signals <- function() {
  MACD_signal_buy <- integer(nrow(prices))
  MACD_signal_sell <- integer(nrow(prices))
  
  for (i in 1:nrow(prices)) {
    if (
      (!is.na(prices$MACD[i]) && !is.na(prices$signal_line[i])) && 
      (!is.na(prices$MACD[i-1]) && !is.na(prices$signal_line[i-1]))
    ){
      if (prices$MACD[i] > prices$signal_line[i] && 
          prices$MACD[i-1] < prices$signal_line[i-1]) {
        MACD_signal_buy[i] <- 1
      } 
      if (prices$MACD[i] < prices$signal_line[i] && 
          prices$MACD[i-1] > prices$signal_line[i-1]) {
        MACD_signal_sell[i] <- 1
      } 
    } 
  }
  prices$MACD_signal_buy <<- MACD_signal_buy
  prices$MACD_signal_sell <<- MACD_signal_sell
}


## Bollinger Bands ####
BBand_signals <- function() {
  BBand_signal_buy <- integer(nrow(prices))
  BBand_signal_sell <- integer(nrow(prices))
  
  for (i in 1:nrow(prices)) {
    if (
      (!is.na(prices$BB_UpperBand[i]) && !is.na(prices$BB_LowerBand[i])) && 
      (!is.na(prices$BB_UpperBand[i-1]) && !is.na(prices$BB_LowerBand[i-1]))
    ){
      if (prices$low[i] < prices$BB_LowerBand[i] && 
          prices$low[i-1] > prices$BB_LowerBand[i-1]) {
        BBand_signal_buy[i] <- 1
      } 
      if (prices$high[i] > prices$BB_UpperBand[i] && 
          prices$high[i-1] < prices$BB_UpperBand[i-1]) {
        BBand_signal_sell[i] <- 1
      } 
    } 
  }
  prices$BBand_signal_buy <<- BBand_signal_buy
  prices$BBand_signal_sell <<- BBand_signal_sell
}



## Relative Strength Index ####
RSI_signals <- function() {
  RSI_signal_buy <- integer(nrow(prices))
  RSI_signal_sell <- integer(nrow(prices))
  
  for (i in 1:nrow(prices)) {
    if (
      (!is.na(prices$RSI[i])) && 
      (!is.na(prices$RSI[i-1]))
    ){
      if (prices$RSI[i] > 30 && 
          prices$RSI[i-1] < 30) {
        RSI_signal_buy[i] <- 1
      } 
      if (prices$RSI[i] < 70 && 
          prices$RSI[i-1] > 70) {
        RSI_signal_sell[i] <- 1
      } 
    } 
  }
  prices$RSI_signal_buy <<- RSI_signal_buy
  prices$RSI_signal_sell <<- RSI_signal_sell
}

#############################################################
#### Plotting #####
Plot_Prices <- function() {
  plot_prices <- plot_ly(prices, x = ~date, name = "Candles", type = "candlestick", open = ~open, close = ~close, high = ~high, low = ~low, increasing = list(line = list(color = "#96D875")), decreasing = list(line = list(color = "#FA9693"))) %>%
    layout(title = "Price Movement", 
           xaxis = list(title = "Date", rangeslider = list(visible = FALSE), type = "date"), 
           yaxis = list(title = "Price"), showlegend = TRUE)
  return(plot_prices)
}

Plot_SMA <- function(){
  plot_SMA <- plot_ly(prices, x = ~date, name = "Candles", type = "candlestick", open = ~open, close = ~close, high = ~high, low = ~low, increasing = list(line = list(color = "#96D875")), decreasing = list(line = list(color = "#FA9693"))) %>%
    add_lines(y = ~SMA_20, name = "SMA 20", line = list(color = "#FF000080", width = 1)) %>%
    add_lines(y = ~SMA_50, name = "SMA 50", line = list(color = "#2196f3", width = 1)) %>%
    add_lines(y = ~SMA_100, name = "SMA 100", line = list(color = "#F0D61A", width = 2)) %>%
    add_markers(data = subset(prices, SMA_signal_buy == 1), y = ~SMA_20, name = "Buy Signal", marker = list(color = "#008000", size = 7)) %>%
    add_markers(data = subset(prices, SMA_signal_sell == 1), y = ~SMA_20, name = "Sell Signal", marker = list(color = "#FF0B04", size = 7)) %>%
    layout(title = "Price Movement with SMAs and Signals (Close)",
           xaxis = list(rangeslider = list(visible = F)),
           yaxis = list(title = "Close Price",autorange = TRUE),
           showlegend = TRUE)
  plot_SMA
}

Plot_EMA <- function(){
  plot_EMA <- plot_ly(prices, x = ~date, name = "Candles", type = "candlestick",open = ~open, close = ~close, high = ~high, low = ~low, increasing = list(line = list(color = "#96D875")), decreasing = list(line = list(color = "#FA9693"))) %>%
    add_lines(y = ~EMA_20, name = "EMA 20", line = list(color = "#FF000080", width = 1)) %>%
    add_lines(y = ~EMA_50, name = "EMA 50", line = list(color = "#2196f3", width = 1)) %>%
    add_lines(y = ~EMA_100, name = "EMA 100", line = list(color = "#F0D61A", width = 1)) %>%
    add_markers(data = subset(prices, EMA_signal_buy == 1), y = ~EMA_20, name = "Buy Signal", marker = list(color = "#008000", size = 7)) %>%
    add_markers(data = subset(prices, EMA_signal_sell == 1), y = ~EMA_20, name = "Sell Signal", marker = list(color = "#FF0B04", size = 7)) %>%
    layout(title = "Price Movement with EMAs and Signals (Close)",
           xaxis = list(rangeslider = list(visible = F)),
           yaxis = list(title = "Close Price", autorange = TRUE),
           showlegend = TRUE)
  plot_EMA  
}

Plot_Stochastic <- function(){
  plot_Stoch <- plot_ly(prices, x = ~date) %>%
    add_lines(y = ~Stochastic_fK, name = "Stochastic fK", line = list(color = "#0000FF80", width = 1)) %>%
    add_lines(y = ~Stochastic_fD, name = "Stochastic fD", line = list(color = "#FF000080", width = 1)) %>%
    add_ribbons(data = subset(prices, Stochastic_fK < 0.2),
                x = ~date, ymin=0, ymax=0.2, name = "Oversold", fillcolor = "rgba(0, 255, 0, 0.15)",line = list(color = "transparent")) %>%
    add_ribbons(data = subset(prices, Stochastic_fK > 0.8),
                x = ~date, ymin=0.8, ymax=1, name = "Overbought", fillcolor = "rgba(255, 0, 0, 0.15)",line = list(color = "transparent")) %>%
    add_markers(data = subset(prices, Stochastic_signal_buy == 1), y = ~Stochastic_fK, name = "Buy Signal", marker = list(color = "#008000", size = 7)) %>%
    add_markers(data = subset(prices, Stochastic_signal_sell == 1), y = ~Stochastic_fK, name = "Sell Signal", marker = list(color = "#FF0B04", size = 7)) %>%
    layout(title = "Stochastic Oscillator with Signals (Close)",
           xaxis = list(title = "Date"),
           yaxis = list(title = "Stochastic"),
           showlegend = TRUE)
  
  plot_prices <- Plot_Prices()
  plot_prices <- plot_prices %>%
    add_markers(data = subset(prices, Stochastic_signal_buy == 1), y = ~close, name = "Buy Signal", marker = list(color = "#008000", size = 7), showlegend = FALSE) %>%
    add_markers(data = subset(prices, Stochastic_signal_sell == 1), y = ~close, name = "Sell Signal", marker = list(color = "#FF0B04", size = 7), showlegend = FALSE)
  
  combined_plot <- subplot(plot_prices, plot_Stoch, nrows = 2, shareX = TRUE, heights = c(0.6, 0.4))
  return(combined_plot)
}

Plot_ATR <- function(){
  plot_ATR <- plot_ly(prices, x = ~date, name = "Candles", type = "candlestick", open = ~open, close = ~close, high = ~high, low = ~low, increasing = list(line = list(color = "#96D875")), decreasing = list(line = list(color = "#FA9693"))) %>%
    add_lines(y = ~SMA_20, name = "SMA 20", line = list(color = "#FF000080", width = 1)) %>%
    add_lines(y = ~ATRUpper_Band, name = "ATR Upper", line = list(color = "#0000FF80", width = 1)) %>%
    add_lines(y = ~ATRLower_Band, name = "ATR Lower", line = list(color = "#0000FF80", width = 1)) %>%
    add_ribbons(ymin = ~ATRLower_Band, ymax = ~ATRUpper_Band, fillcolor = "#0000FF20", line = list(color = "transparent"), name = "ATR Range") %>%
    add_markers(data = subset(prices, ATR_signal_buy == 1), y = ~low, name = "Buy Signal", marker = list(color = "#008000", size = 7)) %>%
    add_markers(data = subset(prices, ATR_signal_sell == 1), y = ~high, name = "Sell Signal", marker = list(color = "#FF0B04", size = 7)) %>%
    layout(title = "ATR with Signals (Close)",
           xaxis = list(rangeslider = list(visible = F)),
           yaxis = list(title = "Close Price",autorange = TRUE),
           showlegend = TRUE)
  plot_ATR
}

Plot_CCI <- function(){
  plot_CCI <- plot_ly(prices, x = ~date) %>%
    add_lines(y = ~CCI, name = "CCI", type = "scatter", mode = "lines", line = list(color = "#0000FF80", width = 1)) %>%
    add_markers(data = subset(prices, CCI_signal_buy == 1), x = ~date, y = ~CCI, name = "Buy Signal", marker = list(color = "#008000", size = 7)) %>%
    add_markers(data = subset(prices, CCI_signal_sell == 1), x = ~date, y = ~CCI, name = "Sell Signal", marker = list(color = "#FF0B04", size = 7)) %>%
    add_trace(data = subset(prices, !is.na(CCI)), x = ~date, y = ~rep(100, nrow(subset(prices, !is.na(CCI)))), type = "scatter", mode = "lines", line = list(color = "red", dash = "dash"), name = "Overbought (100)") %>%
    add_trace(data = subset(prices, !is.na(CCI)), x = ~date, y = ~rep(-100, nrow(subset(prices, !is.na(CCI)))), type = "scatter", mode = "lines", line = list(color = "#008000", dash = "dash"), name = "Oversold (-100)") %>%
    add_ribbons(ymin=-100, ymax=100, fillcolor = "#0000FF20",line = list(color = "transparent"), name = "CCI Range") %>%
    layout(title = "CCI with Signals",
           xaxis = list(title = "Date"),
           yaxis = list(title = "CCI"),
           showlegend = TRUE)
  
  plot_prices <- Plot_Prices()
  plot_prices <- plot_prices %>%
    add_markers(data = subset(prices, CCI_signal_buy == 1), y = ~close, name = "Buy Signal", marker = list(color = "#008000", size = 7), showlegend = FALSE) %>%
    add_markers(data = subset(prices, CCI_signal_sell == 1), y = ~close, name = "Sell Signal", marker = list(color = "#FF0B04", size = 7), showlegend = FALSE)
  
  combined_plot <- subplot(plot_prices, plot_CCI, nrows = 2, shareX = TRUE, heights = c(0.6, 0.4))
  
  return(combined_plot)
}

Plot_MACD <- function(){
  colors <- ifelse(prices$macd_histogram[(which(!is.na(prices$macd_histogram))[1]):length(prices$macd_histogram)] > 0, "#00800070", "#FF0B0470")

  plot_MACD <- plot_ly(prices, x = ~date) %>%
    add_lines(y = ~MACD, name = "MACD line", line = list(color = "#0000FF80", width = 1)) %>%
    add_lines(y = ~signal_line, name = "Signal line", line = list(color = "orange", width = 1)) %>%
    add_bars(y = ~macd_histogram, name = "MACD Histogram", marker = list(color = colors)) %>%
    add_markers(data = subset(prices, MACD_signal_buy == 1), x = ~date, y = ~MACD, name = "Buy Signal", marker = list(color = "#008000", size = 7)) %>%
    add_markers(data = subset(prices, MACD_signal_sell == 1), x = ~date, y = ~MACD, name = "Sell Signal", marker = list(color = "#FF0B04", size = 7)) %>%
    layout(title = "MACD with Signals",
           xaxis = list(title = "Date"),
           yaxis = list(title = "MACD"),
           showlegend = TRUE)
  
  plot_prices <- Plot_Prices()
  plot_prices <- plot_prices %>%
    add_markers(data = subset(prices, MACD_signal_buy == 1), y = ~close, name = "Buy Signal", marker = list(color = "#008000", size = 7), showlegend = FALSE) %>%
    add_markers(data = subset(prices, MACD_signal_sell == 1), y = ~close, name = "Sell Signal", marker = list(color = "#FF0B04", size = 7), showlegend = FALSE)
  
  combined_plot <- subplot(plot_prices, plot_MACD, nrows = 2, shareX = TRUE, heights = c(0.6, 0.4))
  
  return(combined_plot)
}

Plot_BBand <- function(){
  plot_Bollinger <- plot_ly(prices, x = ~date, name = "Candles", type = "candlestick", open = ~open, close = ~close, high = ~high, low = ~low, increasing = list(line = list(color = "#96D875")), decreasing = list(line = list(color = "#FA9693"))) %>%
    add_lines(y = ~BB_UpperBand, name = "Upper Band", line = list(color = "#0000FF80", width = 1)) %>%
    add_lines(y = ~BB_LowerBand, name = "Lower Band", line = list(color = "#0000FF80", width = 1)) %>%
    add_ribbons(ymin = ~BB_LowerBand, ymax = ~BB_UpperBand, fillcolor = "#0000FF20", line = list(color = "transparent"), name = "BBand Range") %>%
    add_markers(data = subset(prices, BBand_signal_buy == 1), x = ~date, y = ~low, name = "Buy Signal", marker = list(color = "#008000", size = 7)) %>%
    add_markers(data = subset(prices, BBand_signal_sell == 1), x = ~date, y = ~high, name = "Sell Signal", marker = list(color = "#FF0B04", size = 7)) %>%
    layout(title = "Bollinger Bands with Signals (Close)",
           xaxis = list(rangeslider = list(visible = F)),
           yaxis = list(title = "Close Price",autorange = TRUE),
           showlegend = TRUE)
  plot_Bollinger  
}

Plot_RSI <- function(){
  plot_RSI <- plot_ly(prices, x = ~date) %>%
    add_lines(y = ~RSI, name = "RSI", line = list(color = "#0000FF80", width = 1)) %>%
    add_trace(type = "scatter", x = ~date, y = rep(70, nrow(prices)), mode = "lines", line = list(dash = "dash", color = "red"), name = "Overbought Threshold (70)") %>%
    add_trace(type = "scatter", x = ~date, y = rep(30, nrow(prices)), mode = "lines", line = list(dash = "dash", color = "#008000"), name = "Oversold Threshold (30)") %>%
    add_markers(data = subset(prices, RSI_signal_buy == 1), x = ~date, y = ~RSI, name = "Buy Signal", marker = list(color = "#008000", size = 7)) %>%
    add_markers(data = subset(prices, RSI_signal_sell == 1), x = ~date, y = ~RSI, name = "Sell Signal", marker = list(color = "#FF0B04", size = 7)) %>%
    layout(title = "RSI with Signals",
           xaxis = list(title = "Date"),
           yaxis = list(title = "RSI"),
           showlegend = TRUE)

  plot_prices <- Plot_Prices()
  plot_prices <- plot_prices %>%
    add_markers(data = subset(prices, RSI_signal_buy == 1), y = ~close, name = "Buy Signal", marker = list(color = "#008000", size = 7), showlegend = FALSE) %>%
    add_markers(data = subset(prices, RSI_signal_sell == 1), y = ~close, name = "Sell Signal", marker = list(color = "#FF0B04", size = 7), showlegend = FALSE)
  
  combined_plot <- subplot(plot_prices, plot_RSI, nrows = 2, shareX = TRUE, heights = c(0.6, 0.4))
  
  return(combined_plot)
}


