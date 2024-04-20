################# Initialisation ########################
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls()) # clear environment
cat(rep("\n", 500)) # clear console

source("functions/initialisation.R")
source("functions/technical_analysis.R")
source("functions/backtesting.R")
source("functions/analysis_of_results.R")

################# Data preparation #####################
prices <- prepared_data()
prices_xts <- xts(prices[, c("open", "high", "low", "close")], order.by = as.Date(prices$date))
prices$buy_hold_returns <- log(prices$close/lag(prices$close))
prices$cumulative_buy_hold_returns <- c(NA, cumsum(prices$buy_hold_returns[-1]))

################# Parameters ########################### 
parameters <- list(
  stop_loss_multiplier = 2,  
  take_profit_multiplier = 2, 
  atr_period = 14,
  atr_multiplier = 2,
  strategy = list(
    "signal_only_strategy" = "signal_only_strategy",
    "risk_managed_strategy" = "risk_managed_strategy",
    "profit_managed_strategy" = "profit_managed_strategy"
  )
)

################# Creating indicators ##################
add_indicators(prices_xts, parameters)

################# Signal detection #####################
add_signals()

################# Plotting signals #####################
Plot_SMA()
Plot_EMA()
Plot_Stochastic()
Plot_ATR()
Plot_CCI()
Plot_MACD()
Plot_BBand()
Plot_RSI()


################# Backtesting ##########################
indicators <- gsub("_.*", "", grep("_signal_buy", colnames(prices), value = TRUE))
generate_orders(indicators, parameters)
closed_orders <- execute_orders(orders)

################# Analysis of results ##################
portfolio_performance <- simulate_portfolio(closed_orders)

## plot executed trades and strategy
portfolio_plot(prices, portfolio_trades, indicators[5], parameters$strategy[3])

## add active returns and performance metrics and plot all portfolios
portfolio_trades <- add_active_return(portfolio_trades, prices)
portfolio_performance <- add_performance_metrics(portfolio_performance, portfolio_trades)
final_plot(portfolio_trades, prices)

## determine better strategies and plot them
better_strategies <- extract_better_strategies(portfolio_performance)
plot_better_strategies(better_strategies, portfolio_trades, prices)



