################# Create portfolio trades #################
create_portfolio_trades <- function(){
  portfolio_trades <- data.frame(
    entry_date = as.Date(character()),
    closed_date = as.Date(character()),
    strategy = character(),
    indicator = character(),
    entry_price = numeric(),
    exit_price = numeric(),
    stop_loss_price = numeric(),
    take_profit_price = numeric(),
    current_trade_pnl = numeric(),
    cumulative_pnl = numeric(),
    stringsAsFactors = FALSE
  )
  return(portfolio_trades)
}

add_portfolio_trade <- function(entry_date, closed_date, strategy, indicator, entry_price, exit_price, stop_loss_price, take_profit_price, current_trade_pnl, cumulative_pnl) {
  portfolio_trade <- data.frame(
    entry_date = entry_date,
    closed_date = as.Date(closed_date),
    strategy = strategy,
    indicator = indicator,
    entry_price = entry_price,
    exit_price = exit_price,
    stop_loss_price = stop_loss_price,
    take_profit_price = take_profit_price,
    current_trade_pnl = current_trade_pnl,
    cumulative_pnl = cumulative_pnl
  )
  portfolio_trades <<- rbind(portfolio_trades, portfolio_trade)
}

################# Simulate portfolio #################
simulate_portfolio <- function(closed_orders){
  portfolio_trades <<- create_portfolio_trades()
  closed_orders <- closed_orders[order(closed_orders$entry_date), ]
  portfolio_performance <- expand.grid(unique(closed_orders$strategy), unique(closed_orders$indicator))
  colnames(portfolio_performance) <- c("strategy", "indicator")
  portfolio_performance$last_close_date <- as.Date(NA)
  portfolio_performance$cumulative_pnl <- 0
  portfolio_performance$active_returns <- 0
  
  for(i in 1:nrow(closed_orders)){
    strategy <- closed_orders$strategy[i]
    indicator <- closed_orders$indicator[i]
    new_open_date <- closed_orders$entry_date[i]
    
    related_strategy <- portfolio_performance[portfolio_performance$strategy == strategy & portfolio_performance$indicator == indicator, ]
    related_strategy_index <- which(portfolio_performance$strategy == strategy & portfolio_performance$indicator == indicator)
    currently_closed_date <- related_strategy$last_close_date
    
    # if the open date of a new order is bigger than the exit date of previous order -> open new trade
    trade_is_not_opened <- is.na(currently_closed_date) || (new_open_date > currently_closed_date)
    if(trade_is_not_opened) { # we open this trade
      trade_pnl <- compute_pnl(closed_orders$exit_price[i],closed_orders$entry_price[i])
      new_cumulative_pnl <- (related_strategy$cumulative_pnl + 1) * (1 + trade_pnl) - 1
      if(related_strategy$cumulative_pnl > -1){
        add_portfolio_trade(closed_orders$entry_date[i], closed_orders$exit_date[i], strategy, indicator, closed_orders$entry_price[i], closed_orders$exit_price[i], closed_orders$stop_loss_price[i], closed_orders$take_profit_price[i], trade_pnl, new_cumulative_pnl )
        portfolio_performance$cumulative_pnl[related_strategy_index] <- new_cumulative_pnl
        portfolio_performance$last_close_date[related_strategy_index] <- closed_orders$exit_date[i]
        
        price_index <- which(prices$date == closed_orders$exit_date[i])
        if (length(price_index) > 0) {
          cumulative_buy_hold_return <- prices$cumulative_buy_hold_returns[price_index]
          active_return <- new_cumulative_pnl - cumulative_buy_hold_return
          portfolio_performance$active_returns[related_strategy_index] <- active_return
        }
      } 
    } 
  }
  buy_hold_row <- data.frame(strategy = "buy_and_hold", indicator = NA, last_close_date = max(prices$date), cumulative_pnl = sum(prices$buy_hold_returns, na.rm = TRUE), active_returns = 0)
  colnames(buy_hold_row) <- colnames(portfolio_performance)
  portfolio_performance <- rbind(portfolio_performance, buy_hold_row)
  
  print("Finished portfolio simulation")
  return(portfolio_performance)
}

########################### Compute PnL #################
compute_pnl <- function(exit_price,entry_price){
  return(log(exit_price / entry_price))
}


################# Plot portfolio #################
portfolio_plot <- function(prices, portfolio_trades, indicator, strategy) {
  plot_prices <- plot_ly(prices, x = ~date, name = "Candles", type = "candlestick", open = ~open, close = ~close, high = ~high, low = ~low, increasing = list(line = list(color = "#96D875")), decreasing = list(line = list(color = "#FA9693"))) 
  filtered_trades <- portfolio_trades[portfolio_trades$indicator == indicator & portfolio_trades$strategy == strategy, ]
  
  for (i in 1:nrow(filtered_trades)) {
    entry_date <- filtered_trades[i, "entry_date"]
    entry_price <- filtered_trades[i, "entry_price"]
    closed_date <- filtered_trades[i, "closed_date"] 
    exit_price <- filtered_trades[i, "exit_price"]  
    pnl<- round(filtered_trades[i, "current_trade_pnl"]*100,2)
    midpoint_price <- (entry_price + exit_price) / 2
    midpoint_date <- as.Date((as.numeric(entry_date) + as.numeric(closed_date)) / 2)
    pnl_annotation <- paste0("PNL: ", pnl, "%")

    
    plot_prices <- plot_prices %>%
      add_markers(x = entry_date, y = entry_price, marker = list(size = 7, color = "#008000")) %>%
      add_markers(x = closed_date, y = exit_price, marker = list(size = 7, color = "#FF0000"))
    
    if (strategy == "profit_managed_strategy") {
      stop_loss_price <- filtered_trades[i, "stop_loss_price"]
      take_profit_price <- filtered_trades[i, "take_profit_price"]
      
      if (pnl <= 0) {
        tp_name <- "Take Profit"
        sl_name <- paste("Stop Loss - PNL", pnl, "%")
      } else if (pnl > 0) {
        sl_name <- "Stop Loss"
        tp_name <- paste("Take Profit - PNL", pnl, "%")
      }
      
      plot_prices <- add_trace(plot_prices, type = "scatter", mode = "lines", name = sl_name,
                               x = c(entry_date, closed_date, closed_date, entry_date, entry_date),
                               y = c(entry_price, entry_price, stop_loss_price, stop_loss_price, entry_price),
                               fill = "toself", fillcolor = "rgba(255, 0, 0, 0.5)", line = list(color = "red"), opacity = 0.5)
      
      plot_prices <- add_trace(plot_prices, type = "scatter", mode = "lines", name = tp_name,
                               x = c(entry_date, closed_date, closed_date, entry_date, entry_date),
                               y = c(entry_price, entry_price, take_profit_price, take_profit_price, entry_price),
                               fill = "toself", fillcolor = "rgba(0, 255, 0, 0.5)", line = list(color = "green"), opacity = 0.5)
      
    } else if (strategy == "risk_managed_strategy") {
      stop_loss_price <- filtered_trades[i, "stop_loss_price"]
      
      if (exit_price == stop_loss_price) {
        sl_name <- paste("Stop Loss - PNL", pnl, "%")
      } else if (exit_price != stop_loss_price) {
        sl_name <- "Stop Loss"
      }
      
      plot_prices <- add_trace(plot_prices, type = "scatter", mode = "lines", name = sl_name,
                               x = c(entry_date, closed_date, closed_date, entry_date, entry_date),
                               y = c(entry_price, entry_price, stop_loss_price, stop_loss_price, entry_price),
                               fill = "toself", fillcolor = "rgba(255, 0, 0, 0.5)", line = list(color = "red"), opacity = 0.5)
      
      if (exit_price != stop_loss_price) {
        plot_prices <- add_trace(plot_prices, type = "scatter", mode = "lines", name = pnl_annotation,
                                 x = c(entry_date, closed_date, closed_date, entry_date, entry_date),
                                 y = c(entry_price, entry_price, exit_price, exit_price, entry_price),
                                 fill = "toself", fillcolor = "rgba(169,169,169,0.5)", line = list(color = "gray"), opacity = 0.5)
      }
    } else if (strategy == "signal_only_strategy") {
      plot_prices <- add_trace(plot_prices, type = "scatter", mode = "lines", name = pnl_annotation,
                               x = c(entry_date, closed_date, closed_date, entry_date, entry_date),
                               y = c(entry_price, entry_price, exit_price, exit_price, entry_price),
                               fill = "toself", fillcolor = "rgba(169,169,169,0.5)", line = list(color = "gray"), opacity = 0.5)
    }
  }
  
  plot_prices <- layout(plot_prices, title = paste("Executed trades for", indicator, "-", strategy),
                        xaxis = list(title = "Date", rangeslider = list(visible = FALSE), type = "date"),
                        yaxis = list(title = "Price"),
                        showlegend = FALSE)
  
  return(plot_prices)
}


################# Add active returns #################
add_active_return <- function(portfolio_trades, prices) {
  active_returns <- numeric(nrow(portfolio_trades))
  
  for (i in 1:nrow(portfolio_trades)) {
    closed_date <- portfolio_trades$closed_date[i]
    cumulative_pnl <- portfolio_trades$cumulative_pnl[i]
    price_index <- which(prices$date == closed_date)
    if (length(price_index) > 0) {
      cumulative_buy_hold_return <- prices$cumulative_buy_hold_returns[price_index]
      active_returns[i] <- cumulative_pnl - cumulative_buy_hold_return
    } else {
      active_returns[i] <- NA
    }
  }
  portfolio_trades$active_returns <- active_returns
  return(portfolio_trades)
}

final_plot <- function(portfolio_trades, prices) {
  p <- plot_ly(data = prices, x = ~date, y = ~cumulative_buy_hold_returns, type = 'scatter', mode = 'lines', name = I('Buy and Hold Returns'), line = list(width = 1), color = I("#404040"))  
  p <- add_trace(p, data = portfolio_trades, x = ~closed_date, y = ~cumulative_pnl, color = ~indicator, linetype = ~strategy, type = 'scatter', mode = 'lines', name = ~paste(indicator, strategy, sep = " "))
  p <- layout(p, title = 'Portfolio performance', xaxis = list(title = 'Closed Date'), yaxis = list(title = 'Cumulative PNL'))
  
  return(p)
}

################# Performance Metrics #################
add_performance_metrics <- function(portfolio_performance, portfolio_trades) {
  for (i in 1:nrow(portfolio_performance)) {
    strategy <- portfolio_performance$strategy[i]
    indicator <- portfolio_performance$indicator[i]
    trades_subset <- portfolio_trades[portfolio_trades$strategy == strategy & portfolio_trades$indicator == indicator, ]
    active_returns <- trades_subset$active_returns
    information_ratio <- mean(active_returns, na.rm = TRUE) / sd(active_returns, na.rm = TRUE)
    portfolio_performance$information_ratio[i] <- information_ratio
  }
  
  return(portfolio_performance)
}

extract_better_strategies <- function(portfolio_performance) {
  better_strategies <- subset(portfolio_performance, cumulative_pnl > cumulative_pnl[which(portfolio_performance$strategy == "buy_and_hold" & is.na(portfolio_performance$indicator))])
  return(better_strategies)
}

plot_better_strategies <- function(better_strategies, portfolio_trades, prices) {
  p <- plot_ly(data = prices, x = ~date, y = ~cumulative_buy_hold_returns, type = 'scatter', mode = 'lines', name = I('Buy and Hold Returns'), line = list(width = 1), color = I("#404040"))  
  for (i in 1:nrow(better_strategies)) {
    strategy <- better_strategies$strategy[i]
    indicator <- better_strategies$indicator[i]
    trades_subset <- portfolio_trades[portfolio_trades$strategy == strategy & portfolio_trades$indicator == indicator, ]
    p <- add_trace(p, data = trades_subset, x = ~closed_date, y = ~cumulative_pnl, color = ~indicator, linetype = ~strategy, type = 'scatter', mode = 'lines', name = paste(indicator, strategy, sep = " "))
  }
  p <- layout(p, title = 'Portfolio performance', xaxis = list(title = 'Closed Date'), yaxis = list(title = 'Cumulative PNL'))
  return(p)
}

