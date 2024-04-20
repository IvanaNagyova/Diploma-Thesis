################# Generate orders #################
create_orders_df <- function(){
  orders <- data.frame(
    strategy = character(),
    indicator = character(),
    entry_date = as.Date(character()),
    exit_date = as.Date(character()),
    entry_price = numeric(),
    stop_loss_price = numeric(),
    take_profit_price = numeric(),
    exit_price = numeric(),
    stringsAsFactors = FALSE
  )
  return(orders)
}

############ Add and generate orders ####################
add_order <- function(strategy, indicator, entry_date, exit_date, entry_price, stop_loss_price, take_profit_price, exit_price) {
  order <- data.frame(
    strategy = strategy,
    indicator = indicator,
    entry_date = as.Date(entry_date),
    exit_date = as.Date(exit_date),
    entry_price = entry_price,
    stop_loss_price = stop_loss_price,
    take_profit_price = take_profit_price,
    exit_price = exit_price
  )
  orders <<- rbind(orders, order)
}

generate_orders <- function(indicators, parameters){
  orders <<- create_orders_df()
  for (i in 2:nrow(prices)){
    for(j in 1:length(indicators)){
      indicator <- indicators[j]
      indicator_buy <- paste0(indicator,"_signal_buy")
      buy_signal <- prices[[indicator_buy]][i] == 1 
      
      if(buy_signal){
        entry_price <- prices$close[i]
        stop_loss_price <- entry_price - (prices$atr[i] * parameters$stop_loss_multiplier)
        take_profit_price <- entry_price + ((entry_price - stop_loss_price) * parameters$take_profit_multiplier)
        
        
        ## create buy order for all 3 strategies
        add_order(parameters$strategy$signal_only_strategy, indicator, prices$date[i], NA, entry_price, stop_loss_price, take_profit_price, NA)
        add_order(parameters$strategy$risk_managed_strategy, indicator, prices$date[i], NA, entry_price, stop_loss_price, take_profit_price, NA)
        add_order(parameters$strategy$profit_managed_strategy, indicator, prices$date[i], NA, entry_price, stop_loss_price, take_profit_price, NA)
      } 
    }
  }
  print("Orders generated")
}

######################## Execute Orders #############################
execute_orders <- function(orders){
  print("Started executing orders...")
  not_opened_yet <- orders[order(orders$entry_date), ] 
  already_opened <- structure(not_opened_yet[0, ], class = "data.frame")
  already_closed <- structure(not_opened_yet[0, ], class = "data.frame")

  for(i in 2:nrow(prices)){
    orders_to_close <- c()
    ## checking if we can close any trade
    if(nrow(already_opened)>0){
      for(k in 1:nrow(already_opened)){
        indicator_sell <- paste0(already_opened$indicator[k],"_signal_sell")
        sell_signal <- prices[[indicator_sell]][i] == 1 
        stop_loss_triggered <- prices$low[i] < already_opened$stop_loss_price[k]
        take_profit_triggered <- prices$high[i] > already_opened$take_profit_price[k]
        price_gapped_down <- prices$open[i] < already_opened$stop_loss_price[k]
        price_gapped_up <- prices$open[i] > already_opened$take_profit_price[k]
        if(already_opened$strategy[k] == parameters$strategy$signal_only_strategy){
          if (sell_signal){
            already_opened$exit_price[k] <- prices$close[i]
            already_opened$exit_date[k] <- prices$date[i]
            orders_to_close <- rbind(orders_to_close,k)
          } 
        } else if(already_opened$strategy[k] == parameters$strategy$risk_managed_strategy){
          if(stop_loss_triggered){
            if(price_gapped_down) {
              already_opened$exit_price[k] <- prices$open[i]
            } else {
              already_opened$exit_price[k] <- already_opened$stop_loss_price[k]
            }
            already_opened$exit_date[k] <- prices$date[i]
            orders_to_close <- rbind(orders_to_close,k)
          } else if (sell_signal){
            already_opened$exit_price[k] <- prices$close[i]
            already_opened$exit_date[k] <- prices$date[i]
            orders_to_close <- rbind(orders_to_close,k)
          } 
        } else if(already_opened$strategy[k]==parameters$strategy$profit_managed_strategy){
          if(stop_loss_triggered){
            if(price_gapped_down) {
              already_opened$exit_price[k] <- prices$open[i]
            } else {
              already_opened$exit_price[k] <- already_opened$stop_loss_price[k]
            }
            already_opened$exit_date[k] <- prices$date[i]
            orders_to_close <- rbind(orders_to_close,k)
          } else if (take_profit_triggered){
            if(price_gapped_up){
              already_opened$exit_price[k] <- prices$open[i]
            } else {
              already_opened$exit_price[k] <- already_opened$take_profit_price[k]
            }
            already_opened$exit_date[k] <- prices$date[i]
            orders_to_close <- rbind(orders_to_close,k)
          } 
        } else {
          stop(paste("Unknown strategy: [", already_opened$strategy[k],"]. Please provide valid strategy"))
        }
      }
      if(length(orders_to_close)>0){
        closing_orders <- already_opened[orders_to_close, ]
        already_opened <- already_opened[-(orders_to_close), , drop = FALSE]
        already_closed <- rbind(already_closed, closing_orders)
      }
    }
    
    ## checking if we can open a trade (assuming only one order open at a time)
    if(nrow(not_opened_yet)>0){
      max_index_to_open <- 0
      for(k in 1:nrow(not_opened_yet)){
        if(prices$date[i] > not_opened_yet$entry_date[k]){
          max_index_to_open <- k
        } else {
          break
        }
      }
      if(max_index_to_open > 0){
        opening_orders <- not_opened_yet[1:max_index_to_open, ]
        not_opened_yet <- not_opened_yet[-(1:max_index_to_open), , drop = FALSE]
        already_opened <- rbind(already_opened, opening_orders)
      }
    }
  }
  cat("Finished executing ", nrow(already_closed)," orders...\n")
  return(already_closed)
}


