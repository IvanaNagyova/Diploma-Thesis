################# Data pre-processing and downloading libraries ##############
prepared_data <- function() {
  packages <- c("tidyquant","plotly", "dplyr")
  
  lapply(packages, function(package) {
    if (!(package %in% installed.packages()))
      install.packages(package)
    library(package, character.only = TRUE)
  })
  start_date <- as.Date("2000-01-01")
  end_date <- as.Date("2024-01-01")
  
  prices <- tq_get("SPY", get="stock.prices", from = start_date, to = end_date)
  
  return(prices)
}