get_ticker_subset <- function(tickers, stock_type) {
  tickers |>
    filter(stock_type == {{ stock_type }})
}
