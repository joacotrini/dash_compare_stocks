calc_max_drawdown <- function(prices) {
  cummax_val <- cummax(prices)
  drawdowns <- (prices - cummax_val) / cummax_val
  max_drawdown <- max(drawdowns)
  return(max_drawdown)
}
