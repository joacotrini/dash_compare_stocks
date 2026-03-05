calc_max_drawdown <- function(portfolio_returns) {
  dd <- portfolio_returns |>
    tq_performance(
      Ra = portfolio.returns,
      performance_fun = maxDrawdown
    ) |>
    rename(maxDrawdown = maxDrawdown.1)

  if (nrow(dd) > 0) {
    # maxDrawdown returns a single value - just format it as percentage
    tibble(
      MaxDrawdown = paste0(round(dd$maxDrawdown[[1]] * 100, 2), "%")
    )
  } else {
    tibble(
      MaxDrawdown = "0%"
    )
  }
}
