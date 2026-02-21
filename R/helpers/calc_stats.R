calc_stats <- function(dtd_data, prop_table) {
  stock_stats <- dtd_data |>
    group_by(symbol) |>
    summarize(
      total_return = last(diff_dtd) * 100,
      volatility = sd(diff(adjusted) / lag(adjusted), na.rm = TRUE) *
        sqrt(252) *
        100,
      max_drawdown = calc_max_drawdown(adjusted) * 100,
      .groups = "drop"
    ) |>
    left_join(prop_table, by = "symbol") |>
    mutate(
      weighted_return = total_return * prop,
      weighted_volatility = volatility * prop,
      weighted_drawdown = max_drawdown * prop
    )
  
  portfolio_return <- sum(stock_stats$weighted_return)
  portfolio_volatility <- sum(stock_stats$weighted_volatility)
  portfolio_drawdown <- sum(stock_stats$weighted_drawdown)
  
  bind_rows(
    stock_stats |> select(symbol, total_return, volatility, max_drawdown),
    tibble(
      symbol = "PORTFOLIO",
      total_return = portfolio_return,
      volatility = portfolio_volatility,
      max_drawdown = portfolio_drawdown
    )
  ) |>
    mutate(
      total_return = round(total_return, 2),
      volatility = round(volatility, 2),
      max_drawdown = round(max_drawdown, 2)
    )
}
