calc_portfolio_returns <- function(returns_data, prop_table) {
  returns_data |>
    select(date, symbol, daily_return) |>
    tq_portfolio(
      assets_col = symbol,
      returns_col = daily_return,
      weights = prop_table |> select(symbol, weight = prop),
      rebalance_on = "days",
      geometric = TRUE
    )
}
