calc_performance <- function(portfolio_returns) {
  portfolio_returns |>
    tq_performance(
      Ra = portfolio.returns,
      performance_fun = table.AnnualizedReturns
    ) |>
    pivot_longer(
      everything(),
      names_to = "metric",
      values_to = "value"
    ) |>
    mutate(
      value = case_when(
        metric == "Annualized.Return" ~ paste0(round(value * 100, 2), "%"),
        metric == "Annualized.Volatility" ~ paste0(round(value * 100, 2), "%"),
        metric == "SharpeRatio" ~ as.character(round(value, 2)),
        TRUE ~ as.character(value)
      )
    )
}
