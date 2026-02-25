calc_stats <- function(dtd_data, prop_table) {
  stock_stats <- dtd_data |>
    group_by(symbol) |>
    summarize(
      total_return = last(diff_dtd) * 100,
      sd = sd(diff_dtd, na.rm = TRUE) * 100
    ) |>
    left_join(prop_table, by = "symbol") |>
    mutate(
      weighted_return = total_return * prop,
      weighted_sd = sd * prop
    )

  portfolio_return <- sum(stock_stats$weighted_return)
  portfolio_sd <- sum(stock_stats$weighted_sd)

  bind_rows(
    stock_stats |> select(symbol, total_return, sd),
    tibble(
      symbol = "PORTFOLIO",
      total_return = portfolio_return,
      sd = portfolio_sd
    )
  ) |>
    mutate(
      total_return = round(total_return, 2),
      sd = round(sd, 2)
    )
}
