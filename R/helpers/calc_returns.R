calc_returns <- function(data_filtered, prop_table) {
  symbols <- prop_table |> pull(symbol)
  
  data_filtered |>
    filter(symbol %in% symbols) |>
    arrange(symbol, date) |>
    group_by(symbol) |>
    tq_mutate(
      select = adjusted,
      mutate_fun = periodReturn,
      period = "daily",
      col_rename = "daily_return"
    ) |>
    ungroup() |>
    left_join(prop_table, by = "symbol")
}
