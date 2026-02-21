calc_dtd <- function(data_filtered, prop_table) {
  symbols <- prop_table |> pull(symbol)
  
  data_filtered |>
    filter(symbol %in% symbols) |>
    arrange(date) |>
    mutate(
      first_price = first(adjusted),
      .by = symbol
    ) |>
    left_join(prop_table, by = "symbol") |>
    mutate(
      diff_dtd = (adjusted - first_price) / first_price,
      diff_dtd_pond = diff_dtd * prop
    )
}
