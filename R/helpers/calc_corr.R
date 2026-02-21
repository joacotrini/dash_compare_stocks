calc_corr <- function(dtd_with_portfolio) {
  dtd_with_portfolio |>
    pivot_wider(names_from = symbol, values_from = diff_dtd) |>
    select(-date) |>
    correlate() |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))
}
