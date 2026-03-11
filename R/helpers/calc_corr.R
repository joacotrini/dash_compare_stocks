calc_corr <- function(dtd_with_portfolio) {
  corr_matrix <- dtd_with_portfolio |>
    pivot_wider(names_from = symbol, values_from = daily_return) |>
    select(-date) |>
    correlate() |>
    mutate(across(where(is.numeric), ~ round(.x, 3)))

  corr_matrix |>
    select(term, PORTFOLIO) |>
    filter(term != "PORTFOLIO") |>
    rename(
      Stock = term,
      `Corr. with Portfolio` = PORTFOLIO
    ) |>
    arrange(desc(`Corr. with Portfolio`))
}
