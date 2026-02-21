calc_portfolio <- function(dtd_data) {
  dtd_data |>
    summarize(
      diff_dtd_portfolio = sum(diff_dtd_pond),
      .by = date
    ) |>
    mutate(
      symbol = "PORTFOLIO",
      diff_dtd = diff_dtd_portfolio
    ) |>
    select(-diff_dtd_portfolio)
}
