calc_max_drawdown <- function(portfolio_returns) {
  dd <- portfolio_returns |>
    tq_performance(
      Ra = portfolio.returns,
      performance_fun = table.Drawdowns
    )

  if (nrow(dd) > 0) {
    dd |>
      head(1) |>
      transmute(
        Start     = as.character(From),
        Depth     = paste0(round(Depth * 100, 2), "%"),
        Recovered = as.character(To),
        ToT       = paste0(Length, " days")
      )
  } else {
    tibble(
      Start     = NA_character_,
      Depth     = "0%",
      Recovered = NA_character_,
      ToT       = "0 days"
    )
  }
}
