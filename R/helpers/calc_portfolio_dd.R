calc_max_drawdown <- function(portfolio_returns) {
  dd <- portfolio_returns |>
    tq_performance(
      returns_col = portfolio.returns,
      performance_fun = table.Drawdowns
    )
  
  if (nrow(dd) > 0) {
    dd |>
      head(1) |>
      select(Start, Depth, Recovered, ToT) |>
      mutate(
        Depth = paste0(round(Depth * 100, 2), "%"),
        ToT = paste0(round(ToT, 0), " days")
      )
  } else {
    tibble(
      Start = NA,
      Depth = "0%",
      Recovered = NA,
      ToT = "0 days"
    )
  }
}
