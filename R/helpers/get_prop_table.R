get_prop_table <- function(ticker_subset, props) {
  tibble(
    symbol = ticker_subset |> pull(symbol),
    prop = props / 100
  )
}
