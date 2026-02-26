# Tests for calc_performance.R

# Helper: create mock portfolio returns data
create_mock_portfolio_returns <- function() {
  tibble(
    date = as.Date("2024-01-01") + 0:99,
    portfolio.returns = rnorm(100, 0.0005, 0.01)  # ~12% annual return, ~16% vol
  )
}

test_that("calc_performance returns 3 metrics", {
  portfolio_returns <- create_mock_portfolio_returns()
  
  result <- calc_performance(portfolio_returns)
  
  expect_equal(nrow(result), 3)
})

test_that("calc_performance has expected metric names", {
  portfolio_returns <- create_mock_portfolio_returns()
  
  result <- calc_performance(portfolio_returns)
  
  expect_true("Annualized.Return" %in% result$metric)
  expect_true("Annualized.Volatility" %in% result$metric)
  expect_true("SharpeRatio" %in% result$metric)
})

test_that("calc_performance values are character format", {
  portfolio_returns <- create_mock_portfolio_returns()
  
  result <- calc_performance(portfolio_returns)
  
  expect_type(result$value, "character")
})

test_that("calc_performance annual return contains percent sign", {
  portfolio_returns <- create_mock_portfolio_returns()
  
  result <- calc_performance(portfolio_returns)
  
  ret_row <- result |> filter(metric == "Annualized.Return")
  expect_true(grepl("%", ret_row$value))
})

test_that("calc_performance annual volatility contains percent sign", {
  portfolio_returns <- create_mock_portfolio_returns()
  
  result <- calc_performance(portfolio_returns)
  
  vol_row <- result |> filter(metric == "Annualized.Volatility")
  expect_true(grepl("%", vol_row$value))
})

test_that("calc_performance sharpe ratio is numeric string", {
  portfolio_returns <- create_mock_portfolio_returns()
  
  result <- calc_performance(portfolio_returns)
  
  sharpe_row <- result |> filter(metric == "SharpeRatio")
  expect_false(grepl("%", sharpe_row$value))
})
