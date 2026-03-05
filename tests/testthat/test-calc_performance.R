# Tests for calc_performance.R
source("../../R/helpers/calc_performance.R")

# Helper: create mock portfolio returns data
# Use a fixed seed so tests are deterministic.
create_mock_portfolio_returns <- function() {
  set.seed(42)
  tibble(
    date = as.Date("2024-01-01") + 0:99,
    portfolio.returns = rnorm(100, 0.0005, 0.01)
  )
}

# --- Output shape ---

test_that("calc_performance returns a data frame with exactly 3 rows", {
  result <- calc_performance(create_mock_portfolio_returns())

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 3)
})

test_that("calc_performance returns exactly 2 columns: metric and value", {
  result <- calc_performance(create_mock_portfolio_returns())

  expect_equal(names(result), c("metric", "value"))
})

# --- Metric names ---

test_that("calc_performance contains Annualized.Return metric", {
  result <- calc_performance(create_mock_portfolio_returns())

  expect_true("Annualized.Return" %in% result$metric)
})

test_that("calc_performance contains Annualized.Volatility metric", {
  result <- calc_performance(create_mock_portfolio_returns())

  expect_true("Annualized.Volatility" %in% result$metric)
})

test_that("calc_performance contains SharpeRatio metric", {
  # Verifies that rename_with(matches('Sharpe')) correctly normalises whatever
  # column name tq_performance emits (e.g. 'AnnualizedSharpe(Rf=0%)').
  result <- calc_performance(create_mock_portfolio_returns())

  expect_true("SharpeRatio" %in% result$metric)
})

# --- Value format ---

test_that("calc_performance value column is character type", {
  result <- calc_performance(create_mock_portfolio_returns())

  expect_type(result$value, "character")
})

test_that("calc_performance Annualized.Return value contains percent sign", {
  result <- calc_performance(create_mock_portfolio_returns())

  ret_val <- result |> filter(metric == "Annualized.Return") |> pull(value)
  expect_true(grepl("%", ret_val))
})

test_that("calc_performance Annualized.Volatility value contains percent sign", {
  result <- calc_performance(create_mock_portfolio_returns())

  vol_val <- result |> filter(metric == "Annualized.Volatility") |> pull(value)
  expect_true(grepl("%", vol_val))
})

test_that("calc_performance SharpeRatio value does not contain percent sign", {
  result <- calc_performance(create_mock_portfolio_returns())

  sharpe_val <- result |> filter(metric == "SharpeRatio") |> pull(value)
  expect_false(grepl("%", sharpe_val))
})

test_that("calc_performance SharpeRatio value is a valid number string", {
  result <- calc_performance(create_mock_portfolio_returns())

  sharpe_val <- result |> filter(metric == "SharpeRatio") |> pull(value)
  expect_false(is.na(suppressWarnings(as.numeric(sharpe_val))))
})
