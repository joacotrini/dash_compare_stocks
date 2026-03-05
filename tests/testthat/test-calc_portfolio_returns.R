# Tests for calc_portfolio_returns.R
source("../../R/helpers/calc_portfolio_returns.R")

# Helper: create mock returns data (simulating output of calc_returns).
# tq_portfolio only uses `date`, `symbol`, and `daily_return`; the extra
# columns (adjusted, prop) mirror the real calc_returns output.
create_mock_returns_data <- function() {
  tibble(
    symbol = rep(c("A", "B"), each = 5),
    date = rep(as.Date("2024-01-01") + 0:4, 2),
    daily_return = c(
      0.01,
      0.02,
      -0.01,
      0.03,
      0.02,
      0.005,
      0.01,
      -0.02,
      0.015,
      0.01
    ),
    adjusted = c(100, 102, 101, 103, 105, 50, 51, 49, 52, 53),
    prop = rep(c(0.6, 0.4), each = 5)
  )
}

# --- Output shape ---

test_that("calc_portfolio_returns returns a data frame", {
  result <- calc_portfolio_returns(
    create_mock_returns_data(),
    tibble(symbol = c("A", "B"), prop = c(0.6, 0.4))
  )

  expect_s3_class(result, "data.frame")
})

test_that("calc_portfolio_returns returns a date column", {
  result <- calc_portfolio_returns(
    create_mock_returns_data(),
    tibble(symbol = c("A", "B"), prop = c(0.6, 0.4))
  )

  expect_true("date" %in% names(result))
})

test_that("calc_portfolio_returns returns a portfolio.returns column", {
  result <- calc_portfolio_returns(
    create_mock_returns_data(),
    tibble(symbol = c("A", "B"), prop = c(0.6, 0.4))
  )

  expect_true("portfolio.returns" %in% names(result))
})

test_that("calc_portfolio_returns returns one row per date", {
  result <- calc_portfolio_returns(
    create_mock_returns_data(),
    tibble(symbol = c("A", "B"), prop = c(0.6, 0.4))
  )

  expect_equal(nrow(result), 5)
})

# --- Weight handling ---

test_that("calc_portfolio_returns works with equal weights", {
  result <- calc_portfolio_returns(
    create_mock_returns_data(),
    tibble(symbol = c("A", "B"), prop = c(0.5, 0.5))
  )

  expect_equal(nrow(result), 5)
  expect_true(!all(is.na(result$portfolio.returns)))
})

test_that("calc_portfolio_returns with 100% weight on A matches A returns", {
  returns_data <- create_mock_returns_data()
  result <- calc_portfolio_returns(
    returns_data,
    tibble(symbol = c("A", "B"), prop = c(1.0, 0.0))
  )

  a_returns <- returns_data |> filter(symbol == "A") |> pull(daily_return)

  # tq_portfolio uses geometric chaining, so check approximate equality
  expect_equal(result$portfolio.returns, a_returns, tolerance = 1e-6)
})
