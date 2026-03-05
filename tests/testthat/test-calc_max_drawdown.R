# Tests for calc_max_drawdown.R
source("../../R/helpers/calc_max_drawdown.R")

# Helper: create mock portfolio returns with clear drawdowns.
# maxDrawdown requires a proper time series with sequential dates.
create_mock_portfolio_with_dd <- function() {
  tibble(
    date = as.Date("2024-01-01") + 0:15,
    portfolio.returns = c(
      0.01,
      0.01,
      0.01,
      -0.05,
      -0.10,
      -0.15, # drawdown
      0.01,
      0.01,
      0.01,
      0.01, # recovery
      0.01,
      0.01,
      -0.08,
      -0.12,
      -0.05, # another drawdown
      0.01
    )
  )
}

# Helper: all-positive returns — maxDrawdown returns 0
create_mock_flat_returns <- function() {
  tibble(
    date = as.Date("2024-01-01") + 0:9,
    portfolio.returns = rep(0.01, 10)
  )
}

# --- Output shape ---

test_that("calc_max_drawdown returns a data frame with exactly 1 column", {
  result <- calc_max_drawdown(create_mock_portfolio_with_dd())

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 1)
})

test_that("calc_max_drawdown returns exactly one row", {
  result <- calc_max_drawdown(create_mock_portfolio_with_dd())

  expect_equal(nrow(result), 1)
})

test_that("calc_max_drawdown returns MaxDrawdown column", {
  result <- calc_max_drawdown(create_mock_portfolio_with_dd())

  expect_true("MaxDrawdown" %in% names(result))
})

# --- Output format ---

test_that("calc_max_drawdown MaxDrawdown contains a percent sign", {
  result <- calc_max_drawdown(create_mock_portfolio_with_dd())

  expect_true(grepl("%", result$MaxDrawdown))
})

test_that("calc_max_drawdown MaxDrawdown is a character string", {
  result <- calc_max_drawdown(create_mock_portfolio_with_dd())

  expect_type(result$MaxDrawdown, "character")
})

# --- Edge case: no drawdowns ---

test_that("calc_max_drawdown handles flat (all-positive) returns gracefully", {
  result <- calc_max_drawdown(create_mock_flat_returns())

  # Should return "0%" when there are no drawdowns
  expect_equal(nrow(result), 1)
  expect_equal(result$MaxDrawdown, "0%")
})
