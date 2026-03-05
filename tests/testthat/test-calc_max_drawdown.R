# Tests for calc_max_drawdown.R
source("../../R/helpers/calc_max_drawdown.R")

# Helper: create mock portfolio returns with clear drawdowns.
# Dates must be sequential — table.Drawdowns requires a proper time index.
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

# Helper: all-positive returns — table.Drawdowns returns empty tibble,
# so the function falls to the else-branch and returns the NA / "0%" sentinel.
create_mock_flat_returns <- function() {
  tibble(
    date = as.Date("2024-01-01") + 0:9,
    portfolio.returns = rep(0.01, 10)
  )
}

# --- Column presence ---

test_that("calc_max_drawdown returns a data frame with exactly 4 columns", {
  result <- calc_max_drawdown(create_mock_portfolio_with_dd())

  expect_s3_class(result, "data.frame")
  expect_equal(ncol(result), 4)
})

test_that("calc_max_drawdown returns Start column", {
  result <- calc_max_drawdown(create_mock_portfolio_with_dd())

  expect_true("Start" %in% names(result))
})

test_that("calc_max_drawdown returns Depth column", {
  result <- calc_max_drawdown(create_mock_portfolio_with_dd())

  expect_true("Depth" %in% names(result))
})

test_that("calc_max_drawdown returns Recovered column", {
  result <- calc_max_drawdown(create_mock_portfolio_with_dd())

  expect_true("Recovered" %in% names(result))
})

test_that("calc_max_drawdown returns ToT column", {
  result <- calc_max_drawdown(create_mock_portfolio_with_dd())

  expect_true("ToT" %in% names(result))
})

# --- Output format ---

test_that("calc_max_drawdown Depth contains a percent sign", {
  result <- calc_max_drawdown(create_mock_portfolio_with_dd())

  expect_true(grepl("%", result$Depth))
})

test_that("calc_max_drawdown ToT contains the word 'days'", {
  result <- calc_max_drawdown(create_mock_portfolio_with_dd())

  expect_true(grepl("days", result$ToT))
})

test_that("calc_max_drawdown returns exactly one row (worst drawdown)", {
  result <- calc_max_drawdown(create_mock_portfolio_with_dd())

  expect_equal(nrow(result), 1)
})

test_that("calc_max_drawdown Start and Recovered are character strings", {
  result <- calc_max_drawdown(create_mock_portfolio_with_dd())

  expect_type(result$Start, "character")
  expect_type(result$Recovered, "character")
})

# --- Edge case: no drawdowns ---

test_that("calc_max_drawdown handles flat (all-positive) returns gracefully", {
  result <- calc_max_drawdown(create_mock_flat_returns())

  # Falls to else-branch: sentinel row with "0%" depth and "0 days"
  expect_equal(nrow(result), 1)
  expect_equal(result$Depth, "0%")
  expect_equal(result$ToT, "0 days")
})
