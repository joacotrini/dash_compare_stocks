# Tests for calc_corr.R
source("../../R/helpers/calc_corr.R")

# Helper: create mock returns with portfolio data.
# Uses a fixed seed for deterministic correlations.
create_mock_returns_with_portfolio <- function() {
  set.seed(42)
  tibble(
    date = rep(as.Date("2024-01-01") + 0:49, each = 4),
    symbol = rep(c("A", "B", "C", "PORTFOLIO"), times = 50),
    daily_return = c(
      rnorm(50, 0.001, 0.01), # A
      rnorm(50, 0.001, 0.01), # B
      rnorm(50, 0.001, 0.01), # C
      rnorm(50, 0.001, 0.008) # PORTFOLIO (lower vol)
    )
  )
}

# --- Output shape ---

test_that("calc_corr returns a data frame", {
  result <- calc_corr(create_mock_returns_with_portfolio())

  expect_s3_class(result, "data.frame")
})

test_that("calc_corr returns exactly 2 columns", {
  result <- calc_corr(create_mock_returns_with_portfolio())

  expect_equal(ncol(result), 2)
})

test_that("calc_corr returns one row per non-portfolio symbol", {
  result <- calc_corr(create_mock_returns_with_portfolio())

  # 3 stocks (A, B, C) — PORTFOLIO row should be excluded
  expect_equal(nrow(result), 3)
})

# --- Column names ---

test_that("calc_corr has Stock column", {
  result <- calc_corr(create_mock_returns_with_portfolio())

  expect_true("Stock" %in% names(result))
})

test_that("calc_corr has 'Corr. with Portfolio' column", {
  result <- calc_corr(create_mock_returns_with_portfolio())

  expect_true("Corr. with Portfolio" %in% names(result))
})

# --- Content ---

test_that("calc_corr does not include PORTFOLIO as a row", {
  result <- calc_corr(create_mock_returns_with_portfolio())

  expect_false("PORTFOLIO" %in% result$Stock)
})

test_that("calc_corr Stock column contains all individual symbols", {
  result <- calc_corr(create_mock_returns_with_portfolio())

  expect_setequal(result$Stock, c("A", "B", "C"))
})

test_that("calc_corr correlation values are between -1 and 1", {
  result <- calc_corr(create_mock_returns_with_portfolio())

  expect_true(all(
    result$`Corr. with Portfolio` >= -1 &
      result$`Corr. with Portfolio` <= 1,
    na.rm = TRUE
  ))
})

test_that("calc_corr results are sorted descending by correlation", {
  result <- calc_corr(create_mock_returns_with_portfolio())

  corr_vals <- result$`Corr. with Portfolio`
  expect_equal(corr_vals, sort(corr_vals, decreasing = TRUE))
})

test_that("calc_corr correlation values are rounded to 3 decimal places", {
  result <- calc_corr(create_mock_returns_with_portfolio())

  rounded <- round(result$`Corr. with Portfolio`, 3)
  expect_equal(result$`Corr. with Portfolio`, rounded)
})
