# Tests for calc_corr.R

# Load the function being tested
source("../../R/helpers/calc_corr.R")

# Helper: create mock returns with portfolio data
create_mock_returns_with_portfolio <- function() {
  tibble(
    date = rep(as.Date("2024-01-01") + 0:49, each = 4),
    symbol = rep(c("A", "B", "C", "PORTFOLIO"), times = 50),
    diff_dtd = c(
      rnorm(50, 0.001, 0.01), # A
      rnorm(50, 0.001, 0.01), # B
      rnorm(50, 0.001, 0.01), # C
      rnorm(50, 0.001, 0.008) # PORTFOLIO (lower vol)
    )
  )
}

test_that("calc_corr returns correlation matrix", {
  data <- create_mock_returns_with_portfolio()

  result <- calc_corr(data)

  expect_s3_class(result, "data.frame")
})

test_that("calc_corr matrix is square", {
  data <- create_mock_returns_with_portfolio()

  result <- calc_corr(data)

  # Remove the 'term' column for dimension check
  num_rows <- nrow(result)
  num_cols <- ncol(result) - 1 # Subtract 'term' column
  expect_equal(num_rows, num_cols)
})

test_that("calc_corr diagonal values are NA", {
  data <- create_mock_returns_with_portfolio()

  result <- calc_corr(data)

  # Get numeric columns only
  numeric_mat <- result |> select(-term) |> as.matrix()
  diag_values <- diag(numeric_mat)

  expect_true(all(is.na(diag_values)))
})

test_that("calc_corr is symmetric", {
  data <- create_mock_returns_with_portfolio()
  result <- calc_corr(data)
  numeric_mat <- result |> select(-term) |> as.matrix()

  # Fix diagonal and strip names for comparison
  diag(numeric_mat) <- 1
  dimnames(numeric_mat) <- NULL

  expect_equal(numeric_mat, t(numeric_mat), tolerance = 0.001)
})

test_that("calc_corr has term column", {
  data <- create_mock_returns_with_portfolio()

  result <- calc_corr(data)

  expect_true("term" %in% names(result))
})

test_that("calc_corr handles single asset", {
  data <- tibble(
    date = as.Date("2024-01-01") + 0:49,
    symbol = rep("A", 50),
    diff_dtd = rnorm(50, 0.001, 0.01)
  )

  result <- calc_corr(data)

  # Should return 1x1 matrix with value 1
  expect_equal(nrow(result), 1)
})
