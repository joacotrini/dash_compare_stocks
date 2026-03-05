# Tests for calc_returns.R
source("../../R/helpers/calc_returns.R")

# Helper: create mock stock data.
# Only `symbol`, `date`, and `adjusted` are required — tq_mutate(select = adjusted)
# passes just the adjusted column to periodReturn.
create_mock_stock_data <- function() {
  tibble(
    symbol = rep(c("A", "B"), each = 5),
    date = rep(as.Date("2024-01-01") + 0:4, 2),
    adjusted = c(100, 102, 101, 103, 105, 50, 51, 49, 52, 53)
  )
}

# --- Output columns ---

test_that("calc_returns adds daily_return column", {
  result <- calc_returns(
    create_mock_stock_data(),
    tibble(symbol = c("A", "B"), prop = c(0.6, 0.4))
  )

  expect_true("daily_return" %in% names(result))
})

test_that("calc_returns preserves adjusted column", {
  # tq_mutate appends daily_return without dropping existing columns,
  # so adjusted must still be present for downstream use (e.g. stocks_info_data).
  result <- calc_returns(
    create_mock_stock_data(),
    tibble(symbol = c("A", "B"), prop = c(0.6, 0.4))
  )

  expect_true("adjusted" %in% names(result))
})

test_that("calc_returns joins prop column from prop_table", {
  result <- calc_returns(
    create_mock_stock_data(),
    tibble(symbol = c("A", "B"), prop = c(0.6, 0.4))
  )

  expect_true("prop" %in% names(result))
  expect_equal(sort(unique(result$prop)), c(0.4, 0.6))
})

# --- Filtering ---

test_that("calc_returns filters to only symbols present in prop_table", {
  result <- calc_returns(
    create_mock_stock_data(),
    tibble(symbol = "A", prop = 1.0)
  )

  expect_equal(unique(result$symbol), "A")
})

test_that("calc_returns handles a single-symbol prop_table", {
  result <- calc_returns(
    create_mock_stock_data() |> filter(symbol == "A"),
    tibble(symbol = "A", prop = 1.0)
  )

  expect_equal(unique(result$symbol), "A")
  expect_true("daily_return" %in% names(result))
})

# --- Value sanity ---

test_that("calc_returns daily_return values are between -1 and 1", {
  result <- calc_returns(
    create_mock_stock_data(),
    tibble(symbol = c("A", "B"), prop = c(0.5, 0.5))
  )

  expect_true(all(
    result$daily_return > -1 & result$daily_return < 1,
    na.rm = TRUE
  ))
})
