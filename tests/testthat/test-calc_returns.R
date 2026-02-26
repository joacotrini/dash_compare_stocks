# Tests for calc_returns.R

# Helper: create mock stock data
create_mock_stock_data <- function() {
  tibble(
    symbol = rep(c("A", "B"), each = 5),
    date = rep(as.Date("2024-01-01") + 0:4, 2),
    adjusted = c(100, 102, 101, 103, 105, 50, 51, 49, 52, 53)
  )
}

test_that("calc_returns adds daily_return column", {
  mock_data <- create_mock_stock_data()
  prop_table <- tibble(symbol = c("A", "B"), prop = c(0.6, 0.4))
  
  result <- calc_returns(mock_data, prop_table)
  
  expect_true("daily_return" %in% names(result))
})

test_that("calc_returns filters to only symbols in prop_table", {
  mock_data <- create_mock_stock_data()
  prop_table <- tibble(symbol = c("A"), prop = c(1.0))
  
  result <- calc_returns(mock_data, prop_table)
  
  expect_equal(unique(result$symbol), "A")
})

test_that("calc_returns joins prop table correctly", {
  mock_data <- create_mock_stock_data()
  prop_table <- tibble(symbol = c("A", "B"), prop = c(0.6, 0.4))
  
  result <- calc_returns(mock_data, prop_table)
  
  expect_true("prop" %in% names(result))
  expect_equal(unique(result$prop), c(0.6, 0.4))
})

test_that("calc_returns handles single symbol", {
  mock_data <- create_mock_stock_data() |> filter(symbol == "A")
  prop_table <- tibble(symbol = c("A"), prop = c(1.0))
  
  result <- calc_returns(mock_data, prop_table)
  
  expect_equal(unique(result$symbol), "A")
  expect_true("daily_return" %in% names(result))
})

test_that("calc_returns daily returns are reasonable (between -1 and 1)", {
  mock_data <- create_mock_stock_data()
  prop_table <- tibble(symbol = c("A", "B"), prop = c(0.5, 0.5))
  
  result <- calc_returns(mock_data, prop_table)
  
  expect_true(all(result$daily_return > -1 & result$daily_return < 1, na.rm = TRUE))
})
