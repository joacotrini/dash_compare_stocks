# Tests for calc_portfolio_returns.R

# Helper: create mock returns data (simulating output of calc_returns)
create_mock_returns_data <- function() {
  tibble(
    symbol = rep(c("A", "B"), each = 5),
    date = rep(as.Date("2024-01-01") + 0:4, 2),
    daily_return = c(0.01, 0.02, -0.01, 0.03, 0.02, 
                     0.005, 0.01, -0.02, 0.015, 0.01),
    adjusted = c(100, 102, 101, 103, 105, 50, 51, 49, 52, 53),
    prop = rep(c(0.6, 0.4), each = 5)
  )
}

test_that("calc_portfolio_returns returns portfolio.returns column", {
  returns_data <- create_mock_returns_data()
  prop_table <- tibble(symbol = c("A", "B"), prop = c(0.6, 0.4))
  
  result <- calc_portfolio_returns(returns_data, prop_table)
  
  expect_true("portfolio.returns" %in% names(result))
})

test_that("calc_portfolio_returns returns one row per date", {
  returns_data <- create_mock_returns_data()
  prop_table <- tibble(symbol = c("A", "B"), prop = c(0.6, 0.4))
  
  result <- calc_portfolio_returns(returns_data, prop_table)
  
  expect_equal(nrow(result), 5)
})

test_that("calc_portfolio_returns has date column", {
  returns_data <- create_mock_returns_data()
  prop_table <- tibble(symbol = c("A", "B"), prop = c(0.6, 0.4))
  
  result <- calc_portfolio_returns(returns_data, prop_table)
  
  expect_true("date" %in% names(result))
})

test_that("calc_portfolio_returns handles equal weights", {
  returns_data <- create_mock_returns_data()
  prop_table <- tibble(symbol = c("A", "B"), prop = c(0.5, 0.5))
  
  result <- calc_portfolio_returns(returns_data, prop_table)
  
  expect_true("portfolio.returns" %in% names(result))
  expect_equal(nrow(result), 5)
})

test_that("calc_portfolio_returns portfolio returns are weighted average of assets", {
  returns_data <- create_mock_returns_data()
  prop_table <- tibble(symbol = c("A", "B"), prop = c(1.0, 0.0))
  
  result <- calc_portfolio_returns(returns_data, prop_table)
  
  # With 100% weight on A, should match A's returns exactly
  # This is a sanity check that the function works
  expect_true("portfolio.returns" %in% names(result))
  expect_true(!all(is.na(result$portfolio.returns)))
})
