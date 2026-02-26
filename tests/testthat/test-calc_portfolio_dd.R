# Tests for calc_portfolio_dd.R (calc_max_drawdown)

# Helper: create mock portfolio returns with drawdowns
create_mock_portfolio_with_dd <- function() {
  tibble(
    date = as.Date("2024-01-01") + 0:99,
    portfolio.returns = c(
      0.01, 0.01, 0.01, -0.05, -0.10, -0.15,  # Drawdown
      0.01, 0.01, 0.01, 0.01,                   # Recovery
      0.01, 0.01, -0.08, -0.12, -0.05, 0.01    # Another drawdown
    )
  )
}

# Helper: create flat returns (no drawdown)
create_mock_flat_returns <- function() {
  tibble(
    date = as.Date("2024-01-01") + 0:9,
    portfolio.returns = rep(0.01, 10)  # Constant positive returns
  )
}

test_that("calc_max_drawdown returns Depth column", {
  portfolio_returns <- create_mock_portfolio_with_dd()
  
  result <- calc_max_drawdown(portfolio_returns)
  
  expect_true("Depth" %in% names(result))
})

test_that("calc_max_drawdown returns Start column", {
  portfolio_returns <- create_mock_portfolio_with_dd()
  
  result <- calc_max_drawdown(portfolio_returns)
  
  expect_true("Start" %in% names(result))
})

test_that("calc_max_drawdown handles flat returns (no drawdown)", {
  portfolio_returns <- create_mock_flat_returns()
  
  result <- calc_max_drawdown(portfolio_returns)
  
  # Should return a row with "0%" for Depth
  expect_true("Depth" %in% names(result))
  expect_equal(result$Depth, "0%")
})

test_that("calc_max_drawdown has ToT column", {
  portfolio_returns <- create_mock_portfolio_with_dd()
  
  result <- calc_max_drawdown(portfolio_returns)
  
  expect_true("ToT" %in% names(result))
})

test_that("calc_max_drawdown Depth contains percent sign", {
  portfolio_returns <- create_mock_portfolio_with_dd()
  
  result <- calc_max_drawdown(portfolio_returns)
  
  expect_true(grepl("%", result$Depth))
})

test_that("calc_max_drawdown ToT contains days", {
  portfolio_returns <- create_mock_portfolio_with_dd()
  
  result <- calc_max_drawdown(portfolio_returns)
  
  expect_true(grepl("days", result$ToT))
})
