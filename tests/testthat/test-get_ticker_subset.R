# Tests for get_ticker_subset.R
source("../../R/helpers/get_ticker_subset.R")

test_that("get_ticker_subset filters ESG correctly", {
  tickers <- tibble(
    symbol = c("VTI", "VT", "IAUM", "ESGV", "VSGX", "FGDL"),
    stock_type = c(rep("Non-ESG", 3), rep("ESG", 3)),
    asset_type = c(rep(c("US Stock", "Global Stock", "Gold"), 2))
  )

  result <- get_ticker_subset(tickers, "ESG")

  expect_equal(nrow(result), 3)
  expect_true(all(result$stock_type == "ESG"))
  expect_equal(result$symbol, c("ESGV", "VSGX", "FGDL"))
})

test_that("get_ticker_subset filters Non-ESG correctly", {
  tickers <- tibble(
    symbol = c("VTI", "VT", "IAUM", "ESGV", "VSGX", "FGDL"),
    stock_type = c(rep("Non-ESG", 3), rep("ESG", 3)),
    asset_type = c(rep(c("US Stock", "Global Stock", "Gold"), 2))
  )

  result <- get_ticker_subset(tickers, "Non-ESG")

  expect_equal(nrow(result), 3)
  expect_true(all(result$stock_type == "Non-ESG"))
  expect_equal(result$symbol, c("VTI", "VT", "IAUM"))
})

test_that("get_ticker_subset returns empty tibble when no match", {
  tickers <- tibble(
    symbol = c("VTI", "VT"),
    stock_type = c("Non-ESG", "Non-ESG"),
    asset_type = c("US Stock", "Global Stock")
  )

  result <- get_ticker_subset(tickers, "ESG")

  expect_equal(nrow(result), 0)
  expect_s3_class(result, "tbl_df")
})

test_that("get_ticker_subset preserves asset_type column", {
  tickers <- tibble(
    symbol = c("ESGV", "VSGX"),
    stock_type = c("ESG", "ESG"),
    asset_type = c("US Stock", "Global Stock")
  )

  result <- get_ticker_subset(tickers, "ESG")

  expect_named(result, c("symbol", "stock_type", "asset_type"))
  expect_equal(result$asset_type, c("US Stock", "Global Stock"))
})
