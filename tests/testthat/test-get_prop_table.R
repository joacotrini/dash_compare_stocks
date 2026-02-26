# Tests for get_prop_table.R

test_that("get_prop_table converts percentages to proportions", {
  ticker_subset <- tibble(symbol = c("A", "B"))
  props <- c(60, 40)
  
  result <- get_prop_table(ticker_subset, props)
  
  expect_equal(result$prop, c(0.6, 0.4))
})

test_that("get_prop_table returns correct columns", {
  ticker_subset <- tibble(symbol = c("A", "B", "C"))
  props <- c(50, 30, 20)
  
  result <- get_prop_table(ticker_subset, props)
  
  expect_named(result, c("symbol", "prop"))
})

test_that("get_prop_table proportions sum to 1", {
  ticker_subset <- tibble(symbol = c("A", "B", "C"))
  props <- c(50, 30, 20)
  
  result <- get_prop_table(ticker_subset, props)
  
  expect_equal(sum(result$prop), 1)
})

test_that("get_prop_table handles 100-0 split", {
  ticker_subset <- tibble(symbol = c("A", "B"))
  props <- c(100, 0)
  
  result <- get_prop_table(ticker_subset, props)
  
  expect_equal(result$prop, c(1, 0))
})

test_that("get_prop_table preserves symbol order", {
  ticker_subset <- tibble(symbol = c("C", "A", "B"))
  props <- c(20, 50, 30)
  
  result <- get_prop_table(ticker_subset, props)
  
  expect_equal(result$symbol, c("C", "A", "B"))
})
