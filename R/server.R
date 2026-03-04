library(corrr)
library(lubridate)
library(plotly)
library(shiny)
library(tidyquant)
library(tidyverse)

# Source helpers
source_helpers <- function() {
  helper_files <- list.files("R/helpers", pattern = "\\.R$", full.names = TRUE)
  sapply(helper_files, source)
}
source_helpers()

# Global values

## Ticker definitions
tickers <- tibble(
  symbol = c("VTI", "VT", "IAUM", "ESGV", "VSGX", "FGDL"),
  stock_type = c(rep("Non-ESG", 3), rep("ESG", 3)),
  asset_type = c(rep(c("US Stock", "Global Stock", "Gold"), 2)),
)

## Load all data at startup
data <- tq_get(
  x = tickers$symbol,
  get = "stock.prices",
  from = "2020-01-01",
  to = Sys.Date()
)

# Server
server <- function(input, output, session) {
  # === VALIDATION ===

  validate_inputs <- reactive({
    req(
      input$date_start,
      input$date_end,
      input$proportion_non_esg_1,
      input$proportion_non_esg_2,
      input$proportion_esg_1,
      input$proportion_esg_2
    )

    errors <- c()

    # Check if start date is before end date
    if (input$date_start >= input$date_end) {
      errors <- c(
        errors,
        "Start date must be before end date"
      )
    }

    # Check if end date is after start date
    if (input$date_end < input$date_start) {
      errors <- c(
        errors,
        "End date must be after start date"
      )
    }

    # Check if proportions sum to 100% (slider 1 + slider 2 + calculated <= 100)
    sum_non_esg <- input$proportion_non_esg_1 + input$proportion_non_esg_2
    sum_esg <- input$proportion_esg_1 + input$proportion_esg_2

    if (sum_non_esg > 100) {
      errors <- c(
        errors,
        paste0(
          "Non-ESG proportions (VTI + VT) = ",
          sum_non_esg,
          "% (must be <= 100%)"
        )
      )
    }

    if (sum_esg > 100) {
      errors <- c(
        errors,
        paste0(
          "ESG proportions (ESGV + VSGX) = ",
          sum_esg,
          "% (must be <= 100%)"
        )
      )
    }

    # Return TRUE if valid, otherwise return error messages
    if (length(errors) > 0) {
      return(list(valid = FALSE, errors = errors))
    } else {
      return(list(valid = NULL, errors = NULL))
    }
  })

  # === DATA FILTERING ===

  # Filter by date
  data_filtered <- reactive({
    req(input$date_start, input$date_end)
    data |>
      filter(date >= input$date_start, date <= input$date_end)
  })

  # === COMPUTATION ===

  # Subset tickers by stock type
  ticker_subset_non_esg <- reactive(get_ticker_subset(tickers, "Non-ESG"))
  ticker_subset_esg <- reactive(get_ticker_subset(tickers, "ESG"))

  # Calculate proportions (2 sliders + calculated 3rd)
  data_tickers_non_esg <- reactive({
    prop_3 <- 100 - input$proportion_non_esg_1 - input$proportion_non_esg_2
    get_prop_table(
      ticker_subset_non_esg(),
      c(
        input$proportion_non_esg_1,
        input$proportion_non_esg_2,
        prop_3
      )
    )
  })

  data_tickers_esg <- reactive({
    prop_3 <- 100 - input$proportion_esg_1 - input$proportion_esg_2
    get_prop_table(
      ticker_subset_esg(),
      c(
        input$proportion_esg_1,
        input$proportion_esg_2,
        prop_3
      )
    )
  })

  # Calculate daily returns
  data_returns_non_esg <- reactive({
    calc_returns(data_filtered(), data_tickers_non_esg())
  })

  data_returns_esg <- reactive({
    calc_returns(data_filtered(), data_tickers_esg())
  })

  # Calculate portfolio returns using tidyquant
  data_portfolio_non_esg <- reactive({
    calc_portfolio_returns(data_returns_non_esg(), data_tickers_non_esg())
  })

  data_portfolio_esg <- reactive({
    calc_portfolio_returns(data_returns_esg(), data_tickers_esg())
  })

  # Cumulative returns for plotting
  data_cum_returns_non_esg <- reactive({
    data_returns_non_esg() |>
      group_by(symbol) |>
      mutate(cumulative_return = cumprod(1 + daily_return) - 1) |>
      ungroup()
  })

  data_cum_returns_esg <- reactive({
    data_returns_esg() |>
      group_by(symbol) |>
      mutate(cumulative_return = cumprod(1 + daily_return) - 1) |>
      ungroup()
  })

  data_cum_portfolio_non_esg <- reactive({
    data_portfolio_non_esg() |>
      mutate(
        symbol = "PORTFOLIO",
        cumulative_return = cumprod(1 + portfolio.returns) - 1
      ) |>
      select(date, symbol, cumulative_return)
  })

  data_cum_portfolio_esg <- reactive({
    data_portfolio_esg() |>
      mutate(
        symbol = "PORTFOLIO",
        cumulative_return = cumprod(1 + portfolio.returns) - 1
      ) |>
      select(date, symbol, cumulative_return)
  })

  # Join tables for following computations
  data_returns_with_portfolio_non_esg <- reactive({
    bind_rows(
      data_cum_returns_non_esg() |>
        select(date, symbol, diff_dtd = cumulative_return),
      data_cum_portfolio_non_esg()
    )
  })

  data_returns_with_portfolio_esg <- reactive({
    bind_rows(
      data_cum_returns_esg() |>
        select(date, symbol, diff_dtd = cumulative_return),
      data_cum_portfolio_esg()
    )
  })

  # === TABLES ===

  # Calculate performance metrics using tidyquant
  perf_non_esg <- reactive({
    validate(need(
      is.null(validate_inputs()$errors),
      paste(validate_inputs()$errors, collapse = "\n")
    ))
    calc_performance(data_portfolio_non_esg())
  })

  perf_esg <- reactive({
    validate(need(
      is.null(validate_inputs()$errors),
      paste(validate_inputs()$errors, collapse = "\n")
    ))
    calc_performance(data_portfolio_esg())
  })

  # Calculate max drawdown
  dd_non_esg <- reactive({
    validate(need(
      is.null(validate_inputs()$errors),
      paste(validate_inputs()$errors, collapse = "\n")
    ))
    calc_max_drawdown(data_portfolio_non_esg())
  })

  dd_esg <- reactive({
    validate(need(
      is.null(validate_inputs()$errors),
      paste(validate_inputs()$errors, collapse = "\n")
    ))
    calc_max_drawdown(data_portfolio_esg())
  })

  # Calculate correlation matrices
  corr_non_esg <- reactive({
    validate(need(
      is.null(validate_inputs()$errors),
      paste(validate_inputs()$errors, collapse = "\n")
    ))
    calc_corr(data_returns_with_portfolio_non_esg())
  })

  corr_esg <- reactive({
    validate(need(
      is.null(validate_inputs()$errors),
      paste(validate_inputs()$errors, collapse = "\n")
    ))
    calc_corr(data_returns_with_portfolio_esg())
  })

  # === PLOTS ===

  p_comparison <- reactive({
    validate(need(
      is.null(validate_inputs()$errors),
      paste(validate_inputs()$errors, collapse = "\n")
    ))
    build_comparison_plot(
      data_cum_portfolio_non_esg(),
      data_cum_portfolio_esg(),
      "Portfolio Comparison: ESG vs Non-ESG"
    )
  })

  p_dtd_non_esg <- reactive({
    validate(need(
      is.null(validate_inputs()$errors),
      paste(validate_inputs()$errors, collapse = "\n")
    ))
    build_plot(
      data_returns_with_portfolio_non_esg(),
      ticker_subset_non_esg(),
      "Non-ESG Portfolio: Cumulative Return"
    )
  })

  p_dtd_esg <- reactive({
    validate(need(
      is.null(validate_inputs()$errors),
      paste(validate_inputs()$errors, collapse = "\n")
    ))
    build_plot(
      data_returns_with_portfolio_esg(),
      ticker_subset_esg(),
      "ESG Portfolio: Cumulative Return"
    )
  })

  # === OUTPUTS ===

  output$validation_errors_ui <- renderUI({
    val <- validate_inputs()
    if (!is.null(val$errors)) {
      div(
        class = "alert alert-danger",
        style = "margin-top: 10px;",
        strong("Error:"),
        br(),
        lapply(val$errors, br)
      )
    } else {
      NULL
    }
  })

  output$stats_non_esg <- renderTable(perf_non_esg())
  output$corr_non_esg <- renderTable(corr_non_esg())
  output$p_comparison <- renderPlotly(p_comparison())
  output$p_dtd_non_esg <- renderPlotly(p_dtd_non_esg())

  output$stats_esg <- renderTable(perf_esg())
  output$corr_esg <- renderTable(corr_esg())
  output$p_dtd_esg <- renderPlotly(p_dtd_esg())

  # === VALUE BOXES ===
  output$vb_return_non_esg <- renderText({
    validate(need(
      is.null(validate_inputs()$errors),
      paste(validate_inputs()$errors, collapse = "\n")
    ))
    perf <- perf_non_esg()
    ret <- perf |> filter(metric == "Annualized.Return") |> pull(value)
    ret
  })

  output$vb_volatility_non_esg <- renderText({
    validate(need(
      is.null(validate_inputs()$errors),
      paste(validate_inputs()$errors, collapse = "\n")
    ))
    perf <- perf_non_esg()
    vol <- perf |> filter(metric == "Annualized.Volatility") |> pull(value)
    vol
  })

  output$vb_return_esg <- renderText({
    validate(need(
      is.null(validate_inputs()$errors),
      paste(validate_inputs()$errors, collapse = "\n")
    ))
    perf <- perf_esg()
    ret <- perf |> filter(metric == "Annualized.Return") |> pull(value)
    ret
  })

  output$vb_volatility_esg <- renderText({
    validate(need(
      is.null(validate_inputs()$errors),
      paste(validate_inputs()$errors, collapse = "\n")
    ))
    perf <- perf_esg()
    vol <- perf |> filter(metric == "Annualized.Volatility") |> pull(value)
    vol
  })

  # === PROPORTION DISPLAY ===
  output$proportion_non_esg_3_display <- renderText({
    prop_3 <- 100 - input$proportion_non_esg_1 - input$proportion_non_esg_2
    paste0("IAUM (Gold): ", prop_3, "% (calculated)")
  })

  output$proportion_esg_3_display <- renderText({
    prop_3 <- 100 - input$proportion_esg_1 - input$proportion_esg_2
    paste0("FGDL (Gold): ", prop_3, "% (calculated)")
  })

  # === STOCK INFO TABLE ===
  stocks_info_data <- reactive({
    validate(need(
      is.null(validate_inputs()$errors),
      paste(validate_inputs()$errors, collapse = "\n")
    ))

    # Combine daily returns from both portfolios
    returns_combined <- bind_rows(
      data_returns_non_esg() |> select(symbol, date, adjusted, daily_return),
      data_returns_esg() |> select(symbol, date, adjusted, daily_return)
    ) |>
      distinct(symbol, .keep_all = TRUE)

    # Get latest prices
    latest_prices <- data_filtered() |>
      group_by(symbol) |>
      slice_tail(n = 1) |>
      select(symbol, price = adjusted)

    # Calculate metrics using already-computed daily returns
    returns_summary <- returns_combined |>
      group_by(symbol) |>
      summarize(
        period_return = (last(adjusted) / first(adjusted) - 1) * 100,
        volatility = sd(daily_return, na.rm = TRUE) * sqrt(252) * 100,
        .groups = "drop"
      )

    tickers |>
      left_join(latest_prices, by = "symbol") |>
      left_join(returns_summary, by = "symbol") |>
      mutate(
        price = scales::dollar(price),
        period_return = scales::percent(period_return / 100, accuracy = 0.1),
        volatility = scales::percent(volatility / 100, accuracy = 0.1)
      ) |>
      select(
        Symbol = symbol,
        `Asset Type` = asset_type,
        Price = price,
        Return = period_return,
        Volatility = volatility
      )
  })

  output$stocks_info <- renderTable(stocks_info_data())
}
