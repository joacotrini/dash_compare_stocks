library(corrr)
library(lubridate)
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
      input$proportion_non_esg_3,
      input$proportion_esg_1,
      input$proportion_esg_2,
      input$proportion_esg_3
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

    # Check if proportions sum to 100%
    sum_non_esg <- input$proportion_non_esg_1 +
      input$proportion_non_esg_2 +
      input$proportion_non_esg_3
    sum_esg <- input$proportion_esg_1 +
      input$proportion_esg_2 +
      input$proportion_esg_3

    if (sum_non_esg != 100) {
      errors <- c(
        errors,
        paste0("Non-ESG proportions sum to ", sum_non_esg, "% (must be 100%)")
      )
    }

    if (sum_esg != 100) {
      errors <- c(
        errors,
        paste0("ESG proportions sum to ", sum_esg, "% (must be 100%)")
      )
    }

    # Return NULL if valid, otherwise return error messages
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

  # Calculate proportions
  data_tickers_non_esg <- reactive({
    get_prop_table(
      ticker_subset_non_esg(),
      c(
        input$proportion_non_esg_1,
        input$proportion_non_esg_2,
        input$proportion_non_esg_3
      )
    )
  })

  data_tickers_esg <- reactive({
    get_prop_table(
      ticker_subset_esg(),
      c(
        input$proportion_esg_1,
        input$proportion_esg_2,
        input$proportion_esg_3
      )
    )
  })

  # Calculate date-to-date (DTD) change rates
  data_dtd_non_esg <- reactive({
    calc_dtd(data_filtered(), data_tickers_non_esg())
  })

  data_dtd_esg <- reactive({
    calc_dtd(data_filtered(), data_tickers_esg())
  })

  # Calculate portfolio-level DTD
  data_portfolio_non_esg <- reactive({
    calc_portfolio(data_dtd_non_esg())
  })

  data_portfolio_esg <- reactive({
    calc_portfolio(data_dtd_esg())
  })

  # Join tables for follwoing computations
  data_dtd_with_portfolio_non_esg <- reactive({
    bind_rows(
      data_dtd_non_esg() |> select(date, symbol, diff_dtd),
      data_portfolio_non_esg()
    )
  })

  data_dtd_with_portfolio_esg <- reactive({
    bind_rows(
      data_dtd_esg() |> select(date, symbol, diff_dtd),
      data_portfolio_esg()
    )
  })

  # === TABLES ===

  # Calculate stats
  stats_non_esg <- reactive({
    validate(validate_inputs()$valid, validate_inputs()$errors)
    calc_stats(data_dtd_non_esg(), data_tickers_non_esg())
  })

  stats_esg <- reactive({
    validate(validate_inputs()$valid, validate_inputs()$errors)
    calc_stats(data_dtd_esg(), data_tickers_esg())
  })

  # Calculate correlation matrices
  corr_non_esg <- reactive({
    validate(validate_inputs()$valid, validate_inputs()$errors)
    calc_corr(data_dtd_with_portfolio_non_esg())
  })

  corr_esg <- reactive({
    validate(validate_inputs()$valid, validate_inputs()$errors)
    calc_corr(data_dtd_with_portfolio_esg())
  })

  # === PLOTS ===

  p_dtd_non_esg <- reactive({
    validate(validate_inputs()$valid, validate_inputs()$errors)
    build_plot(
      data_dtd_with_portfolio_non_esg(),
      ticker_subset_non_esg(),
      "Non-ESG Portfolio: DTD Change Rate"
    )
  })

  p_dtd_esg <- reactive({
    validate(validate_inputs()$valid, validate_inputs()$errors)
    build_plot(
      data_dtd_with_portfolio_esg(),
      ticker_subset_esg(),
      "ESG Portfolio: DTD Change Rate"
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

  output$stats_non_esg <- renderTable(stats_non_esg())
  output$corr_non_esg <- renderTable(corr_non_esg())
  output$p_dtd_non_esg <- renderPlot(p_dtd_non_esg())

  output$stats_esg <- renderTable(stats_esg())
  output$corr_esg <- renderTable(corr_esg())
  output$p_dtd_esg <- renderPlot(p_dtd_esg())
}
