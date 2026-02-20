library(corrr)
library(lubridate)
library(shiny)
library(tidyquant)
library(tidyverse)

# Global values

## Ticker definitions
tickers_non_esg <- c("VTI", "VT", "IAUM")
tickers_esg <- c("ESGV", "VSGX", "FGDL")
all_tickers <- c(tickers_non_esg, tickers_esg)

## Load all data at startup
data <- tq_get(
  x = all_tickers,
  get = "stock.prices",
  from = "2020-01-01",
  to = Sys.Date()
)

# Helper function: Calculate max drawdown
calc_max_drawdown <- function(prices) {
  cummax_val <- cummax(prices)
  drawdowns <- (prices - cummax_val) / cummax_val
  max_drawdown <- max(drawdowns)
  return(max_drawdown)
}

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

    # Check if start date and end date are valid
    if (input$date_start >= input$date_end) {
      errors <- c(
        errors,
        "Start date must be before end date"
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

  data_filtered <- reactive({
    req(input$date_start, input$date_end)
    data |>
      filter(date >= input$date_start, date <= input$date_end)
  })

  # === NON-ESG PORTFOLIO ===

  data_tickers_non_esg <- reactive({
    tibble(
      symbol = tickers_non_esg,
      prop = c(
        input$proportion_non_esg_1,
        input$proportion_non_esg_2,
        input$proportion_non_esg_3
      ) /
        100
    )
  })

  data_dtd_non_esg <- reactive({
    data_filtered() |>
      filter(symbol %in% tickers_non_esg) |>
      arrange(date) |>
      mutate(
        first_price = first(adjusted),
        .by = symbol
      ) |>
      left_join(data_tickers_non_esg(), by = "symbol") |>
      mutate(
        diff_dtd = (adjusted - first_price) / first_price,
        diff_dtd_pond = diff_dtd * prop
      )
  })

  data_portfolio_non_esg <- reactive({
    data_dtd_non_esg() |>
      summarize(
        diff_dtd_portfolio = sum(diff_dtd_pond),
        .by = date
      ) |>
      mutate(
        symbol = "PORTFOLIO",
        diff_dtd = diff_dtd_portfolio
      ) |>
      select(-diff_dtd_portfolio)
  })

  data_dtd_with_portfolio_non_esg <- reactive({
    bind_rows(
      data_dtd_non_esg() |> select(date, symbol, diff_dtd),
      data_portfolio_non_esg()
    )
  })

  stats_non_esg <- reactive({
    validate(validate_inputs()$valid, validate_inputs()$errors)

    dtd_data <- data_dtd_non_esg()

    stock_stats <- dtd_data |>
      group_by(symbol) |>
      summarize(
        total_return = last(diff_dtd) * 100,
        volatility = sd(diff(adjusted) / lag(adjusted), na.rm = TRUE) *
          sqrt(252) *
          100,
        max_drawdown = calc_max_drawdown(adjusted) * 100,
        .groups = "drop"
      ) |>
      left_join(data_tickers_non_esg(), by = "symbol") |>
      mutate(
        weighted_return = total_return * prop,
        weighted_volatility = volatility * prop,
        weighted_drawdown = max_drawdown * prop
      )

    portfolio_return <- sum(stock_stats$weighted_return)
    portfolio_volatility <- sum(stock_stats$weighted_volatility)
    portfolio_drawdown <- sum(stock_stats$weighted_drawdown)

    bind_rows(
      stock_stats |> select(symbol, total_return, volatility, max_drawdown),
      tibble(
        symbol = "PORTFOLIO",
        total_return = portfolio_return,
        volatility = portfolio_volatility,
        max_drawdown = portfolio_drawdown
      )
    ) |>
      mutate(
        total_return = round(total_return, 2),
        volatility = round(volatility, 2),
        max_drawdown = round(max_drawdown, 2)
      )
  })

  corr_non_esg <- reactive({
    validate(validate_inputs()$valid, validate_inputs()$errors)

    data_dtd_with_portfolio_non_esg() |>
      pivot_wider(names_from = symbol, values_from = diff_dtd) |>
      select(-date) |>
      correlate() |>
      mutate(across(where(is.numeric), ~ round(.x, 3)))
  })

  # === ESG PORTFOLIO ===

  data_tickers_esg <- reactive({
    tibble(
      symbol = tickers_esg,
      prop = c(
        input$proportion_esg_1,
        input$proportion_esg_2,
        input$proportion_esg_3
      ) /
        100
    )
  })

  data_dtd_esg <- reactive({
    data_filtered() |>
      filter(symbol %in% tickers_esg) |>
      arrange(date) |>
      mutate(
        first_price = first(adjusted),
        .by = symbol
      ) |>
      left_join(data_tickers_esg(), by = "symbol") |>
      mutate(
        diff_dtd = (adjusted - first_price) / first_price,
        diff_dtd_pond = diff_dtd * prop
      )
  })

  data_portfolio_esg <- reactive({
    data_dtd_esg() |>
      summarize(
        diff_dtd_portfolio = sum(diff_dtd_pond),
        .by = date
      ) |>
      mutate(
        symbol = "PORTFOLIO",
        diff_dtd = diff_dtd_portfolio
      ) |>
      select(-diff_dtd_portfolio)
  })

  data_dtd_with_portfolio_esg <- reactive({
    bind_rows(
      data_dtd_esg() |> select(date, symbol, diff_dtd),
      data_portfolio_esg()
    )
  })

  stats_esg <- reactive({
    validate(validate_inputs()$valid, validate_inputs()$errors)

    dtd_data <- data_dtd_esg()

    stock_stats <- dtd_data |>
      group_by(symbol) |>
      summarize(
        total_return = last(diff_dtd) * 100,
        volatility = sd(diff(adjusted) / lag(adjusted), na.rm = TRUE) *
          sqrt(252) *
          100,
        max_drawdown = calc_max_drawdown(adjusted) * 100,
        .groups = "drop"
      ) |>
      left_join(data_tickers_esg(), by = "symbol") |>
      mutate(
        weighted_return = total_return * prop,
        weighted_volatility = volatility * prop,
        weighted_drawdown = max_drawdown * prop
      )

    portfolio_return <- sum(stock_stats$weighted_return)
    portfolio_volatility <- sum(stock_stats$weighted_volatility)
    portfolio_drawdown <- sum(stock_stats$weighted_drawdown)

    bind_rows(
      stock_stats |> select(symbol, total_return, volatility, max_drawdown),
      tibble(
        symbol = "PORTFOLIO",
        total_return = portfolio_return,
        volatility = portfolio_volatility,
        max_drawdown = portfolio_drawdown
      )
    ) |>
      mutate(
        total_return = round(total_return, 2),
        volatility = round(volatility, 2),
        max_drawdown = round(max_drawdown, 2)
      )
  })

  corr_esg <- reactive({
    validate(validate_inputs()$valid, validate_inputs()$errors)

    data_dtd_with_portfolio_esg() |>
      pivot_wider(names_from = symbol, values_from = diff_dtd) |>
      select(-date) |>
      correlate() |>
      mutate(across(where(is.numeric), ~ round(.x, 3)))
  })

  # === PLOTS ===

  p_dtd_non_esg <- reactive({
    validate(validate_inputs()$valid, validate_inputs()$errors)

    plot_data <- data_dtd_with_portfolio_non_esg() |>
      mutate(symbol = factor(symbol, levels = c(tickers_non_esg, "PORTFOLIO")))

    ggplot(plot_data, aes(date, diff_dtd, color = symbol)) +
      geom_line(
        data = filter(plot_data, symbol != "PORTFOLIO"),
        linewidth = 0.8
      ) +
      geom_line(
        data = filter(plot_data, symbol == "PORTFOLIO"),
        linewidth = 1.5,
        linetype = "dashed"
      ) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y") +
      scale_color_viridis_d() +
      labs(
        title = "Non-ESG Portfolio: DTD Change Rate",
        color = "Symbol"
      ) +
      xlab("Date") +
      ylab("DTD Change Rate") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })

  p_dtd_esg <- reactive({
    validate(validate_inputs()$valid, validate_inputs()$errors)

    plot_data <- data_dtd_with_portfolio_esg() |>
      mutate(symbol = factor(symbol, levels = c(tickers_esg, "PORTFOLIO")))

    ggplot(plot_data, aes(date, diff_dtd, color = symbol)) +
      geom_line(
        data = filter(plot_data, symbol != "PORTFOLIO"),
        linewidth = 0.8
      ) +
      geom_line(
        data = filter(plot_data, symbol == "PORTFOLIO"),
        linewidth = 1.5,
        linetype = "dashed"
      ) +
      geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
      scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y") +
      scale_color_viridis_d() +
      labs(
        title = "ESG Portfolio: DTD Change Rate",
        color = "Symbol"
      ) +
      xlab("Date") +
      ylab("DTD Change Rate") +
      theme_bw() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
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
