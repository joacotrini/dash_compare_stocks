library(bslib)
library(corrr)
library(lubridate)
library(tidyquant)
library(tidyverse)
library(shiny)

# Global values

## Hepler objects
date_year_start <- "2020-01-01"
date_today <- Sys.Date()
symbol = c("ESGV", "VSGX", "FGDL")

## Preprocessed data

### Get data dtd
data <- tq_get(
  x = symbol,
  get = "stock.prices",
  from = date_year_start,
  to = date_today
)

# GUI
ui <- page_fluid(
  # Titulo

  h1("The Plan (TM)"),

  br(),

  # Perillas
  h2("From date:"),

  dateInput(
    "date_start",
    "Date in which stock was bought.",
    value = paste0(year(Sys.yearmon()), "-01-01"),
    min = date_year_start,
    max = Sys.Date()
  ),

  h2("Insert proportions:"),

  br(),

  # Proporcion VTI
  sliderInput(
    "proportion_1",
    "ESGV",
    min = 0,
    max = 100,
    value = 50,
    step = 1
  ),

  # Proporcion VT
  sliderInput(
    "proportion_2",
    "VSGX",
    min = 0,
    max = 100,
    value = 25,
    step = 1
  ),

  # Proporcion IAUM
  sliderInput(
    "proportion_3",
    "FGDL",
    min = 0,
    max = 100,
    value = 25,
    step = 1
  ),

  br(),

  strong("Total:"),

  textOutput("total_rate"),

  br(),

  em(
    "ATTENTION: If the proportions do not add up to one, the results will be meaningless."
  ),

  br(),

  # Tabla riesgo
  h1("Risk table"),

  tableOutput("table_risk"),

  # Correlacion con portfolio
  h1("Correlation table"),

  tableOutput("table_corr"),

  # Plot precio absoluto
  h1("Price plot"),

  plotOutput("p_adj_price"),

  # Plot dtd chng rate w portfolio
  h1("DTD plot"),

  plotOutput("p_dtd_chng_r8_w_portfolio")
)

# Server
server <- function(input, output, session) {
  # Data input =====

  # Data de tickers
  data_tickers <- reactive({
    tmp <- tibble(
      symbol = symbol,
      prop = c(input$proportion_1, input$proportion_2, input$proportion_3) / 100
    )

    return(tmp)
  })

  # Filtered data based on date input
  data_filtered <- reactive({
    data |>
      filter(date >= input$date_start)
  })

  # Data para tasa d ganancia gral
  data_rate <- reactive({
    data_filtered() |>
      group_by(symbol) |>
      arrange(date) |>
      summarize(
        value_start = first(adjusted),
        value_end = last(adjusted),
        .groups = "drop"
      ) |>
      mutate(
        diff = (value_end - value_start) / value_start
      )
  })

  # Reactivo Data + DTD
  data_dtd <- reactive({
    tmp <- data_filtered() |>
      left_join(data_tickers()) |>
      mutate(
        diff_dtd = (adjusted - adjusted[1]) / adjusted[1],
        diff_dtd_pond = diff_dtd * prop,
        .by = symbol # coz we r calculating for each instrument separately
      )

    return(tmp)
  })

  # Reactivo DTD con portfolio
  data_portfolio <- reactive({
    tmp <- data_dtd() |>
      mutate(
        diff_dtd_portfolio = sum(diff_dtd_pond),
        .by = date
      ) |>
      filter(!duplicated(diff_dtd_portfolio)) |>
      mutate(
        symbol = "PORTFOLIO",
        open = NA,
        high = NA,
        low = NA,
        close = NA,
        volume = NA,
        adjusted = NA,
        prop = NA,
        diff_dtd = diff_dtd_portfolio,
        diff_dtd_pond = NA
      ) |>
      select(-diff_dtd_portfolio)

    return(tmp)
  })

  # Data DTD + DTD portfolio
  data_dtd_portfolio <- reactive({
    tmp <- data_dtd() |>
      bind_rows(data_portfolio())

    return(tmp)
  })

  # Cosas que no necesitan cambiar la data ====

  # Calculate rate
  calculate_rate <- reactive({
    # Add proportion data and calculate pondered differences
    tmp <- data_rate() |>
      left_join(data_tickers()) |>
      mutate(
        diff_pondered = diff * prop
      ) |>
      # Compute total change
      summarize(
        total_rate = round(sum(diff_pondered), digits = 2) * 100
      )

    # Make string
    paste0(
      "Portfolio rate of change is:",
      as.character(tmp$total_rate),
      "%"
    )
  })

  # Render output
  output$total_rate <- renderText(calculate_rate())

  # Table risk
  table_risk <- reactive({
    data_filtered() |>
      group_by(symbol) |>
      summarize(
        mean_price = mean(adjusted),
        sd_price = sd(adjusted),
        coefvar_price = sd_price / mean_price,
        mean_vol = mean(volume),
        sd_vol = sd(volume),
        coefvar_vol = sd_vol / mean_vol,
        dtd_rate = (last(adjusted) - first(adjusted)) / first(adjusted) * 100
      )
  })

  # Render
  output$table_risk <- renderTable(table_risk())

  # Plot precio absoluto
  p_adj_price <- data |>
    ggplot(aes(date, adjusted, color = symbol)) +
    geom_line() +
    scale_x_date(
      date_breaks = "6 months",
      date_labels = "%b-%Y"
    ) +
    scale_color_viridis_d() +
    labs(
      title = "ETF Adjusted Price, by date.",
      color = "Symbol"
    ) +
    xlab("Date") +
    ylab("Adjusted Price") +
    theme_bw()

  # Render
  output$p_adj_price <- renderPlot(p_adj_price)

  # Cosas q necesitan cambiar la data ====

  # Plot DTD con portfolio
  p_dtd_chng_r8_w_portfolio <- reactive({
    tmp <- data_dtd_portfolio() |>
      ggplot(aes(date, diff_dtd, color = symbol)) +
      geom_line() +
      geom_hline(yintercept = 0) +
      scale_x_date(
        date_breaks = "6 months",
        date_labels = "%b-%Y"
      ) +
      scale_color_viridis_d() +
      labs(
        title = "ETF and Portfolio DTD Change Rate, by date.",
        color = "Symbol"
      ) +
      xlab("Date") +
      ylab("DTD Change Rate") +
      theme_bw()

    return(tmp)
  })

  # Render
  output$p_dtd_chng_r8_w_portfolio <- renderPlot(p_dtd_chng_r8_w_portfolio())

  # Correlacion con portfolio
  table_corr <- reactive({
    tmp <- data_dtd_portfolio() |>
      select(date, symbol, diff_dtd) |>
      pivot_wider(
        names_from = symbol,
        values_from = diff_dtd
      ) |>
      select(-date) |>
      correlate()

    return(tmp)
  })

  # Render
  output$table_corr <- renderTable(table_corr())
}

# Run app
shinyApp(ui, server)
