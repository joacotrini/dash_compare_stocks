library(bslib)
library(bsicons)
library(shiny)

default_start <- Sys.Date() - 365
default_end <- Sys.Date()

ui <- page_sidebar(
  theme = bs_theme(bootswatch = "flatly"),
  
  title = "Portfolio Comparison: ESG vs Non-ESG",
  
  # === SIDEBAR CONTROLS ===
  sidebar = tags$div(
    h4("Configuration", class = "mb-3"),
    
    card(
      card_header("Date Range", class = "bg-primary text-white"),
      dateInput("date_start", "Start Date", value = default_start, max = Sys.Date()),
      dateInput("date_end", "End Date", value = default_end, max = Sys.Date())
    ),
    
    card(
      card_header("Non-ESG Proportions", class = "bg-primary text-white"),
      sliderInput("proportion_non_esg_1", "VTI", min = 0, max = 100, value = 50, step = 1),
      sliderInput("proportion_non_esg_2", "VT", min = 0, max = 100, value = 40, step = 1),
      textOutput("proportion_non_esg_3_display")
    ),
    
    card(
      card_header("ESG Proportions", class = "bg-success text-white"),
      sliderInput("proportion_esg_1", "ESGV", min = 0, max = 100, value = 50, step = 1),
      sliderInput("proportion_esg_2", "VSGX", min = 0, max = 100, value = 40, step = 1),
      textOutput("proportion_esg_3_display")
    ),
    
    uiOutput("validation_errors_ui")
  ),
  
  # === MAIN CONTENT ===
  accordion(
    accordion_panel(
      "Stock Overview",
      card(
        card_header("Individual Stock Metrics", class = "bg-dark text-white"),
        tableOutput("stocks_info")
      )
    ),
    
    accordion_panel(
      "Portfolio Comparison",
      card(
        full_screen = TRUE,
        min_height = "400px",
        card_header("ESG vs Non-ESG", class = "bg-dark text-white"),
        card_body(
          plotlyOutput("p_comparison", fill = TRUE)
        )
      ),
      layout_column_wrap(
        width = 1/2,
        card(
          min_height = "150px",
          card_header("Non-ESG Portfolio", class = "bg-primary text-white"),
          layout_column_wrap(
            width = 1/2,
            value_box(
              title = "Ann. Return",
              textOutput("vb_return_non_esg"),
              showcase = bs_icon("graph-up-arrow"),
              theme = "primary"
            ),
            value_box(
              title = "Ann. Volatility",
              textOutput("vb_volatility_non_esg"),
              showcase = bs_icon("bar-chart-fill"),
              theme = "primary"
            )
          )
        ),
        card(
          min_height = "150px",
          card_header("ESG Portfolio", class = "bg-success text-white"),
          layout_column_wrap(
            width = 1/2,
            value_box(
              title = "Ann. Return",
              textOutput("vb_return_esg"),
              showcase = bs_icon("graph-up-arrow"),
              theme = "success"
            ),
            value_box(
              title = "Ann. Volatility",
              textOutput("vb_volatility_esg"),
              showcase = bs_icon("bar-chart-fill"),
              theme = "success"
            )
          )
        )
      )
    ),
    
    accordion_panel(
      "Non-ESG Portfolio Details",
      card(
        full_screen = TRUE,
        min_height = "500px",
        card_header("Cumulative Return", class = "bg-primary text-white"),
        card_body(
          plotlyOutput("p_dtd_non_esg", fill = TRUE)
        ),
        card_body(
          layout_column_wrap(
            width = 1/2,
            card(h4("Statistics"), tableOutput("stats_non_esg")),
            card(h4("Correlations"), tableOutput("corr_non_esg"))
          )
        )
      )
    ),
    
    accordion_panel(
      "ESG Portfolio Details",
      card(
        full_screen = TRUE,
        min_height = "500px",
        card_header("Cumulative Return", class = "bg-success text-white"),
        card_body(
          plotlyOutput("p_dtd_esg", fill = TRUE)
        ),
        card_body(
          layout_column_wrap(
            width = 1/2,
            card(h4("Statistics"), tableOutput("stats_esg")),
            card(h4("Correlations"), tableOutput("corr_esg"))
          )
        )
      )
    )
  )
)
