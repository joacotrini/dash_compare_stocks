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
      sliderInput("proportion_non_esg_1", "VTI", min = 0, max = 100, value = 34, step = 1),
      sliderInput("proportion_non_esg_2", "VT", min = 0, max = 100, value = 33, step = 1),
      sliderInput("proportion_non_esg_3", "IAUM", min = 0, max = 100, value = 33, step = 1)
    ),
    
    card(
      card_header("ESG Proportions", class = "bg-success text-white"),
      sliderInput("proportion_esg_1", "ESGV", min = 0, max = 100, value = 34, step = 1),
      sliderInput("proportion_esg_2", "VSGX", min = 0, max = 100, value = 33, step = 1),
      sliderInput("proportion_esg_3", "FGDL", min = 0, max = 100, value = 33, step = 1)
    ),
    
    uiOutput("validation_errors_ui")
  ),
  
  # === MAIN CONTENT ===
  tags$div(
    
    # === SECTION 1: COMPARISON OVERVIEW ===
    card(
      full_screen = TRUE,
      min_height = "400px",
      card_header("Portfolio Comparison: ESG vs Non-ESG", class = "bg-dark text-white"),
      card_body(
        plotOutput("p_comparison", fill = TRUE)
      )
    ),
    
    br(),
    
    layout_column_wrap(
      width = 1/2,
      
      card(
        min_height = "150px",
        card_header("Non-ESG Portfolio", class = "bg-primary text-white"),
        layout_column_wrap(
          width = 1/2,
          value_box(
            title = "Total Return",
            textOutput("vb_return_non_esg"),
            showcase = bs_icon("graph-up-arrow"),
            theme = "primary"
          ),
          value_box(
            title = "Volatility (SD)",
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
            title = "Total Return",
            textOutput("vb_return_esg"),
            showcase = bs_icon("graph-up-arrow"),
            theme = "success"
          ),
          value_box(
            title = "Volatility (SD)",
            textOutput("vb_volatility_esg"),
            showcase = bs_icon("bar-chart-fill"),
            theme = "success"
          )
        )
      )
    ),
    
    br(),
    
    # === SECTION 2: NON-ESG PORTFOLIO DETAILS ===
    card(
      full_screen = TRUE,
      min_height = "500px",
      card_header("Non-ESG Portfolio: DTD Change Rate", class = "bg-primary text-white"),
      card_body(
        plotOutput("p_dtd_non_esg", fill = TRUE)
      ),
      card_body(
        layout_column_wrap(
          width = 1/2,
          card(h4("Statistics"), tableOutput("stats_non_esg")),
          card(h4("Correlations"), tableOutput("corr_non_esg"))
        )
      )
    ),
    
    br(),
    
    # === SECTION 3: ESG PORTFOLIO DETAILS ===
    card(
      full_screen = TRUE,
      min_height = "500px",
      card_header("ESG Portfolio: DTD Change Rate", class = "bg-success text-white"),
      card_body(
        plotOutput("p_dtd_esg", fill = TRUE)
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
