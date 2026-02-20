library(bslib)
library(shiny)

# Default dates
default_start <- Sys.Date() - 365
default_end <- Sys.Date()

ui <- page_fillable(
  theme = bs_theme(bootswatch = "flatly"),
  
  # === CONTROLS SECTION ===
  card(
    card_header(h2("Portfolio Comparison: ESG vs Non-ESG")),
    
    fluidRow(
      column(
        4,
        h3("Date Range"),
        dateInput("date_start", "Start Date", value = default_start, max = Sys.Date()),
        dateInput("date_end", "End Date", value = default_end, max = Sys.Date())
      ),
      column(
        4,
        h3("Non-ESG Proportions"),
        sliderInput("proportion_non_esg_1", "VTI", min = 0, max = 100, value = 34, step = 1),
        sliderInput("proportion_non_esg_2", "VT", min = 0, max = 100, value = 33, step = 1),
        sliderInput("proportion_non_esg_3", "IAUM", min = 0, max = 100, value = 33, step = 1)
      ),
      column(
        4,
        h3("ESG Proportions"),
        sliderInput("proportion_esg_1", "ESGV", min = 0, max = 100, value = 34, step = 1),
        sliderInput("proportion_esg_2", "VSGX", min = 0, max = 100, value = 33, step = 1),
        sliderInput("proportion_esg_3", "FGDL", min = 0, max = 100, value = 33, step = 1)
      )
    ),
    
    uiOutput("validation_errors_ui")
  ),
  
  # === RESULTS SECTIONS (side by side) ===
  layout_column_wrap(
    width = 1/2,
    
    # === NON-ESG SECTION ===
    card(
      card_header(h2("Non-ESG Portfolio")),
      fluidRow(
        column(6, plotOutput("p_dtd_non_esg", height = "400px")),
        column(
          6,
          h4("Statistics"),
          tableOutput("stats_non_esg"),
          br(),
          h4("Correlations"),
          tableOutput("corr_non_esg")
        )
      )
    ),
    
    # === ESG SECTION ===
    card(
      card_header(h2("ESG Portfolio")),
      fluidRow(
        column(6, plotOutput("p_dtd_esg", height = "400px")),
        column(
          6,
          h4("Statistics"),
          tableOutput("stats_esg"),
          br(),
          h4("Correlations"),
          tableOutput("corr_esg")
        )
      )
    )
  )
)
