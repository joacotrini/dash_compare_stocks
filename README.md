# Portfolio Comparison Dashboard: ESG vs Non-ESG

An interactive R Shiny application for comparing the performance of Environmental, Social, and Governance (ESG) focused investment portfolios against traditional (Non-ESG) alternatives.

------------------------------------------------------------------------

## Overview

This is a **personal project** built to test investment portfolio options tailored to my specific financial situation and goals. The application allows side-by-side comparison of two three-asset portfolios, with interactive controls for adjusting asset allocation and analyzing historical performance.

> ⚠️ **Important Disclaimer**
>
> **I am not a financial expert or advisor.** This project was created for personal experimentation and self-educational purposes only. The analysis, metrics, and visualizations presented here should **NOT** be interpreted as financial advice or investment recommendations. Always consult with qualified financial professionals before making investment decisions.

------------------------------------------------------------------------

## Demonstration

![](https://media4.giphy.com/media/v1.Y2lkPTc5MGI3NjExMGxjaXFteWFncnViZzZmYjIxOW5zcXBkNnVwMXIyeGtvNmc3ODY5dSZlcD12MV9pbnRlcm5hbF9naWZfYnlfaWQmY3Q9Zw/kiFKMU4L9orfpVY9Df/giphy.gif)

------------------------------------------------------------------------

## Features

-   **Real-time Portfolio Comparison**: Visualize cumulative returns of ESG vs Non-ESG portfolios side-by-side
-   **Interactive Asset Allocation**: Adjust portfolio weights using intuitive sliders (third asset calculated automatically)
-   **Comprehensive Performance Metrics**: Annualized returns, volatility, Sharpe ratio, and maximum drawdown analysis
-   **Correlation Analysis**: Examine relationships between individual assets and portfolio performance
-   **Customizable Date Ranges**: Analyze performance over any historical period from 2020 to present
-   **Stock Overview Dashboard**: Current prices, returns, and volatility for all tracked instruments

------------------------------------------------------------------------

## Technical Approach

This project represents a learning journey with two distinct phases:

### Phase 1: Functional Prototype

I built the initial version as my first Shiny application, focusing on core functionality: data retrieval, return calculations, and basic visualizations. This established the foundational data pipeline and analytical framework.

### Phase 2: AI-Enhanced Refinement

The current version leverages AI assistance to elevate the user experience:

-   **Modern UI**: Implemented [bslib](https://rstudio.github.io/bslib/) for responsive Bootstrap theming

-   **Interactive Visualizations**: Integrated [plotly](https://plotly.com/r/) for dynamic, explorable charts

-   **Polished Design**: Applied consistent color schemes and professional layout patterns

More specifically, I used Kimi K2.5 for planning and Claude Sonnet 4.6 for coding, via Opencode.

------------------------------------------------------------------------

## Installation & Usage

### Prerequisites

-   R (version 4.0 or higher recommended)
-   R packages: `shiny`, `bslib`, `bsicons`, `tidyverse`, `tidyquant`, `plotly`, `corrr`, `lubridate`

### Running the Application

``` r
# Install required packages
install.packages(c("shiny", "bslib", "bsicons", "tidyverse", "tidyquant", "plotly", "corrr", "lubridate"))

# Clone the repository
git clone https://github.com/yourusername/dash_compare_stocks.git
cd dash_compare_stocks

# Run the app from R console
shiny::runApp()
```

The application loads historical stock data at startup (data covers 2020-01-01 to current date), using fixed symbols. This design choice supports my personal workflow of starting and stopping the application as needed, for the assets I'm evaluating.

------------------------------------------------------------------------

## Data & Methodology

### Tracked Instruments

**Non-ESG Portfolio:**

-   **VTI** - Vanguard Total Stock Market ETF (US Stocks)

-   **VT** - Vanguard Total World Stock ETF (Global Stocks)

-   **IAUM** - iShares Gold Trust Micro (Gold)

**ESG Portfolio:**

-   **ESGV** - Vanguard ESG U.S. Stock ETF (US Stocks)

-   **VSGX** - Vanguard ESG International Stock ETF (Global Stocks)

-   **FGDL** - Franklin Gold and Precious Metals ETF (Gold)

### Data Source

Historical price data retrieved from Yahoo Finance via the `tidyquant` R package. Analysis uses adjusted closing prices to account for dividends and splits.

### Calculations

-   **Daily Returns**: Daily returns calculated using `tidyquant::periodReturn`
-   **Portfolio Returns**: Value-weighted combination of individual asset returns
-   **Annualized Metrics**:
    -   Return: Geometric mean of daily returns annualized
    -   Volatility: Standard deviation of returns
    -   Sharpe Ratio: Risk-adjusted return measure
-   **Maximum Drawdown**: Largest peak-to-trough decline in portfolio value

------------------------------------------------------------------------

## Tech Stack

-   **R** - Statistical computing and data analysis
-   **Shiny** - Interactive web application framework
-   **bslib + bsicons** - Modern Bootstrap theming and iconography
-   **tidyverse** - Data manipulation and visualization grammar
-   **tidyquant** - Financial data retrieval and quantitative analysis
-   **plotly** - Interactive, publication-quality graphics
-   **corrr** - Correlation matrix analysis
-   **testthat** - Unit testing framework

------------------------------------------------------------------------

## Testing

The project includes comprehensive unit tests for core calculation functions:

``` r
# Run tests
devtools::test()

# Or
devtools::check()
```

Tests cover: ticker filtering, proportion table creation, return calculations, portfolio aggregation, performance metrics, drawdown analysis, and correlation matrices.

------------------------------------------------------------------------

## Future Roadmap

If revisiting this project, I would focus on:

1.  **Enhanced Statistical Methods**: Revise and expand the statistical measures used for portfolio evaluation, potentially incorporating additional risk metrics.
2.  **Dynamic Ticker Selection**: Add capability for users to select their own tickers rather than using the fixed six-asset framework. This would require significant rearchitecture of the data loading strategy to handle on-demand API calls and caching.

------------------------------------------------------------------------

## License

This project is open source and available under the [MIT License](LICENSE).

------------------------------------------------------------------------

*Built with R, Shiny, and AI-assisted UI enhancements.*