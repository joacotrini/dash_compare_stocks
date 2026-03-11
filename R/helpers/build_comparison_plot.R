build_comparison_plot <- function(
  data_portfolio_non_esg,
  data_portfolio_esg,
  title
) {
  library(plotly)

  plot_data <- bind_rows(
    data_portfolio_non_esg |> mutate(portfolio = "Non-ESG"),
    data_portfolio_esg |> mutate(portfolio = "ESG")
  ) |>
    mutate(
      text = paste0(
        "Date: ",
        format(date, "%b %d, %Y"),
        "\n",
        "Cumulative Return: ",
        scales::percent(cumulative_return, accuracy = 0.1),
        "\n",
        "Portfolio: ",
        portfolio
      )
    )

  p <- plot_data |>
    ggplot(
      aes(
        date,
        cumulative_return,
        color = portfolio,
        linetype = portfolio,
        text = text,
        group = 1
      )
    ) +
    geom_line(linewidth = 1.2) +
    geom_hline(yintercept = 0, linetype = "dotted", color = "gray50") +
    scale_x_date(date_breaks = "3 months", date_labels = "%b-%Y") +
    scale_color_manual(values = c("Non-ESG" = "#337ab7", "ESG" = "#5cb85c")) +
    scale_linetype_manual(values = c("Non-ESG" = "solid", "ESG" = "solid")) +
    labs(
      title = title,
      color = "Portfolio",
      linetype = "Portfolio"
    ) +
    xlab("Date") +
    ylab("Cumulative Return") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggplotly(p, tooltip = "text") |>
    config(displayModeBar = FALSE)
}
