build_plot <- function(returns_with_portfolio, ticker_subset, title) {
  symbols <- ticker_subset |> pull(symbol)

  plot_data <- returns_with_portfolio |>
    mutate(
      symbol = factor(symbol, levels = c(symbols, "PORTFOLIO")),
      text = paste0(
        "Date: ",
        format(date, "%b %d, %Y"),
        "\n",
        "Cumulative Return: ",
        scales::percent(cumulative_return, accuracy = 0.1),
        "\n",
        "Symbol: ",
        symbol
      )
    )

  p <- plot_data |>
    ggplot(aes(
      date,
      cumulative_return,
      color = symbol,
      text = text,
      group = 1
    )) +
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
      title = title,
      color = "Symbol"
    ) +
    xlab("Date") +
    ylab("Cumulative Return") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  ggplotly(p, tooltip = "text") |>
    config(displayModeBar = FALSE)
}
