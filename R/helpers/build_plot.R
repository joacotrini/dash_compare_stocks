build_plot <- function(dtd_with_portfolio, ticker_subset, title) {
  symbols <- ticker_subset |> pull(symbol)
  
  plot_data <- dtd_with_portfolio |>
    mutate(symbol = factor(symbol, levels = c(symbols, "PORTFOLIO")))
  
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
      title = title,
      color = "Symbol"
    ) +
    xlab("Date") +
    ylab("DTD Change Rate") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}
