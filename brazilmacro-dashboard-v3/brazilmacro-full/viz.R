theme_report <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(face = "bold", size = 14),
      plot.subtitle= ggplot2::element_text(size = 11),
      plot.caption = ggplot2::element_text(size = 9, hjust = 0),
      axis.title.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank()
    )
}
ts_plot <- function(df, x, y, title, subtitle = NULL, source_caption = NULL) {
  ggplot2::ggplot(df, ggplot2::aes(x = {{ x }}, y = {{ y }})) +
    ggplot2::geom_line() +
    ggplot2::labs(title = title, subtitle = subtitle, caption = source_caption, y = NULL) +
    theme_report()
}
to_plotly <- function(p) {
  plotly::ggplotly(p, dynamicTicks = TRUE) |>
    plotly::layout(legend = list(orientation = 'h', y = -0.2))
}
tbl_export <- function(df, caption = NULL, digits = 3) {
  DT::datatable(
    df, rownames = FALSE, caption = caption, class = 'compact stripe hover',
    extensions = c('Buttons'),
    options = list(pageLength = 10, scrollX = TRUE, dom = 'Bfrtip',
                   buttons = list('copy','csv','excel'))
  )
}
