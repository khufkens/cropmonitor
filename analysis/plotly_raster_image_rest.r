a <- list(
  title = "",
  zeroline = FALSE,
  showline = FALSE,
  showticklabels = FALSE,
  showgrid = FALSE
)

plot_ly(
  x = c(0, 1),
  y = c(0, 1),
  type = 'scatter',
  line = list(width = 0),
  mode = 'lines'
) %>%
  layout(
    xaxis = a,
    yaxis = a,
    showlegend = FALSE,
    images = list(
      list(
        source =  "https://openclipart.org/image/800px/svg_to_png/171407/inkscaper-logo.png",
        xref = "x",
        yref = "y",
        x = 0,
        y = 1,
        sizex = 1,
        sizey = 1,
        sizing = "stretch",
        opacity = 1,
        layer = "below"
      )
    )
  )
