# ggplot2
# RColorBrewer
# scales

# function for using ggplot2 for forecast objects
plot_fan <- function(ts, fc, series, origin, method){

  n.levels <- 3
  shade.cols = RColorBrewer::brewer.pal(n.levels, "PuBuGn")
  line.cols = c("black", "darkcyan", "goldenrod1")

  fc <- dplyr::filter(fc, series_id == series & method_id == method & origin_timestamp == origin)
  ts <- dplyr::filter(ts, series_id == series)
  df <- dplyr::left_join(ts, fc, by = c("timestamp_dbo", "series_id", "timestamp"))
  df$timestamp_dbo <- as.Date(df$timestamp_dbo)
  df <- dplyr::select(df, timestamp_dbo, value, forecast,lo80, hi80,lo90,hi90)
  df <- dplyr::select(df, timestamp_dbo, value, forecast,lo80, hi80,lo90,hi90, everything())
  df <- rename(df, date = timestamp_dbo)

# plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_ribbon(data = df, aes(date, ymin = lo90, ymax = hi90, fill = "90%")) +
    ggplot2::geom_ribbon(data = df, aes(date, ymin = lo80, ymax = hi80, fill = "80%")) +
    ggplot2::geom_line(data = df, aes(date, value, color = "Training", size = 1))+
    ggplot2::geom_point(data = df, aes(date, value, color = "Training", size = 1))
    ggplot2::geom_line(data = df, aes(date, forecast, colour = "Forecast"), size = 1) +
    ggplot2::geom_point(data = df, aes(date, forecast, colour = "Forecast"), size = 1) +
    scale_colour_manual(name = "Model Data",
                        values = c("Training" = line.cols[1],
                                   "Forecast" = line.cols[3]),
                        breaks = c("Training", "Forecast")) +
    scale_fill_manual(name = "Forecast Intervals",
                      values = c( "90%" = shade.cols[1],
                                  "80%" = shade.cols[2])) +
    guides(colour = guide_legend(order = 1), fill = guide_legend(order = 2))
    # labs(title = main.title,
    #      subtitle = sub.title,
    #      caption = caption,
    #      x = x.title,
    #      y = y.title)
print(p)
}
##-------------------------------------------------------------------------------------------##
