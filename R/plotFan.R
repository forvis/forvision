#' Fan chart
#'
#' Draws a fan chart for forecasts for a specified method, time series, and origin.
#'
#' @aliases plotFan
#' @param ts dataframe containing time series actuals formatted using the Time Series Table Schema (TSTS),
#'  use \code{showTSTS()} to display schema specification details. \code{ts} must contain \code{timestamp_dbo} column with
#'  timestamps formatted as an appropriate time-based object.
#' @param fc dataframe containing forecasts formatted using the Forecast Table Schema (FTS), use \code{showFTS()}
#' to display schema specification details. \code{fc} must contain \code{timestamp_dbo} column with timestamps formatted
#' as an appropriate time-based object.
#' @param series series_id for the time series to be shown.
#' @param origin origin_timestamp for the origin of the forecast to be shown.
#' @param method method_id for the method to be shown.
#' @param graphLib choose method for disp
#' @details Generates a fan chart joining a simple line chart for actuals (given in the TSTS format) with
#' ranges for possible future values (given in the FTS format). Uses all the prediction intervals available in fc.
#'
#' Current time-based objects supported are \code{Date}, \code{POSIXct}, \code{chron},
#' \code{yearmon}, \code{yearqtr}, and \code{timeDate}.
#' @return A dygraph object with  the fan chart.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{plotFixedHorizon}}, \code{\link{plotFixedOrigin}}
#' @keywords explore
#' @examples
#' # obtain data
#' ts <- example1_ts
#' fc <- example1_fc
#'
#' # create the "timestamp_dbo" column
#' library(zoo)
#' ts$timestamp_dbo <- as.yearmon(ts$timestamp, format = '%Y')
#' fc$timestamp_dbo <- as.yearmon(fc$timestamp, '%Y')
#'
#' # plot the fan chart for some given origin, series, and method
#' plotFan(ts, fc, "Y1", "1988", "A")
#' \dontrun{
#' # produces error because the input dataframes do not contain the "timestamp_dbo" column
#' plotFan(example1_TSTS, example1_FTS, "Y1", "1988", "A")
#' }
#'
#'
#' @export


plotFan <- function(ts, fc, series, origin, method, graphLib = "dygraphs", piColor="blue"){
  # Error handling
  # For TSTS schema
  if (!is.data.frame(ts)){
    stop("Argument ts should be a data frame.")
  }
  if (!sum(is.element(c("series_id", "timestamp_dbo", "value"), colnames(ts))) == 3) {
    stop("Check the column names of input data frame ts. The input data ts needed in the form of
         a data frame containing columns named  'series_id', value, and 'timestamp_dbo'.")
  }

  # For FTS schema
  if (!is.data.frame(fc)){
    stop("Argument fc should be a data frame.")
  }
  if (!sum(is.element(c("series_id", "timestamp", "method_id", "forecast"), colnames(fc))) == 4) {
    stop("Check the column names of input data frame fc. The input data fc needed in the form of
         a data frame containing columns named  'series_id', forecast, method_id, and 'timestamp'.")
  }
  #
  if (graphLib == "ggplot2"){

    # subset data
    fc <- dplyr::filter(fc, series_id == series & method_id == method & origin_timestamp == origin)
    ts <- dplyr::filter(ts, series_id == series)
    a <- length(ts$timestamp) - length(fc$timestamp)

    #subset lower and upper data
    lower_data <- dplyr::select(fc, dplyr::starts_with("lo"))
    upper_data <- dplyr::select(fc, dplyr::starts_with("hi"))

    # extract name of lower and upper
    lower_names <- colnames(lower_data)
    upper_names <- colnames(upper_data)
    # order by name
    lower_names<- lo_names[order(lower_names)]
    upper_names <- hi_names[order(upper_names)]

    # for PI
    level <- as.numeric(gsub("lo", "", lower_names))
    levels <- NROW(lower_names)

    interval <- list()
    for (i in 1:levels){
      interval[[i]] <- data.frame(date = as.Date(fc$timestamp_dbo),
                                  lower = unlist(fc[lower_names[i]]),
                                  upper = unlist(fc[upper_names[i]]),
                                  level = rep(level[i], length(fc[upper_names[i]])))
    }
    interval <- do.call(rbind, interval)
    rownames(interval) <- NULL
    interval$forecast <-  rep(fc$forecast, levels)
    interval <- interval[order(interval$level, decreasing = TRUE), ] # Must be ordered for gg z-index

    # prepare data
    ts <- dplyr::select(ts, timestamp_dbo, value)
    ts <- dplyr::rename(ts, date = timestamp_dbo)
    ts$date <- as.Date(ts$date)
    interval$level <- as.factor(interval$level)

    # Initialise ggplot object
    p <- ggplot2::ggplot()
    p <- p + ggplot2::geom_line(data = interval, aes(x= date, y = forecast, linetype= "forecast",
                                                     colour = "forecast"), size = 1) +
    ggplot2::geom_point(data = interval, aes(x= date, y = forecast, shape = "forecast",
                                                      colour = "forecast"), size = 2)+
    ggplot2::geom_ribbon(data = interval, aes(date, ymin = lower, ymax = upper, fill = level))

    # Create fill colour
    fill_value <- c()
    for (i in 1:levels) {
      fill_value[i] <- adjustcolor(piColor, alpha.f= 1-(level/100)[i])
    }

    p <- p +scale_fill_manual(name = "prediction intervals", values = fill_value) +
      ggplot2::geom_line(data = ts, ggplot2::aes(x=date, y=value,  linetype= "actual", colour = "actual"), size = 1) +
      ggplot2::geom_point(data = ts, ggplot2::aes(x=date, y=value, shape="actual", colour = "actual"), size = 2) +
      ggplot2::scale_colour_manual(name ="", values=c("actual" = "black", "forecast" = "red"))+
      ggplot2::scale_linetype_manual(name ="", values=c("actual" ="solid", "forecast" ="dotted")) +
      ggplot2::scale_shape_manual(name ="", values=c("actual" = 0, "forecast" = 5)) +
      ggplot2::labs(title = "series", x = "Time", y = NULL)+
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
      ggplot2::geom_vline(aes(xintercept=df$date[a]),
                 linetype = "longdash", colour="black", size = 1) +
      ggplot2::geom_text(aes(x = df$date[a-1], y = min(df$value) + 250, label = "Forecast Origin"),
                angle = 90, colour="black", size = 3.5,
                fontface = "italic")
    p
}

# dygrap
  if (graphLib == "dygraphs"){
  # dplyr::filter data
  M <- dplyr::filter(fc, series_id == series & method_id == method & origin_timestamp == origin)
  M2 <- dplyr::filter(ts, series_id == series)

  xts1 <- xts::xts(M2$value, order.by=M2$timestamp_dbo)
  names(xts1) <- "actuals"
  # dplyr::filter data
  fc_data <- dplyr::select(M, forecast)
  lo_data <- dplyr::select(M, dplyr::starts_with("lo"))
  hi_data <- dplyr::select(M, dplyr::starts_with("hi"))
  df_total <- cbind(fc_data, lo_data, hi_data)
  df_total <- df_total %>%  dplyr::mutate(fit = forecast)
  # convert to XTS object
  df_total <- xts::xts(df_total, order.by=M$timestamp_dbo)
  #dplyr::filter names
  lo_names <- colnames(lo_data)
  hi_names <- colnames(hi_data)
  # order by name
  lo_names <- lo_names[order(lo_names)]
  hi_names <- hi_names[order(hi_names)]
  #
  lab <- gsub("lo", "PI", lo_names)
  out <- cbind(xts1, df_total)
  p <- dygraphs::dygraph(out, main = series, xlab = "Time") %>%
      dygraphs::dyRangeSelector(height = 20) %>%
      dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2) %>%
      dygraphs::dyLegend(width = 800)
  p <- p %>% dygraphs::dySeries("actuals", color = "black", strokePattern = "dashed")

  # Create fill colour

  # for PI
  level <- as.numeric(gsub("lo", "", lo_names))
  levels <- NROW(lo_names)
  fill_value <- c()
  for (i in 1:levels) {
    fill_value[i] <- adjustcolor(piColor, alpha.f= 1-(level/100)[i])
  }
  shade.cols = RColorBrewer::brewer.pal(levels, "PuBuGn")
  for (i in 1:length(lo_names)) {
    p <- p %>% dygraphs::dySeries(c(lo_names[i], "fit", hi_names[i]), label = lab[i], color = fill_value[i])
  }
 a <- length(time(xts1)) - length(time(df_total))
 p <- p %>% dygraphs::dyEvent(time(xts1)[a], "Forecast origin", labelLoc = "bottom", strokePattern = "dotdash")
 p <- p %>% dygraphs::dyShading(from = time(xts1)[a], to = time(xts1)[length(time(xts1))], color = "#F5F5F5")
 p <- p %>% dygraphs::dySeries("forecast", label = method, color = "red") %>%
   dygraphs::dyLegend(show = "always") %>%
   dygraphs::dyLegend(width = 550)
  }
  return(p)
}
