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
#' ts <- example1_TSTS
#' fc <- example1_FTS
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


plotFan <- function(ts, fc, series, origin, method){
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

  # dplyr::filter data
  M <- dplyr::filter(fc, series_id == series & method_id == method & origin_timestamp == origin)
  M2 <- dplyr::filter(ts, series_id == series)

  xts1 <- xts::xts(M2$value, order.by=M2$timestamp_dbo)
  names(xts1) <- "TS"
  # dplyr::filter data
  fc_data <- dplyr::select(M, forecast)
  lo_data <- dplyr::select(M, dplyr::starts_with("lo"))
  hi_data <- dplyr::select(M, dplyr::starts_with("hi"))
  df_total <- cbind(fc_data, lo_data, hi_data)
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
  p <- dygraphs::dygraph(out, main = paste("Fanchart of", series), xlab = "Time") %>% dygraphs::dyRangeSelector(height = 20) %>%
       dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2)
  for (i in 1:length(lo_names)) {
    p <- p %>% dygraphs::dySeries(c(lo_names[i], "forecast", hi_names[i]), label = lab[i])
  }
 p
}
