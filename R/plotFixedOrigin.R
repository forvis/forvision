#' Fixed origin graph
#'
#' Plots the fixed origin graph showing point forecasts produced for the same time series from
#' the same origin, with different horizons. Forecasts can relate to one method or to a set of alternative methods.
#'
#' @aliases plotFixedOrigin
#' @param ts dataframe containing time series actuals formatted using the Time Series Table Schema (TSTS),
#'  use \code{showTSTS()} to display schema specification details. \code{ts} must contain \code{timestamp_dbo} column with
#'  timestamps formatted as an appropriate time-based object.
#' @param fc dataframe containing forecasts formatted using the Forecast Table Schema (FTS), use \code{showFTS()}
#' to display schema specification details. \code{fc} must contain \code{timestamp_dbo} column with timestamps formatted
#' as an appropriate time-based object. if NULL, forecasts are not shown on the graph.
#' @param series series_id for the time series to be shown.
#' @param origin origin_timestamp for the origin of the forecast to be shown.
#' @param method method_id for the method to be shown.
#' @return a dygraph object containing the fixed origin graph.
#' @details Uses the following algorithm to plot the graph:
#'
#' 1) select all the forecasts from fc having the specified origin and series_id and containing forecasts
#' produced by methods included in methods,
#'
#' 2) plot point forecasts, use different colors for different methods
#'
#' 3) plot actuals contained in ts for the given series_id.
#'
#' Current time-based objects supported are \code{Date}, \code{POSIXct}, \code{chron},
#' \code{yearmon}, \code{yearqtr}, and \code{timeDate}.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{plotFan}}, \code{\link{plotFixedHorizon}}
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
#' # plot a graph for some given origin, series, and method
#' plotFixedOrigin(ts, fc, "Y1", "1988", "A")
#' \dontrun{
#' # produces error because the input dataframes do not contain the "timestamp_dbo" column
#' plotFixedOrigin(example1_TSTS, example1_FTS, "Y1", "1988", "A")
#' }
#'
#' @export

plotFixedOrigin <- function(ts, fc = NULL, series, origin, method = NULL ) {
  # Error handling
  # For TSTS schema
  if (!is.data.frame(ts)){
    stop("Argument ts should be a data frame.")
  }
  if (!sum(is.element(c("series_id", "timestamp_dbo", "value"), colnames(ts))) == 3) {
    stop("Check the column names of input data frame ts. The input data ts needed in the form of
           a data frame containing columns named  'series_id', value, and 'timestamp_dbo'.")
  }

  if (!xts::is.timeBased(ts$timestamp_dbo)) {
    stop("The column timestamp_dbo of TSTS schema requires an appropriate time-based object")
  }

  # dplyr::filter data
  M2 <- dplyr::filter(ts, series_id == series)
  xts1 <- xts::xts(M2$value, order.by=M2$timestamp_dbo)
  names(xts1) <- "TS"

  if (is.null(fc)) {
    dygraphs::dygraph( xts1, main = paste("Graph of", series), xlab = "Time") %>%
      dygraphs::dyRangeSelector(height = 20) %>%
      dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2)
  } else {
    # For FTS schema
    if (!is.data.frame(fc)){
      stop("Argument fc should be a data frame.")
    }
    if (!sum(is.element(c("series_id", "timestamp_dbo", "method_id", "forecast"), colnames(fc))) == 4) {
      stop("Check the column names of input data frame fc. The input data ts needed in the form of
           a data frame containing columns named  'series_id', forecast, method_id, and 'timestamp_dbo'.")
    }
    if (!xts::is.timeBased(fc$timestamp_dbo)) {
      stop("The column timestamp_dbo of FTS shema requires an appropriate time-based object")
    }
    M <- dplyr::filter(fc, series_id == series & origin_timestamp == origin)

    time <- M[1:length(unique(M$horizon)),]$timestamp_dbo
    out <-matrix(NA, nrow = length(unique(M$horizon)), ncol = length(unique(M$method_id)))
    df = data.frame(out)
    colnames(df) <- unique(M$method_id)

    for(i in as.vector(unique(M$method_id))){
      df[, i] <- dplyr::filter(M, method_id == i)$forecast
    }

    df2 <- cbind(df[method[1:length(method)]])
    xts2 <- xts::xts(df2, order.by=time)
    out <- cbind(xts1, xts2)
    dygraphs::dygraph(out, main = series, xlab = "Time") %>%
      dygraphs::dyRangeSelector(height = 20) %>%
      dygraphs::dyOptions(drawPoints = TRUE, pointSize = 2) %>%
      dygraphs::dyLegend(width = 300)
    }
}

