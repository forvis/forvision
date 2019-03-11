#' Time Series Summary Table
#'
#' Prints summary info for actuals.
#'
#' @aliases printSeriesSummary
#' @param ts dataframe containing time series actuals formatted using the Times Series Table Schema (TSTS),
#' use \code{showTSTS()} to display schema specification details.
#' @details Outputs summary info containing the number of series, number of observations, period range.
#'
#'
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{printForecastSummary}}
#' @examples
#' printSeriesSummary(example1_TSTS)
#'
#' @export

printSeriesSummary <- function(ts) {

  if (validateTSTS(ts)) {
    num_series <- length(unique(ts$series_id))
    tutol_num_ob <- nrow(ts)
    time_range_min <- min(as.character(ts$timestamp))
    time_range_max <- max(as.character(ts$timestamp))

    glue::glue('
      Time series data summary
      ========================
      Number of time series:        {num_series}
      Total number of observations: {tutol_num_ob}
      Timestamp range:              from {time_range_min} to {time_range_max}
    ')
  } else {
    validateTSTS(ts)
  }
}
