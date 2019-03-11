#' Forecast Summary Table
#'
#' Prints summary info for forecasts.
#'
#' @aliases printForecastSummary
#' @param fc dataframe containing forecasts formatted using the Forecast Table Schema (FTS),
#' use \code{showFTS()} to display schema specification details.
#' @details Outputs summary info containing the number of series, number of forecasts, origin range, period range, etc.
#'
#'
#' @author Cuong Sai, Andrey Davydenko, and Maxim shcherbakov.
#' @seealso \code{\link{printSeriesSummary}}
#' @examples
#' printForecastSummary(example1_FTS)
#'
#' @export
printForecastSummary <- function(fc) {

  if (validateFTS(fc)) {
    num_series <- length(unique(fc$series_id))
    time_range_min <- min(as.character(fc$timestamp))
    time_range_max <- max(as.character(fc$timestamp))
    origin_range_min <- min(as.character(fc$origin_timestamp))
    origin_range_max <- max(as.character(fc$origin_timestamp))
    num_method <- length(unique(fc$method))
    num_forecasts <- length(fc$forecast)
    horizon_low <-  min(fc$horizon)
    horizon_hi <-  max(fc$horizon)
    glue::glue('
      Forecast data summary
      ========================
      Number of time series:     {num_series}
      Timestamp range:           from {time_range_min} to {time_range_max}
      Origin range:              from {origin_range_min} to {origin_range_max}
      Number of methods:         {num_method}
      Total number of forecasts: {num_forecasts}
      Horizon range:             from {horizon_low} to {horizon_hi}
    ')
  } else {
    validateFTS(fc)
  }
}
