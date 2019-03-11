#' Create a dataframe formatted using the Actual and Forecast Table Schema (AFTS)
#'
#' Joins a table containing time series actuals (given in the TSTS format) and a
#' table containing forecasts (given in the FTS format) to create a table containing both
#' actuals and forecasts using the Actual and Forecast Table Schema (AFTS) format.
#'
#' @aliases createAFTS
#' @param ts dataframe containing time series actuals formatted using the Time Series
#' Table Schema (TSTS), use \code{showTSTS()} to display schema specification details.
#' @param fc dataframe containing forecasts formatted using the Forecast Table Schema (FTS),
#' use \code{showFTS()} to display schema specification details.
#' @return dataframe in the AFTS format, use \code{showAFTS()} to display schema specification details.
#' @details Takes all records contained in \code{fc} and matches with values from \code{ts}.
#' If no matching value is found, the record is not included in the result.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{showTSTS}}, \code{\link{showFTS}}, \code{\link{example1_TSTS}}, \code{\link{example1_FTS}}
#' @keywords datasets
#' @examples
#' af <- createAFTS(example1_TSTS, example1_FTS)
#' head(af)
#'
#' @export

createAFTS <-function(ts, fc) {
  # Error handling
  # For TSTS schema
  if (!is.data.frame(ts)){
    stop("Argument ts should be a data frame.")
  }
  if (!sum(is.element(c("series_id", "timestamp"), colnames(ts))) == 2) {
    stop("Check the column names of input data frame ts. The input data ts needed in the form of
           a data frame containing columns named  'series_id' and 'timestamp'.")
  }

  # For FTS schema
  if (!is.data.frame(fc)){
    stop("Argument fc should be a data frame.")
  }
  if (!sum(is.element(c("series_id", "timestamp"), colnames(fc))) == 2) {
    stop("Check the column names of input data frame fc. The input data fc needed in the form of
           a data frame containing columns named  'series_id' and 'timestamp'.")
  }
  #
    df <- dplyr::inner_join(ts, fc, by = c("series_id", "timestamp"))

  return(df)
}
