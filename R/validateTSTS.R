#' Time Series Table Schema (TSTS) Validation
#'
#' Checks if the input data is correctly formatted in accordance with the Time Series Table Schema (TSTS).
#'
#' @aliases validateTSTS
#' @param ts dataframe containing actuals formatted using the Time Series Table Schema (TSTS),
#'  use \code{showTSTS()} to display schema specification details.
#' @return TRUE if the checks are passed, FALSE otherwise.
#' @details Checks that ts contains necessary column and the composite primary key values are not duplicated.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{validateFTS}}
#'
#' @examples
#' validateTSTS(example1_TSTS)
#'
#' @export


validateTSTS <- function(ts) {
  # Error handling
  if (!is.data.frame(ts)){
    stop("Argument ts should be a data frame.")
  }
  if (!sum(is.element(c("series_id", "timestamp", "value"), colnames(ts))) == 3) {
    stop("Check the column names of input data frame. The input data needed in the form of
           a data frame containing columns named 'series_id', 'timestamp', and 'value'.")
  }
  if(class(ts$value) != "numeric"){
    stop("Column value must be numeric")
  }
  # Finding duplicate records for <series_id> and <timestamp>
  ts_check <- ts[, c("series_id", "timestamp")]
  if (sum(!duplicated(ts_check)) != nrow(ts_check)) {
    stop(paste(paste("Check the input data frame. The input data frame has duplicate records for the columns series_id and timestamp.
                     Indices of duplicated rows:"), paste(rownames(ts_check[duplicated(ts_check),]), collapse = ",")))
  }
  return(TRUE)
}
