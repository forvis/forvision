#' Forecast Table Schema (FTS) Validation
#'
#' Checks if the input data is correctly formatted in accordance with the Forecast Table Schema (FTS).
#'
#' @aliases validateFTS
#' @param fc dataframe containing forecasts formatted using the Forecast Table Schema(FTS),
#'  use \code{showFTS()} to display schema specification details.
#' @return TRUE if the checks are passed, FALSE otherwise.
#' @details Checks that fc contains necessary column and the composite primary key values are not duplicated.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{validateTSTS}}
#'
#' @examples
#' validateFTS(example1_fc)
#'
#' @export
#' @export

validateFTS <- function(fc) {
  # Error handling
  if (!is.data.frame(fc)){
    stop("Argument fc should be a data frame.")
  }
  if (!sum(is.element(c("series_id", "timestamp", "origin_timestamp", "horizon", "method_id"), colnames(fc))) == 5) {
    stop("Check the column names of input data frame. The input data needed in the form of
           a data frame containing columns named 'series_id', 'method_id', 'timestamp', origin_timesstamp, and 'horizon'.")
  }
  if(class(fc$horizon) != "numeric"){
    stop("Column horizon must be numeric")
  }
  # Finding duplicate records for <method>, <series_id>, <origin_timestamp>, <timestamp>, and <horizon>
  fc_check1 <- fc[, c("series_id", "method_id", "origin_timestamp", "timestamp", "horizon")]
  if (sum(!duplicated(fc_check1)) != nrow(fc_check1)) {
    stop(paste(paste("Check the input data frame. The input data frame has duplicate records for the columns series_id, origin_timestamp,
    method_id, timestamp and horizon. Indices of duplicated rows:"), paste(rownames(fc_check1[duplicated(fc_check1),]), collapse = ", ")))
  }
  # Finding duplicate records for <series_id>, "method",<origin_timestamp>, and <timestamp>
  fc_check2 <- fc[, c("series_id","method_id", "origin_timestamp", "timestamp")]
  if (sum(!duplicated(fc_check2)) != nrow(fc_check2)) {
    stop(paste(paste("Check the input data frame. The input data frame has duplicate records for the columns series_id, method_id,
    origin_timestamp, and timestamp. Indices of duplicated rows:"), paste(rownames(fc_check2[duplicated(fc_check2),]), collapse = ",")))
  }
  # Finding duplicate records for <series_id>, "method", <timestamp>, and <horizon>
  fc_check3 <- fc[, c("series_id", "method_id", "timestamp", "horizon")]
  if (sum(!duplicated(fc_check3)) != nrow(fc_check3)) {
    stop(paste(paste("Check the input data frame. The input data frame has duplicate records for the columns series_id, method_id
    timestamp and horizon. Indices of duplicated rows:"), paste(rownames(fc_check3[duplicated(fc_check3),]), collapse = ",")))
  }
  return(TRUE)
}
