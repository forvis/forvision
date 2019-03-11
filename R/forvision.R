#' \code{forvision} package
#'
#' Forecast Evaluation and Visualization
#'
#'
#' @docType package
#' @name forvision
#' @import ggplot2
#' @import dygraphs
#' @importFrom stats binom.test
#' @importFrom stats reorder
#' @importFrom stats median
#' @importFrom utils vignette
#' @importFrom magrittr %>%
#' @importFrom scales log10_trans
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("method_id", "MAE",
"forecast", "horizon", "origin_timestamp", "series_id", "value", "log_RelMAE",
"othervar"))




