#' Coverage probability evaluation
#'
#' Calculates estimates of actual coverage probabilities for the given
#' nominal coverage probability.
#'
#'
#' @aliases calculateCoverage
#'
#' @param af dataframe containing forecast data formatted using the Actual and
#' Forecast Table Schema (AFTS), use \code{showAFTS()} to display schema specification details.
#' @param pi nominal coverage probability (confidence level for the prediction interval to be assessed).
#' @param conf.level confidence level for coverage probability estimates, default 0.95.
#'
#'
#' @return dataframe with coverage probability estimates for each method and each horizon
#' contained in \code{af}.
#' @seealso  \code{\link{plotCoverage}}
#' @details Calculates coverage probability estimates for each method and each horizon contained in \code{af}.
#' \emph{Coverage probability estimate} is the percentage of cases when prediction interval contained actual.
#' Confidence limits for the actual \emph{coverage probability} are obtained using binom.test function (two.sided=TRUE).
#'
#' To get a coverage probability graph, use \code{plotCoverage()}.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @keywords evaluate
#' @examples
#' af <- createAFTS(example1_TSTS, example1_FTS)
#' calculateCoverage(af, 90)
#'
#' @export

calculateCoverage <- function(af, pi, conf.level = 0.95) {
  # Check class of input
  if (!is.data.frame(af)) {
    stop("input af must be a data.frame")
  }

  # Subset data
  lo <- paste0("lo", pi)
  hi <- paste0("hi", pi)
  df <- dplyr::select(af, method_id, horizon, value, lo, hi)
  label <- (df['value'] <= df[hi] & df['value'] >= df[lo])
  colnames(label) <- NULL
  df$label <- label
  df$label <- as.vector(df$label)
  out <-matrix(NA, nrow = length(unique(df$method_id))*length(unique(df$horizon)), ncol = 2)
  df1 <- data.frame(out)
  colnames(df1) <- c("method_id", "horizon")
  meth <- list()
  for (i in 1:length(unique(df$method_id))) {
    meth[[i]] <- rep(as.character(unique(df$method_id)[i]), length(unique(df$horizon)))
  }
  meth <- do.call(c, meth)
  df1$method_id <- meth
  df1$horizon <- rep(unique(df$horizon), length(unique(df$method_id)))
  #
  n_forecasts <- c()
  n_hits <- c()
  for (i in 1:nrow(df1)){
    n_forecasts[i] <- nrow(dplyr::filter(df, method_id == df1$method_id[i] & horizon == df1$horizon[i]))
    n_hits[i] <- sum((dplyr::filter(df, method_id == df1$method_id[i] & horizon == df1$horizon[i]))$label)
  }
  df1$n_forecasts<- n_forecasts
  df1$n_hits <- n_hits

  # Create a empty vectors
  empirical_hitrate <- c()
  lo <- c()
  hi <- c()
  # calculate percentage with confidence interval
  for (i in 1:length(df1$n_forecasts)){
    empirical_hitrate[i] <-  df1$n_hits[i]/df1$n_forecasts[i]
    lo[i] <- stats::binom.test(df1$n_hits[i], df1$n_forecasts[i], conf.level = conf.level)$conf.int[1]
    hi[i] <- stats::binom.test(df1$n_hits[i], df1$n_forecasts[i], conf.level = conf.level)$conf.int[2]
  }
  # add percentage and lo, hi columns into data frame as input
  df1$empirical_hitrate <- empirical_hitrate*100
  df1$lo <- lo*100
  df1$hi <- hi*100
  # Changing column names of Hi and Lo
  colnames(df1)[6] <- paste0("lo", conf.level*100)
  colnames(df1)[7] <- paste0("hi", conf.level*100)

  return(df1)
}
