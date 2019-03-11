#' Coverage Probability Plot
#'
#' Generates a coverage probability plot showing coverage probability estimates,
#' corresponding error bands, and nominal coverage probability.
#'
#'
#' @aliases plotCoverage
#'
#' @param af dataframe containing forecast data formatted using the Actual and
#' Forecast Table Schema (AFTS), use \code{showAFTS()} to display schema specification details.
#' @param pi nominal coverage probability (confidence level for the prediction interval to be assessed).
#' @param methods array containing method_id values for methods to be shown on the graph,
#' if NULL, all available methods will be shown.
#' @param horizons array containing horizons to be shown on the graph,
#' if NULL, all available horizons will be shown.
#' @param conf.level confidence level for coverage probability estimates, default 0.95.
#' @param coord.flip use FALSE for horizontal bar chart, TRUE for vertical.
#' @return Returns a ggplot2 object containing the coverage probability plot.
#' @details Generates a bar chart showing coverage percentages for the given nominal coverage
#' (Sai, Davydenko, and Shcherbakov, 2018). The results are confined to methods contained in methods
#' and horizons contained in horizons. Adds error bars and nominal coverage line to chart.
#'
#'
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @references Sai, C., Davydenko, A., & Shcherbakov, M. (2018, November). \href{https://forvis.github.io/our-publications/1.pdf}{Data schemas for forecasting (with examples in R).}
#' \emph{Seventh International Conference on System Modelling & Advancement on Research Trends}, 145-149. Moradabad, India.
#' @keywords evaluate
#' @examples
#' af <- createAFTS(example1_TSTS, example1_FTS)
#' plotCoverage(af, pi = 90, methods = c("A", "B"))
#' plotCoverage(af, pi = 90, horizons = 1:2)
#'
#' @export

plotCoverage <- function(af, pi, methods = NULL,
                         horizons = NULL,  conf.level = 0.95,
                         coord.flip = FALSE) {

  # Check class of input
  if (!is.data.frame(af)) {
    stop("input af must be a data.frame")
  }

  if (!is.null(methods)) {
    af <- dplyr::filter(af, method_id == methods)
  }
  if (!is.null(horizons)) {
    af <- dplyr::filter(af, horizon == horizons)
  }
  # dplyr::filter data
  lo <- paste0("lo", pi)
  hi <- paste0("hi", pi)
  df <- dplyr::select(af, method_id, horizon, value, lo, hi)
  label <- (df['value'] <= df[hi] & df['value'] >= df[lo])
  colnames(label) <- NULL
  df$label <- label
  df$label <- as.vector(df$label)
  out <-matrix(NA, nrow = length(unique(df$method_id))*length(unique(df$horizon)), ncol = 4)
  df1 <- data.frame(out)
  colnames(df1) <- c("method_id", "horizon", "n_forecasts", "n_hits")
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
    lo[i] <- stats::binom.test(df1$n_hits[i], df1$n_forecasts[i],
                                      conf.level = conf.level)$conf.int[1]
    hi[i] <- stats::binom.test(df1$n_hits[i], df1$n_forecasts[i],
                                      conf.level = conf.level)$conf.int[2]
  }
  # add percentage and lo, hi columns into data frame as input
  df1$empirical_hitrate <- empirical_hitrate*100
  df1$lo <- lo*100
  df1$hi <- hi*100
  # Changing column names of Hi and Lo
  colnames(df1)[6] <- paste0("lo", conf.level*100)
  colnames(df1)[7] <- paste0("hi", conf.level*100)
  # Creae new daa frame for fill
  method_id <- c()
  for (i in 1:nrow(df1)) {
    method_id[i] <- paste0(df1$method_id[i],", horizon=", df1$horizon[i])
  }
  df2 <- df1
  df2$method_id <- method_id
  df2$horizon <- NULL

  # # sorting x by percentage for plotting
  # if (sort == TRUE) {
  #   df2$method_id <-  factor(df2$method_id, levels = unique(df2$method_id)[order(df2$empirical_hitrate)])
  # }


  # Create a bar plot with difference options
  dodge <- ggplot2::position_dodge(width = 0.9)
  limits <- ggplot2::aes(ymax =  df2[,6],
                         ymin = df2[, 5])


  p <- ggplot2::ggplot(df2, ggplot2::aes(x=df2$method_id, y=df2$empirical_hitrate, fill = df2$method_id))
  p <- p + ggplot2::geom_bar(stat="identity")
  p <- p + ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1))
  p <- p + ggplot2::geom_errorbar(limits, position = dodge, width = 0.25)
  p <- p + ggplot2::theme(legend.position="none")
  p <- p + ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))
  p <- p + ggplot2::ggtitle("Coverage")
  p <- p + ggplot2::labs(x = NULL, y = NULL) +
    ggplot2::geom_hline(yintercept=pi, linetype="dashed",
                        color = "red", size=1)

  if( coord.flip == TRUE){
    p <- p + ggplot2::coord_flip()
  }


  # if (sort == TRUE){
  #   df1 <- df1[order(df1$empirical_hitrate),]
  # }

  print(p)
  #return(df1)
}


