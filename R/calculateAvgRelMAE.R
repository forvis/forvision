#' AvgRelMAE accuracy tables and plots
#'
#' Generates AvgRelMAE accuracy tables and plots.
#'
#' @aliases calculateAvgRelMAE
#' @param af dataframe with forecast data formatted using the Actual and Forecast
#' Table Schema (AFTS), use \code{showAFTS()} to display schema specification details.
#' @param bench method_id for the benchmark method
#' @param sort if TRUE, sorts accuracy results according to the average AvgRelMAE
#' rank across all available horizons.
#' @param digits number of digits after the dot used to output AvgRelMAE; if digits=NA,
#' AvgRelMAEs and ranks are not rounded.
#' @return a list with the following variables:
#' \describe{
#'   \item{accuracy:}{dataframe with accuracy table for AvgRelMAE}
#'   \item{mae:}{dataframe with MAEs, RelMAE, logRelMAE for each time series for each horizon}
#'   \item{rank:}{ dataframe with AvgRelMAE ranks}
#'   \item{plot:}{ggplot2 object showing accuracy vs horizon for AvgRelMAE}
#'   \item{boxplot:}{ggplot2 object with boxplots showing logRelMAEs for each method}
#' }
#' @details Calculates AvgRelMAEs (Davydenko and Fildes, 2013) for each horizon and
#' each method contained in af. Outputs accuracy tables and plots.
#'
#' The input dataframe af can be created using \code{createAFTS()}.
#'
#'
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso  \code{\link{calculateMAPE}}, \code{\link{calculateSMAPE}}, \code{\link{calculateMASE}}
#'
#' @references Davydenko, A., & Fildes, R. (2013). \href{https://forvis.github.io/our-publications/5.pdf}{Measuring Forecasting
#' Accuracy:The Case of Judgmental Adjustments to Sku-Level Demand Forecasts.}\emph{International Journal of Forecasting,} 29(3), 510-512.
#' @references Davydenko, A., & Fildes, R. (2016). \href{https://forvis.github.io/our-publications/2.pdf}{Forecast Error Measures:
#' Critical Review and Practical Recommendations.}
#' In \emph{Business Forecasting: Practical Problems and Solutions}. John Wiley & Sons Inc.
#'
#' @keywords evaluate
#' @examples
#' af <- createAFTS(example1_ts, example1_fc)
#' acc <- calculateAvgRelMAE(af, "A")
#' acc$accuracy
#' acc$rank
#' acc$plot
#'
#' @export



calculateAvgRelMAE <- function(af, bench, sort = FALSE, digits = 3) {

  af$MAE<- abs(af$value - af$forecast)

  af <- dplyr::select(af, series_id, method_id, horizon, MAE)
  check_df <- dplyr::filter(af, MAE ==0)
  id <- unique(check_df$series_id)
  if (nrow(check_df) != 0) {
    warning(paste(paste("The presence of zero MAEs in the following time series:"), paste(id, collapse = ", ")))
  }

  ae_ben <- list()
  for(k in unique(af$series_id)){
    ae_ben[[k]] <- dplyr::filter(af, series_id == k & method_id == bench)
  }
  ae_ben = do.call(rbind, ae_ben)
  rownames(ae_ben) <- NULL
  ae_ben$method_id <- NULL
  colnames(ae_ben) <- c("series_id", "horizon", "ae_ben")

  df2 <- dplyr::inner_join(af, ae_ben, by = c("series_id", "horizon"))
  df2$RelMAE <- df2$MAE/df2$ae_ben
  df2$log_RelMAE <- log(df2$RelMAE)
  df2$ae_ben <- NULL


  # Create emty frames
  out <-matrix(NA, nrow = length(unique(df2$method_id)), ncol = length(unique(df2$horizon)))
  df = data.frame(out)
  colnames(df) <- paste("h = ", 1:length(unique(df2$horizon)), sep ="")
  rownames(df) <- unique(df2$method_id)
  ranks = data.frame(out)
  colnames(ranks) <- paste("h = ", 1:length(unique(df2$horizon)), sep ="")
  rownames(ranks) <- unique(df2$method_id)

  for(j in as.vector(unique(df2$horizon))){
    for(i in as.vector(unique(df2$method_id))){
      df[i, j] <-  round(exp(mean(dplyr::filter(df2, method_id == i & horizon == j)$log_RelMAE)), digits)
    }
  }

  for (k in 1:length(unique(df2$horizon))){
    ranks[,k] <- rank(df[, k])
  }
  # Create and add averagerank column for df and ranks
  averagerank <- round(rowMeans(ranks, na.rm =TRUE), digits)
  ranks <- cbind(ranks, "average_rank" = averagerank)
  outlist <- list("AvgRelMAEs" = df, "rank" =ranks)

  # Create emty object
  methodlist <- list()
  horizonlist <- list()
  AvgRelMAEslist <- list()

  for(m in 1:length(unique(df2$method_id))){
    AvgRelMAEslist[[m]] <- unname(df[m, 1:length(unique(af$horizon))])
    methodlist[[m]] <- rep(as.vector(unique(af$method_id))[m],length(unique(af$horizon)))
    horizonlist[[m]]<- as.vector(unique(af$horizon))
  }
  AvgRelMAEs1 <- Reduce(c, AvgRelMAEslist)
  AvgRelMAEs <- Reduce(c, AvgRelMAEs1)
  horizon <- Reduce(c, horizonlist)
  method_id = Reduce(c, methodlist)
  df3 <- data.frame(AvgRelMAEs, horizon, method_id )
  # remove Inf and NaN values
  df3 <- df3[is.finite(df3$AvgRelMAEs), ]
  # plots AvgRelMAEs frame
  gp1 <- ggplot2::ggplot(df3, ggplot2::aes(x=horizon, y=AvgRelMAEs, group=method_id,color=method_id, shape=method_id))+
    ggplot2::scale_shape_manual(values=1:nlevels(df3$method_id)) +
    ggplot2::labs(title = "AvgRelMAEs for different horizons and methods") +
    ggplot2::geom_line() +
    ggplot2::geom_point(size=3)+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  # boxplot
  gp2 <- ggplot2::ggplot(df2, ggplot2::aes(stats::reorder(method_id, log_RelMAE, median), log_RelMAE, colour = method_id)) + #sorting by median
    ggplot2::geom_boxplot()+
    ggplot2::coord_flip() + # to flip the coordinate axes
    ggplot2::scale_x_discrete(name = "method_id") + # to change the axis labels,
    ggplot2::stat_boxplot(geom='errorbar') +  #whiskers
    ggplot2::stat_summary(fun.y=mean, geom="point", size=2) + #dot for the mean
    #ggplot2::theme(axis.text.x=ggplot2::element_text(angle=60, hjust=1)) +
    ggplot2::ggtitle("Distribution of logRelMAE for defferent methods") +
    ggplot2::theme(legend.position="none")+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  outlist <- list("accuracy" = df, "rank" =ranks, "mae" = df2, "plot" = gp1, "boxplot" = gp2)
  # using sorting if TRUE
  if(sort == FALSE){
    return(outlist)
  }else{
    ranks <- ranks[order(ranks$`average rank`),]
    list("accuracy" = df, "rank" =ranks, "mae" = df2, "plot" = gp1, "boxplot" = gp2)
    return(outlist)
  }
}

