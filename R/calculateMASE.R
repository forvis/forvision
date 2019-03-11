#' MASE  accuracy tables and plots
#'
#' Generates MASE accuracy tables and plots.
#'
#' @aliases calculateMASE
#' @param ts dataframe containing time series actuals needed to calculate the benchmark MAE
#' for each time series, this dataframe needs to be formatted using the Time Series Table
#' Schema (TSTS), use \code{showTSTS()} to display schema specification details.
#' @param af dataframe containing forecast data formatted using the Actual and Forecast Table
#' Schema (AFTS), use \code{showAFTS()} to display schema specification details.
#' @param sort if TRUE, sorts accuracy results according to the average MASE
#' rank across all available horizons.
#' @param digits number of digits after the dot used to output MASE; if digits=NA,
#' MASEs and ranks are not rounded.
#' @return a list with the following variables:
#' \describe{
#'   \item{accuracy:}{dataframe with accuracy table for MASE}
#'   \item{rank:}{ dataframe with MASE ranks}
#'   \item{plot:}{ggplot2 object showing accuracy vs horizon for MASE}
#' }
#' @details Calculates MASEs for each horizon and each method contained in \code{af}.
#' Outputs accuracy tables and plots.
#'
#' The input dataframe \code{af} can be created using \code{createAFTS()}.
#'
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{calculateAvgRelMAE}}, \code{\link{calculateSMAPE}}, \code{\link{calculateMAPE}}
#' @references Hyndman, R.J. and Koehler, A.B. (2006). Another look at measures
#' of forecast accuracy. \emph{International Journal of Forecasting},
#' \bold{22}(4), 679-688.
#' @keywords evaluate
#'
#' @examples
#' ts <- example1_TSTS
#' af <- createAFTS(example1_TSTS, example1_FTS)
#' acc <- calculateMASE(ts, af)
#' acc$accuracy
#' acc$rank
#' acc$plot
#'
#'
#' @export
#'
calculateMASE <- function(ts, af, sort = FALSE, digits = 3){

# Creating in-sample MAE
  naive_list <- list()
  for(i in unique(af$series_id)){
    naive_df <- dplyr::filter(ts, series_id == i)
    af_df <- dplyr::filter(af, series_id == i)
    af_df$naive_mae <-  mean(abs(diff(naive_df$value)))
    naive_list[[i]] <- af_df
  }
  af_naive = do.call(rbind, naive_list)
  rownames(af_naive) <- NULL

  # create emty frames
  out <-matrix(NA, nrow = length(unique(af_naive$method_id)), ncol = length(unique(af_naive$horizon)))
  df = data.frame(out)
  colnames(df) <- paste("h = ", 1:length(unique(af_naive$horizon)), sep ="")
  rownames(df) <- unique(af_naive$method_id)
  ranks = data.frame(out)
  colnames(ranks) <- paste("h = ", 1:length(unique(af_naive$horizon)), sep ="")
  rownames(ranks) <- unique(af_naive$method_id)

  for(j in as.vector(unique(af_naive$horizon))){
      for(i in as.vector(unique(af_naive$method_id))){
        value <- dplyr::filter(af_naive, method_id == i & horizon == j)$value
        forecast <- dplyr::filter(af_naive, method_id == i & horizon == j)$forecast
        naive_mae <- dplyr::filter(af_naive, method_id == i & horizon == j)$naive_mae
        df[i, j] <-  round(mean(abs(value - forecast)/naive_mae, na.rm=TRUE), digits)
      }
 }
  for (k in 1:length(unique(af_naive$horizon))){
    ranks[,k] <- rank(df[, k])
  }
  # Create and add averagerank column for df and ranks
  averagerank <- round(rowMeans(ranks, na.rm =TRUE), digits)
  averageMASE <- round(rowMeans(df, na.rm =TRUE), digits)
  ranks <- cbind(ranks, "average rank" = averagerank)
  df <- cbind(df, " average MASE" = averageMASE)


  # Create emty object
  methodlist <- list()
  horizonlist <- list()
  MASElist <- list()

  for(m in 1:length(unique(af_naive$method_id))){
    MASElist[[m]] <- unname(df[m, 1:length(unique(af_naive$horizon))])
    methodlist[[m]] <- rep(as.vector(unique(af_naive$method_id))[m],length(unique(af_naive$horizon)))
    horizonlist[[m]]<- as.vector(unique(af_naive$horizon))
  }
  MASE1 <- Reduce(c, MASElist)
  MASE <- Reduce(c, MASE1)
  horizon <- Reduce(c, horizonlist)
  method_id = Reduce(c, methodlist)
  df2 <- data.frame(MASE, horizon, method_id )
  # plots MAPEs frame
  gp1 <- ggplot2::ggplot(df2, ggplot2::aes(x=horizon, y=MASE, group=method_id,color=method_id, shape=method_id))+
    ggplot2::scale_shape_manual(values=1:nlevels(df2$method_id)) +
    ggplot2::labs(title = "MASE for different horizons and methods") +
    ggplot2::geom_line() +
    ggplot2::geom_point(size=3)+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))


  outlist <- list("accuracy" = df, "rank" =ranks, "plot" = gp1)
  if(sort == FALSE){
    return(outlist)
  }else{
    df <-df[order(df$` average MAPE`),]
    ranks <- ranks[order(ranks$`average rank`),]
    outlist <- list("accuracy" = df,"rank" = ranks, "plot" = gp1)
    return(outlist)
  }
}


