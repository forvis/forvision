#' MAPE accuracy tables and plots
#'
#' Generates MAPE accuracy tables and plots.
#'
#' @aliases calculateMAPE
#' @param af dataframe containing forecast data formatted using the Actual and Forecast Table
#' Schema (AFTS), use \code{showAFTS()} to display schema specification details.
#' @param sort if TRUE, sorts accuracy results according to the average MAPE
#' rank across all available horizons.
#' @param digits number of digits after the dot used to output MAPE; if digits=NA,
#' MAPEs and ranks are not rounded.
#' @return a list with the following variables:
#' \describe{
#'   \item{accuracy:}{dataframe with accuracy table for MAPE}
#'   \item{rank:}{ dataframe with MAPE ranks}
#'   \item{plot:}{ggplot2 object showing accuracy vs horizon for MAPE}
#' }
#' @details Calculates MAPEs for each horizon and each method contained in af.
#' Outputs accuracy tables and plots.
#'
#' The input dataframe af can be created using \code{createAFTS()}.
#'
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{calculateAvgRelMAE}}, \code{\link{calculateSMAPE}}, \code{\link{calculateMASE}}
#' @keywords evaluate
#' @examples
#' af <- createAFTS(example1_TSTS, example1_FTS)
#' acc <- calculateMAPE(af)
#' acc$accuracy
#' acc$rank
#' acc$plot
#'
#' @export
#'
calculateMAPE <- function(af, sort = FALSE, digits = 2){
  # Error handling
  # For TSTS schema
  if (!is.data.frame(af)){
    stop("Argument af should be a data frame.")
  }

  if (sum(!is.infinite(af$value)) != length(af$value)) {
    stop("The column value has Inf or -Inf values.")
  }

  if(sum(!is.na(af$value)) != length(af$value)) {
    stop("The column value has NA values")
  }

  if (sum(af$value > 0) != length(af$value)) {
    stop("The column value has negative values or zero.")
  }

  # Create emty frames
  out <-matrix(NA, nrow = length(unique(af$method_id)), ncol = length(unique(af$horizon)))
  df = data.frame(out)
  colnames(df) <- paste("h = ", 1:length(unique(af$horizon)), sep ="")
  rownames(df) <- unique(af$method_id)
  ranks = data.frame(out)
  colnames(ranks) <- paste("h = ", 1:length(unique(af$horizon)), sep ="")
  rownames(ranks) <- unique(af$method_id)

  for(j in as.vector(unique(af$horizon))){
    for(i in as.vector(unique(af$method_id))){
      value <- dplyr::filter(af, method_id == i & horizon == j)$value
      forecast <- dplyr::filter(af, method_id == i & horizon == j)$forecast
      df[i, j] <-  round(mean(100*abs(value - forecast)/ value, na.rm=TRUE), digits)
    }
  }

  for (k in 1:length(unique(af$horizon))){
    ranks[,k] <- rank(df[, k])
  }
  # Create and add averagerank column for df and ranks
  averagerank <- round(rowMeans(ranks, na.rm =TRUE), digits)
  averageMAPE <- round(rowMeans(df, na.rm =TRUE), digits)
  ranks <- cbind(ranks, "average rank" = averagerank)
  df <- cbind(df, "mean" = averageMAPE)

  # Create emty object
  methodlist <- list()
  horizonlist <- list()
  MAPElist <- list()

  for(m in 1:length(unique(af$method_id))){
    MAPElist[[m]] <- unname(df[m, 1:length(unique(af$horizon))])
    methodlist[[m]] <- rep(as.vector(unique(af$method_id))[m],length(unique(af$horizon)))
    horizonlist[[m]]<- as.vector(unique(af$horizon))
  }
  MAPE1 <- Reduce(c, MAPElist)
  MAPE <- Reduce(c, MAPE1)
  horizon <- Reduce(c, horizonlist)
  method_id = Reduce(c, methodlist)
  df2 <- data.frame(MAPE, horizon, method_id )
  # plots MAPEs frame
  gp1 <- ggplot2::ggplot(df2, ggplot2::aes(x=horizon, y=MAPE, group=method_id,color=method_id, shape=method_id))+
    ggplot2::scale_shape_manual(values=1:nlevels(df2$method_id)) +
    ggplot2::labs(title = "MAPE for different horizons and methods") +
    ggplot2::geom_line() +
    ggplot2::geom_point(size=3)+
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

  # Create outputlist for MAPE and rank of MAPE
  outlist <- list("accuracy" = df, "rank" =ranks, "plot" = gp1)

  # using sorting if TRUE
  if(sort == FALSE){
    return(outlist)
  }else{
    df <-df[order(df$mean),]
    ranks <- ranks[order(ranks$`average rank`),]
    outlist <- list("accuracy" = df,"rank" = ranks, "plot" = gp1)
    return(outlist)
  }
}
