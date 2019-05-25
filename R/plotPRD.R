#' Prediction-Realization Diagram
#'
#' Generates a prediction-realization diagram showing a scatterplot with forecast vs actual values.
#'
#' @aliases plotPRD
#' @param af dataframe containing forecast data formatted using the Actual and Forecast Table
#' Schema (AFTS), use \code{showAFTS()} to display schema specification details.
#' @param useLogs if TRUE, uses logarithmic scales.
#' @param xlim the minimum and maximum values for x-axis, if NULL, finds min/max values based on the data.
#' @param ylim the minimum and maximum values for y-axis, if NULL, finds min/max values based on the data.
#' @return Returns a ggplot2 object.
#' @details  Generates a \href{https://forvis.github.io/our-publications/1.pdf}{prediction-realization diagram} for the given dataset.
#' The diagram shows a scatter plot with actuals and forecasts.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{plotFixedOrigin}}, \code{\link{plotFan}}, \code{\link{plotFixedHorizon}}
#' @references Sai, C., Davydenko, A., & Shcherbakov, M. (2018, November). \href{https://forvis.github.io/our-publications/1.pdf}{Data schemas for forecasting (with examples in R).}
#' \emph{Seventh International Conference on System Modelling & Advancement on Research Trends}, 145-149. Moradabad, India.
#' @keywords explore
#' @examples
#' af <- createAFTS(example1_ts, example1_fc)
#' plotPRD(af)
#'
#' @export

plotPRD <-function(af, useLogs = FALSE, xlim=NULL, ylim=NULL){
  # Error handling
  if (!is.data.frame(af)){
    stop("Argument af should be a data frame.")
  }
  if (!sum(is.element(c("value", "forecast"), colnames(af))) == 2) {
    stop("Check the column names of input data frame. The input data needed in the form of
           a data frame containing columns named 'value'and 'forecast'.")
  }
  if (TRUE %in% is.na(af)) {
    stop(" Arguments af must not have missing values.")
  }
  if(!is.numeric(af$value)| !is.numeric(af$forecast)){
    stop("Both columns value and forecast must be numeric")
  }
  #
  df <- data.frame(x = c(0, min(af$value)), y = c(0, min(af$value)))
  if (is.element(c("method_id"), colnames(af))){
    # convert column method_id to factor
    af$method_id <- as.factor(af$method_id)
    # Plot
    gp1 <- ggplot2::ggplot()+
           ggplot2::geom_point(data = af, ggplot2::aes(x = forecast, y= value,colour = method_id,shape = method_id)) +
           ggplot2::scale_shape_manual(values=1:nlevels(af$method_id))+
           ggplot2::geom_line(data = af, ggplot2::aes(x = value, y = value, linetype = "perfect forecast")) +
           ggplot2::geom_point(size=3)+
           ggplot2::ggtitle("Prediction-Realization Diagram") +
           ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
           ggplot2::guides(linetype = ggplot2::guide_legend("")) +
           ggplot2::geom_line(data = df, aes(x = x, y=y))
    if(useLogs == TRUE) {
      gp1 <- gp1 + ggplot2::scale_x_continuous(trans=scales::log10_trans(),
                                               limits =xlim,
                                               labels = function(x) format(x, scientific = FALSE)) +
                   ggplot2::scale_y_continuous(trans=scales::log10_trans(),
                                               limits =ylim,
                                               labels = function(x) format(x, scientific = FALSE))

    } else {
      gp1 <- gp1 + ggplot2::scale_x_continuous(limits =xlim,
                                               labels = function(x) format(x, scientific = FALSE))+
                   ggplot2::scale_y_continuous(limits =ylim,
                                               labels = function(x) format(x, scientific = FALSE))
    }
  } else {
    gp1 <- ggplot2::ggplot()+
           ggplot2::geom_point(data = af, ggplot2::aes(x = log(forecast), y= log(value))) +
           ggplot2::geom_line(data = af, ggplot2::aes(x = log(value), y = log(value), linetype = "perfect forecast")) +
           ggplot2::geom_point(size=3)+
           ggplot2::ggtitle("Prediction-Realization Diagram") +
           ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
           ggplot2::guides(linetype = ggplot2::guide_legend("")) +
           ggplot2::geom_line(data = df, aes(x = x, y=y))

     if(useLogs == TRUE) {
       gp1 <- gp1+ggplot2::scale_x_continuous(trans=scales::log10_trans(),
                                              limits =xlim,
                                              labels = function(x) format(x, scientific = FALSE))+
                  ggplot2::scale_y_continuous(trans=scales::log10_trans(),
                                              limits =ylim,
                                              labels = function(x) format(x, scientific = FALSE))
     } else {
       gp1 <- gp1+ggplot2::scale_x_continuous(limits =xlim,
                                              labels = function(x) format(x, scientific = FALSE))+
                  ggplot2::scale_y_continuous(limits =ylim,
                                              labels = function(x) format(x, scientific = FALSE))
      }
    }

print(gp1)
}




