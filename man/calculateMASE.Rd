% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateMASE.R
\name{calculateMASE}
\alias{calculateMASE}
\title{MASE  accuracy tables and plots}
\usage{
calculateMASE(ts, af, sort = FALSE, digits = 3)
}
\arguments{
\item{ts}{dataframe containing time series actuals needed to calculate the benchmark MAE
for each time series, this dataframe needs to be formatted using the Time Series Table
Schema (TSTS), use \code{showTSTS()} to display schema specification details.}

\item{af}{dataframe containing forecast data formatted using the Actual and Forecast Table
Schema (AFTS), use \code{showAFTS()} to display schema specification details.}

\item{sort}{if TRUE, sorts accuracy results according to the average MASE
rank across all available horizons.}

\item{digits}{number of digits after the dot used to output MASE; if digits=NA,
MASEs and ranks are not rounded.}
}
\value{
a list with the following variables:
\describe{
  \item{accuracy:}{dataframe with accuracy table for MASE}
  \item{rank:}{ dataframe with MASE ranks}
  \item{plot:}{ggplot2 object showing accuracy vs horizon for MASE}
}
}
\description{
Generates MASE accuracy tables and plots.
}
\details{
Calculates MASEs for each horizon and each method contained in \code{af}.
Outputs accuracy tables and plots.

The input dataframe \code{af} can be created using \code{createAFTS()}.
}
\examples{
ts <- example1_ts
af <- createAFTS(example1_ts, example1_fc)
acc <- calculateMASE(ts, af)
acc$accuracy
acc$rank
acc$plot


}
\references{
Hyndman, R.J. and Koehler, A.B. (2006). Another look at measures
of forecast accuracy. \emph{International Journal of Forecasting},
\bold{22}(4), 679-688.
}
\seealso{
\code{\link{calculateAvgRelMAE}}, \code{\link{calculateSMAPE}}, \code{\link{calculateMAPE}}
}
\author{
Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
}
\keyword{evaluate}
