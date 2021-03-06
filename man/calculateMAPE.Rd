% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calculateMAPE.R
\name{calculateMAPE}
\alias{calculateMAPE}
\title{MAPE accuracy tables and plots}
\usage{
calculateMAPE(af, sort = FALSE, digits = 2)
}
\arguments{
\item{af}{dataframe containing forecast data formatted using the Actual and Forecast Table
Schema (AFTS), use \code{showAFTS()} to display schema specification details.}

\item{sort}{if TRUE, sorts accuracy results according to the average MAPE
rank across all available horizons.}

\item{digits}{number of digits after the dot used to output MAPE; if digits=NA,
MAPEs and ranks are not rounded.}
}
\value{
a list with the following variables:
\describe{
  \item{accuracy:}{dataframe with accuracy table for MAPE}
  \item{rank:}{ dataframe with MAPE ranks}
  \item{plot:}{ggplot2 object showing accuracy vs horizon for MAPE}
}
}
\description{
Generates MAPE accuracy tables and plots.
}
\details{
Calculates MAPEs for each horizon and each method contained in af.
Outputs accuracy tables and plots.

The input dataframe af can be created using \code{createAFTS()}.
}
\examples{
af <- createAFTS(example1_ts, example1_fc)
acc <- calculateMAPE(af)
acc$accuracy
acc$rank
acc$plot

}
\seealso{
\code{\link{calculateAvgRelMAE}}, \code{\link{calculateSMAPE}}, \code{\link{calculateMASE}}
}
\author{
Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
}
\keyword{evaluate}
