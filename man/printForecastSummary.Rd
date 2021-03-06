% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/printForecastSummary.R
\name{printForecastSummary}
\alias{printForecastSummary}
\title{Forecast Summary Table}
\usage{
printForecastSummary(fc)
}
\arguments{
\item{fc}{dataframe containing forecasts formatted using the Forecast Table Schema (FTS),
use \code{showFTS()} to display schema specification details.}
}
\description{
Prints summary info for forecasts.
}
\details{
Outputs summary info containing the number of series, number of forecasts, origin range, period range, etc.
}
\examples{
printForecastSummary(example1_fc)

}
\seealso{
\code{\link{printSeriesSummary}}
}
\author{
Cuong Sai, Andrey Davydenko, and Maxim shcherbakov.
}
