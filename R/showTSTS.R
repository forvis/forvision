#' Time Series Table Schema (TSTS) Specification
#'
#' Displays the Time Series Table Schema (TSTS) specification. The TSTS format is
#' used to store actual time series data.
#'
#' @aliases showTSTS
#' @details The \href{https://forvis.github.io/our-publications/1.pdf}{Time Series Table Schema} is designed to store time series
#' actuals across many time series. The fovision R-package uses the TSTS format in
#' its functions API to pass actuals as dataframes.
#'
#' The TSTS format makes it possible to keep actuals in external storage, to select
#' relevant data subsets and to slice-and-dice forecast data for further processing.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{showGlossary}}, \code{\link{showFTS}}
#' @references Sai, C., Davydenko, A., & Shcherbakov, M. (2018, November). \href{https://forvis.github.io/our-publications/1.pdf}{Data schemas for forecasting (with examples in R).}
#' \emph{Seventh International Conference on System Modelling & Advancement on Research Trends}, 145-149. Moradabad, India.
#'
#' @examples
#' showTSTS()
#'
#' @export


showTSTS <- function(){
  vignette("tsts", "forvision")
}
