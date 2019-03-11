#' Forecast Table Schema (FTS) Specification
#'
#' Displays the Forecast Table Schema (FTS) specification. The FTS format is
#' used to store forecasting results.
#'
#' @aliases showFTS
#' @details The \href{https://forvis.github.io/our-publications/1.pdf}{Forecast Table Schema} is designed to store forecasting results
#' across many time series. The fovision R-package uses the FTS format in
#' its functions API to pass forecasts as dataframes.
#'
#' The FTS format makes it possible to keep forecasts in external storage, to select
#' relevant data subsets and to slice-and-dice forecast data for further processing.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{showGlossary}}, \code{\link{showTSTS}}
#' @references Sai, C., Davydenko, A., & Shcherbakov, M. (2018, November). \href{https://forvis.github.io/our-publications/1.pdf}{Data schemas for forecasting (with examples in R).}
#' \emph{Seventh International Conference on System Modelling & Advancement on Research Trends}, 145-149. Moradabad, India.
#'
#' @examples
#' showFTS()
#'
#' @export


showFTS <- function(){
  vignette("fts", "forvision")
}
