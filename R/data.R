#' Example Dataset Formatted Using Time Series Table Schema (TSTS)
#'
#' Example dataset containing time series actuals relating to different series. The dataset contains artificial
#' yearly observations created to illustrate the capabilities of the package.
#'
#' @format \code{example1_ts} is a data frame formatted using the Time Series
#' Table Schema, use \code{showTSTS()} to display schema specification details.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{m3_yearly_ts}}, \code{\link{m3_quarterly_ts}}, \code{\link{m3_monthly_ts}},
#' \code{\link{m3_other_ts}}
#' @source
#' \url{https://github.com/forvis/forvision_data/}.
#'
#' @keywords datasets
#' @examples
#'
#' head(example1_ts, 10)
#'
#'
"example1_ts"



#' Example Dataset Formatted Using Forecast Table Schema (FTS)
#'
#' Example dataset containing forecast data to different series. The dataset contains forecast data
#'  created to illustrate the capabilities of the package.
#'
#'
#' @format \code{example1_fc} is a data frame formatted using the Forecast
#' Table Schema, use \code{showFTS()} to display schema specification details.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{m3_yearly_fc}}, \code{\link{m3_yearly_fc_pis}}, \code{\link{m3_quarterly_fc}},
#' \code{\link{m3_quarterly_fc_pis}}, \code{\link{m3_monthly_fc}}, \code{\link{m3_monthly_fc_pis}},
#' \code{\link{m3_other_fc}}, \code{\link{m3_other_fc_pis}}
#' @source
#' \url{https://github.com/forvis/forvision_data/}.
#'
#' @keywords datasets
#' @examples
#' head(example1_fc, 10)
#'
#'
#'
"example1_fc"


#' Yearly actuals from the M3-competition (TSTS)
#'
#' The dataset contains yearly time series actuals relating to different series from the M3-competition.
#'
#' @format \code{m3_yearly_ts} is a data frame formatted using the Time Series
#' Table Schema, use \code{showTSTS()} to display schema specification details.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{example1_ts}}, \code{\link{m3_quarterly_ts}}, \code{\link{m3_monthly_ts}},
#' \code{\link{m3_other_ts}}
#' @source
#' \url{https://github.com/forvis/forvision_data/}.
#'
#' @keywords datasets
#' @examples
#'
#' head(m3_yearly_ts, 10)
#'
#'
"m3_yearly_ts"


#' Yearly forecasts from the M3-competition (FTS)
#'
#' The dataset contains forecast data to different yearly time series from the M3-competition.
#'
#' @format \code{m3_yearly_fc} is a data frame formatted using the Forecast
#' Table Schema, use \code{showFTS()} to display schema specification details.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{example1_fc}}, \code{\link{m3_yearly_fc_pis}}, \code{\link{m3_quarterly_fc}},
#' \code{\link{m3_quarterly_fc_pis}}, \code{\link{m3_monthly_fc}}, \code{\link{m3_monthly_fc_pis}},
#' \code{\link{m3_other_fc}}, \code{\link{m3_other_fc_pis}}
#' @source
#' \url{https://github.com/forvis/forvision_data/}.
#'
#' @keywords datasets
#' @examples
#'
#' head(m3_yearly_fc, 10)
#'
#'
"m3_yearly_fc"

#' Yearly forecasts containing prediction intervals calculated for the M3-competition data (FTS)
#'
#' The dataset contains forecast data containing prediction intervals calculated for different
#' yearly time series from the M3-competition.
#'
#' @format \code{m3_yearly_fc_pis} is a data frame formatted using the Forecast
#' Table Schema, use \code{showFTS()} to display schema specification details.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{example1_fc}}
#' @source
#' \url{https://github.com/forvis/forvision_data/}.
#'
#' @keywords datasets
#' @examples
#'
#' head(m3_yearly_fc_pis, 10)
#'
#'
"m3_yearly_fc_pis"


#' Quarterly actuals from the M3-competition (TSTS)
#'
#' The dataset contains quarterly time series actuals relating to different series from the M3-competition.
#'
#' @format \code{m3_quarterly_ts} is a data frame formatted using the Time Series
#' Table Schema, use \code{showTSTS()} to display schema specification details.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{example1_fc}}
#' @source
#' \url{https://github.com/forvis/forvision_data/}.
#'
#' @keywords datasets
#' @examples
#'
#' head(m3_quarterly_ts, 10)
#'
#'
"m3_quarterly_ts"


#' Quarterly forecasts from the M3-competition (FTS)
#'
#' The dataset contains forecast data to different quarterly time series from the M3-competition.
#'
#' @format \code{m3_quarterly_fc} is a data frame formatted using the Forecast
#' Table Schema, use \code{showFTS()} to display schema specification details.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{example1_fc}}
#' @source
#' \url{https://github.com/forvis/forvision_data/}.
#'
#' @keywords datasets
#' @examples
#'
#' head(m3_quarterly_fc, 10)
#'
#'
"m3_quarterly_fc"

#' Quarterly forecasts containing prediction intervals calculated for the M3-competition data (FTS)
#'
#' The dataset contains forecast data containing prediction intervals calculated for different
#' quarterly time series from the M3-competition.
#'
#' @format \code{m3_quarterly_fc_pis} is a data frame formatted using the Forecast
#' Table Schema, use \code{showFTS()} to display schema specification details.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{example1_fc}}
#' @source
#' \url{https://github.com/forvis/forvision_data/}.
#'
#' @keywords datasets
#' @examples
#'
#' head(m3_quarterly_fc_pis, 10)
#'
#'
"m3_quarterly_fc_pis"


#' Monthly actuals from the M3-competition (TSTS)
#'
#' The dataset contains monthly time series actuals relating to different series from the M3-competition.
#'
#' @format \code{m3_monthly_ts} is a data frame formatted using the Time Series
#' Table Schema, use \code{showTSTS()} to display schema specification details.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{example1_ts}}
#' @source
#' \url{https://github.com/forvis/forvision_data/}.
#'
#' @keywords datasets
#' @examples
#'
#' head(m3_monthly_ts, 10)
#'
#'
"m3_monthly_ts"


#' Monthly forecasts from the M3-competition (FTS)
#'
#' The dataset contains forecast data to different monthly time series from the M3-competition.
#'
#' @format \code{m3_monthly_fc} is a data frame formatted using the Forecast
#' Table Schema, use \code{showFTS()} to display schema specification details.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{example1_fc}}
#' @source
#' \url{https://github.com/forvis/forvision_data/}.
#'
#' @keywords datasets
#' @examples
#'
#' head(m3_monthly_fc, 10)
#'
#'
"m3_monthly_fc"

#' Monthly forecasts containing prediction intervals calculated for the M3-competition data (FTS)
#'
#' The dataset contains forecast data containing prediction intervals calculated for different
#' monthly time series from the M3-competition.
#'
#' @format \code{m3_monthly_fc_pis} is a data frame formatted using the Forecast
#' Table Schema, use \code{showFTS()} to display schema specification details.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{example1_fc}}
#' @source
#' \url{https://github.com/forvis/forvision_data/}.
#'
#' @keywords datasets
#' @examples
#'
#' head(m3_monthly_fc_pis, 10)
#'
#'
"m3_monthly_fc_pis"


#' Other actuals from the M3-competition (TSTS)
#'
#' The dataset contains other time series actuals relating to different series from the M3-competition.
#'
#' @format \code{m3_other_ts} is a data frame formatted using the Time Series
#' Table Schema, use \code{showTSTS()} to display schema specification details.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{example1_ts}}
#' @source
#' \url{https://github.com/forvis/forvision_data/}.
#'
#' @keywords datasets
#' @examples
#'
#' head(m3_other_ts, 10)
#'
#'
"m3_other_ts"


#' Other forecasts from the M3-competition (FTS)
#'
#' The dataset contains forecast data to different other time series from the M3-competition.
#'
#' @format \code{m3_other_fc} is a data frame formatted using the Forecast
#' Table Schema, use \code{showFTS()} to display schema specification details.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{example1_fc}}
#' @source
#' \url{https://github.com/forvis/forvision_data/}.
#'
#' @keywords datasets
#' @examples
#'
#' head(m3_other_fc, 10)
#'
#'
"m3_other_fc"

#' Other forecasts containing prediction intervals calculated for the M3-competition data (FTS)
#'
#' The dataset contains forecast data containing prediction intervals calculated for different
#' other time series from the M3-competition.
#'
#' @format \code{m3_other_fc_pis} is a data frame formatted using the Forecast
#' Table Schema, use \code{showFTS()} to display schema specification details.
#' @author Cuong Sai, Andrey Davydenko, and Maxim Shcherbakov.
#' @seealso \code{\link{example1_fc}}
#' @source
#' \url{https://github.com/forvis/forvision_data/}.
#'
#' @keywords datasets
#' @examples
#'
#' head(m3_other_fc_pis, 10)
#'
#'
"m3_other_fc_pis"

