#' An example of incubation temperature time-series
#'
#' A dataset containing temperatures, time and date for a blue tit nest. This data set
#' was produced by an iButton device (Maxim) and represents raw data to start the analysis
#' of incubation behaviour. Environmental temperature has been added as average per hour.
#' 
#' @format A data frame with 954 rows and 2 variables, representing three days of nest
#' temperatures.
#' \describe{
#'   \item{DATE}{a date + time column. Both elements are concatenated in one column}
#'   \item{valueT}{temperature recordings}
#'   \item{env.temp}{environmental temperature}
#' }
"incRdataExample"

